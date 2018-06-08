module Deprecations
    using CSTParser
    using CSTParser: EXPR, MacroCall, IDENTIFIER, LITERAL
    using AbstractTrees
    using Tokenize: Tokens

    export edit_text, edit_file, edit_markdown

    struct Deprecation
        description::String
        package::String
        minsupported::Base.VersionNumber
        minrequired::Base.VersionNumber
        # Don't apply this deprecation if all versions are above
        # this version - generally because this syntax is now available
        # for re-use, and we want to avoid false positives for such syntax
        maxver::Base.VersionNumber
    end

    include("CSTUtils/CSTUtils.jl")
    using .CSTUtils
    import .CSTUtils: span_text, fullspan_text, leading_ws, trailing_ws
    include("CSTAnalyzer/CSTAnalyzer.jl")
    using .CSTAnalyzer
    using .CSTAnalyzer: State, FileSystem, Scope, Location, File
    include("formatting.jl")
    include("astmatching.jl")
    include("resolutions.jl")

    all_templates = Any[]
    all_replacements = Any[]
    deprecation = Any[]

    function applicable_dep(dep, vers)
        i = info(dep)
        haskey(vers, i.package) || return false
        pvers = vers[i.package]
        all(interval->i.minsupported <= interval.lower, pvers.intervals) || return false
        all(interval->i.maxver <= interval.lower, pvers.intervals) && return false
        true
    end

    all_deprecations = Dict()
    templates = Dict{Any,Vector{Tuple{Any, Any, Any, Any}}}()
    custom_resolutions = Dict{Any, Any}()
    register(dep, info) = all_deprecations[dep] = info
    info(dep) = all_deprecations[dep]

    dep_for_vers(dep, vers) = dep()

    function applicable_deprecations(vers)
        deps = Any[]
        for dep in keys(all_deprecations)
            applicable_dep(dep, vers) || continue
            push!(deps, dep_for_vers(dep, vers))
        end
        deps
    end

    identity_formatter(orig_tree, tree, matches) = tree

    function match(dep, template::String, replacement::String, formatter = identity_formatter; filter = (args...)->return true)
        haskey(templates, dep) || (templates[dep] = Vector{Tuple{Any, Any, Any}}())
        push!(templates[dep], (template, replacement, formatter, filter))
    end

    function match_macrocall(mod, name, dep, template::String, replacement::String)
        # TODO: Make sure the macro in the current module is actually the one from `mod`
        haskey(templates, dep) || (templates[dep] = Vector{Tuple{Any, Any, Any}}())
        push!(templates[dep], (template, replacement, identity_formatter))
    end

    function match(f::Function, dep, expr_kind)
        haskey(custom_resolutions, dep) || (custom_resolutions[dep] = Vector{Any}())
        push!(custom_resolutions[dep], (expr_kind, f))
    end

    include("database.jl")

    struct Context
        in_macrocall::Bool
        top_macrocall
    end

    # By default, don't apply rewrites in macro context
    applies_in_macrocall(dep, context) = false

    struct TextReplacement
        dep::Union{Deprecation, Nothing}
        range::UnitRange{Int}
        text::String
    end
    function TextReplacement(dep, range::UnitRange{Int}, text::String)
        dep = all_deprecations[typeof(dep)]
        TextReplacement(dep, range, text)
    end

    function overlay_parse(text, cont = true)
        p = CSTParser.parse(text, cont)
        OverlayNode(p, text)
    end

    function changed_text(text, resolutions)
        sort!(resolutions, by=x->first(x.range))
        buf = IOBuffer()
        lastoffset = 1
        for r in resolutions
            if lastoffset > first(r.range)
                # Overlapping replacements. Only apply the first one for now.
                # We'll re-run this to convergence
                continue
            end
            write(buf, text[(lastoffset:first(r.range)-1)])
            write(buf, r.text)
            lastoffset = last(r.range)+1
        end
        write(buf, text[lastoffset:end])
        (length(resolutions) != 0, String(take!(buf)))
    end

    function text_replacements(text, deps; analysis = nothing)
        match = overlay_parse(text)
        # Re-construct analysis by assuming top-level
        if analysis === nothing
            S = State{FileSystem}(Scope(), Location("top", 0), "", [], [], 0:0, false, Dict(), FileSystem())
            Scop = S.current_scope
            CSTAnalyzer.trav(CSTAnalyzer.DefaultWalker(), match, Scop, S)
        else
            (S, Scop) = analysis
        end
        # CST Matchers
        replacements = Any[]
        customs = Any[]
        for dep in deps
            haskey(templates, typeof(dep)) && append!(replacements, collect(Iterators.product((dep,), templates[typeof(dep)])))
            haskey(custom_resolutions, typeof(dep)) && append!(customs, collect(Iterators.product((dep,), custom_resolutions[typeof(dep)])))
        end
        parsed_replacementes = map(x->(x[1],(overlay_parse(x[2][1],false),overlay_parse(x[2][2],false),x[2][3:end]...)), replacements)
        function find_replacements(x, results, context=Context(false, nothing))
            for (i,(dep, (t, r, formatter, filter))) in enumerate(parsed_replacementes)
                if matches_template2(x, t)
                    (!context.in_macrocall || applies_in_macrocall(dep, context)) || continue
                    result = Dict{Any,Any}()
                    match_parameters(t, x, result)[1] || continue
                    filter((S, Scop), dep, x, result) || continue
                    rtree = reassemble_tree(r, result)
                    buf = IOBuffer()
                    print_replacement(buf, formatter(x, rtree, result))
                    push!(results, TextReplacement(dep, x.span, String(take!(buf))))
                end
            end
            for (i,(dep, (k, f))) in enumerate(customs)
                if isexpr(x, k)
                    f((dep, x, results, context, (S, Scop)))
                end
            end
            if isexpr(x, MacroCall) && !context.in_macrocall
                # Don't consider doc macros here. We know they don't
                # affect the meaning of the parsed code.
                if !isexpr(children(x)[1], CSTParser.GlobalRefDoc)
                    macroname = children(children(x)[1])[2]
                    if !(isexpr(macroname, IDENTIFIER) && Expr(macroname) in (
                           Symbol("eval"), Symbol("inline"), Symbol("views"), Symbol("test")
                            ))
                        context = Context(true, x)
                    end
                end
                # Recurse into code examples in documentation
                if isexpr(children(x)[1], CSTParser.GlobalRefDoc)
                    docnode = children(x)[2]
                    doc = span_text(docnode)
                    nquotes = doc[1:3] == "\"\"\"" ? 3 : 1
                    doc = doc[(1+nquotes):(end-nquotes)]
                    if !isa(doc, Expr)
                        changed, new_doc = edit_markdown(doc)
                        if changed
                            # TODO: More fine grained deprecation messages inside
                            # doc strings
                            push!(results, TextReplacement(nothing, (first(docnode.span)+nquotes):(last(docnode.span)-nquotes), new_doc))
                        end
                    end
                end
            end
            for arg in children(x)
                find_replacements(arg, results, context)
            end
        end
        results = TextReplacement[]
        find_replacements(match, results)
        results
    end

    function edit_text(text, deps = map(x->x(), keys(all_deprecations)); kwargs...)
        changed_text(text, text_replacements(text, deps; kwargs...))
    end

    function edit_file(fname, deps = map(x->x(), keys(all_deprecations)), edit=edit_text; kwargs...)
        text = readstring(fname)
        any_changed, new_text = edit(text, deps; kwargs...)
        if any_changed
            open(fname, "w") do io
                write(io, new_text)
            end
        end
        any_changed
    end

    function edit_markdown(text, deps = map(x->x(), keys(all_deprecations)); kwargs...)
        content = Base.Markdown.parse(text)
        changed_any = false
        new_text = text
        map(content.content) do x
            isa(x, Base.Markdown.Code) || return
            if !(x.language =="julia" || ismatch(r"jldoctest[ ]?(.*)$", x.language))
                return
            end
            any_matches = false
            new_code = x.code
            for m in eachmatch(r"^julia> "m, x.code)
                any_matches = true
                curidx = startidx = m.offset
                while (curidx = search(x.code, '\n', curidx)) != 0
                    # In order to be a line continuation, needs to be at least
                    # 7 spaces after a newline.
                    is_continuation = true
                    idx = curidx
                    i = 1
                    while i <= 7
                        idx = nextind(x.code, idx)
                        idx > endof(x.code) && break
                        if !isspace(x.code[idx])
                            is_continuation = false
                            break
                        end
                        i += 1
                    end
                    is_continuation || break
                    curidx = nextind(x.code, curidx)
                end
                curidx == 0 && (curidx = endof(x.code))
                text = x.code[(startidx+7):curidx]
                try
                    new_code = replace(new_code, text, edit_text(text, deps)[2])
                end
            end
            if !any_matches
                # Parse the whole thing
                try
                    new_code = edit_text(new_code, deps)[2]
                end
            end
            if x.code != new_code
                changed_any = true
                new_text = replace(new_text, x.code, new_code)
            end
        end
        return (changed_any, new_text)
    end

    struct IncludeWalker <: CSTAnalyzer.Walker
        include_map::Vector{Pair{Scope, String}}
    end
    IncludeWalker() = IncludeWalker(Vector{Pair{Scope, String}}())

    function CSTAnalyzer.lint_call(w::IncludeWalker, x::CSTParser.EXPR{CSTParser.Call}, s, S)
        fname = CSTParser.get_name(x)
        if CSTParser.str_value(fname) == "include"
            path = CSTAnalyzer.get_path(x)
            isempty(path) && return
            path = isabspath(path) ? path : joinpath(dirname(s.loc.path), path)
            push!(w.include_map, s=>path)
        end
    end

    function scope_include!(parent, scope)
        push!(parent.children, scope)
        scope.parent = parent
        merge!(parent.names, scope.names)
        empty!(scope.names)
    end

    # Given a set of files in a package, process all of them
    # using CSTAnalyzer
    function process_all(files)
        parsed = [ file=>overlay_parse(readstring(file)) for file in files ]
        S = State{FileSystem}(Scope(), Location("top", 0), "", [], [], 0:0, false, Dict(), FileSystem())
        walker = IncludeWalker()
        file_scopes = Dict(file => Scope() for file in files)
        for (file, p) in parsed
            s = file_scopes[file]
            s.loc = Location(file, 0)
            S.current_scope = s
            CSTAnalyzer.trav(walker, p, s, S)
        end
        for (scope, incl) in walker.include_map
            scope_include!(scope, file_scopes[incl])
        end
        S, file_scopes
    end

end # module
