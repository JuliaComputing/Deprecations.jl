module Deprecations
    using CSTParser
    using CSTParser: EXPR, MacroCall, IDENTIFIER, LITERAL

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

    include("treewalking.jl")
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
        range::UnitRange{Int}
        text::String
    end

    function overlay_parse(text, cont = true)
        p = CSTParser.parse(text, cont)
        OverlayNode(p, text)
    end

    function changed_text(text, resolutions)
        sort!(resolutions, by=x->first(x.range))
        buf = IOBuffer()
        lastoffset = 0
        for r in resolutions
            if lastoffset > first(r.range)
                # Overlapping replacements. Only apply the first one for now.
                # We'll re-run this to convergence
                continue
            end
            write(buf, text[1+(lastoffset:first(r.range)-1)])
            write(buf, r.text)
            lastoffset = last(r.range)+1
        end
        write(buf, text[lastoffset+1:end])
        (length(resolutions) != 0, String(take!(buf)))
    end

    function edit_text(text, deps = map(x->x(), keys(all_deprecations)))
        replacements = Any[]
        customs = Any[]
        for dep in deps
            haskey(templates, typeof(dep)) && append!(replacements, collect(Iterators.product((dep,), templates[typeof(dep)])))
            haskey(custom_resolutions, typeof(dep)) && append!(customs, collect(Iterators.product((dep,), custom_resolutions[typeof(dep)])))
        end
        parsed_replacementes = map(x->(x[1],(overlay_parse(x[2][1],false),overlay_parse(x[2][2],false),x[2][3:end]...)), replacements)
        match = overlay_parse(text)
        function find_replacements(x, results, context=Context(false, nothing))
            for (i,(dep, (t, r, formatter, filter))) in enumerate(parsed_replacementes)
                if typeof(x) == typeof(t)
                    (!context.in_macrocall || applies_in_macrocall(dep, context)) || continue
                    result = Dict{Any,Any}()
                    match_parameters(t, x, result)[1] || continue
                    filter(dep, x, result) || continue
                    rtree = reassemble_tree(r, result)
                    buf = IOBuffer()
                    print_replacement(buf, formatter(x, rtree, result))
                    push!(results, TextReplacement(x.span, String(take!(buf))))
                end
            end
            for (i,(dep, (k, f))) in enumerate(customs)
                if isexpr(x, k)
                    f((dep, x, results, context))
                end
            end
            if isexpr(x, MacroCall) && !context.in_macrocall
                # Don't consider doc macros here. We know they don't
                # affect the meaning of the parsed code.
                if !isexpr(children(x)[1], CSTParser.GlobalRefDoc) &&
                   !(isexpr(children(x)[1], IDENTIFIER) && Expr(children(x)[1]) in (
                       Symbol("@eval"), Symbol("@inline"), Symbol("@views")
                   ))
                    context = Context(true, x)
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
                            push!(results, TextReplacement((first(docnode.span)+nquotes):(last(docnode.span)-nquotes), new_doc))
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
        changed_text(text, results)
    end

    function edit_file(fname, deps, edit=edit_text)
        text = readstring(fname)
        any_changed, new_text = edit(text, deps)
        if any_changed
            open(fname, "w") do io
                write(io, new_text)
            end
        end
        any_changed
    end

    function edit_markdown(text, deps = map(x->x(), keys(all_deprecations)))
        content = Base.Markdown.parse(text)
        changed_any = false
        new_text = text
        map(content.content) do x
            isa(x, Base.Markdown.Code) || return
            x.language in ("julia", "jldoctext") || return
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
                    while i <= 7 && idx <= endof(x.code)
                        idx = nextind(x.code, idx)
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

end # module
