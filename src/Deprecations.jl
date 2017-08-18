module Deprecations
    using CSTParser
    using CSTParser: EXPR, MacroCall, IDENTIFIER

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

    identity_formatter(tree, matches) = tree

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
                    filter(x, result) || continue
                    rtree = reassemble_tree(r, result)
                    buf = IOBuffer()
                    print_replacement(buf, formatter(rtree, result))
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
                   !(isa(children(x)[1], IDENTIFIER) && Expr(children(x)[1]) in (
                       Symbol("@eval"), Symbol("@inline"), Symbol("@views")
                   ))
                    context = Context(true, x)
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

    function edit_file(fname, deps)
        text = readstring(fname)
        any_changed, new_text = edit_text(text, deps)
        if any_changed
            open(fname, "w") do io
                write(io, new_text)
            end
        end
        any_changed
    end

end # module
