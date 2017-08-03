module Deprecations
    using CSTParser
    using CSTParser: EXPR

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
    templates = Dict{Any,Vector{Tuple{Any, Any, Any}}}()
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

    function match(dep, template::String, replacement::String, formatter = identity_formatter)
        haskey(templates, dep) || (templates[dep] = Vector{Tuple{Any, Any, Any}}())
        push!(templates[dep], (template, replacement, formatter))
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

    struct TextReplacement
        range::UnitRange{Int}
        text::String
    end

    function overlay_parse(text, cont = true)
        p = CSTParser.parse(text, cont)
        OverlayNode(p, text)
    end

    function edit_text(text, deps = map(x->x(), keys(all_deprecations)))
        replacements = Any[]
        customs = Any[]
        for dep in deps
            haskey(templates, typeof(dep)) && append!(replacements, templates[typeof(dep)])
            haskey(custom_resolutions, typeof(dep)) && append!(customs, custom_resolutions[typeof(dep)])
        end
        parsed_replacementes = map(x->(overlay_parse(x[1],false),overlay_parse(x[2],false),x[3]), replacements)
        match = overlay_parse(text)
        function find_replacements(x, results)
            for (i,(t, r, formatter)) in enumerate(parsed_replacementes)
                if typeof(x) == typeof(t)
                    result = Dict{Any,Any}()
                    match_parameters(t, x, result)[1] || continue
                    rtree = reassemble_tree(r, result)
                    buf = IOBuffer()
                    print_replacement(buf, formatter(rtree, result))
                    push!(results, TextReplacement(x.span, String(take!(buf))))
                end
            end
            for (i,(k, f)) in enumerate(customs)
                if isexpr(x, k)
                    f((x, text, results))
                end
            end
            for arg in children(x)
                find_replacements(arg, results)
            end
        end
        results = TextReplacement[]
        find_replacements(match, results)
        sort!(results, by=x->first(x.range))
        buf = IOBuffer()
        lastoffset = 0
        for r in results
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
        (length(results) != 0,String(take!(buf)))
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
