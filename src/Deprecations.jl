module Deprecations
    using CSTParser
    using CSTParser: EXPR

    struct Deprecation
        description::String
        package::String
        minsupported::Base.VersionNumber
        minrequired::Base.VersionNumber
        maxver::Base.VersionNumber
    end

    include("treewalking.jl")
    include("formatting.jl")
    include("astmatching.jl")
    include("resolutions.jl")

    all_templates = Any[]
    all_replacements = Any[]
    deprecation = Any[]

    custom_resolutions = Any[]

    function match(dep::Deprecation, template::String, replacement::String, formatter)
        push!(deprecation, dep)
        push!(all_templates, template)
        push!(all_replacements, replacement)
    end

    function match_macrocall(mod, name, dep::Deprecation, template::String, replacement::String)
        # TODO: Make sure the macro in the current module is actually the one from `mod`
        push!(deprecation, dep)
        push!(all_templates, template)
        push!(all_replacements, replacement)
    end

    function match(f::Function, dep::Deprecation, expr_kind)
        push!(custom_resolutions, (expr_kind, f))
    end

    include("database.jl")

    struct TextReplacement
        range::UnitRange{Int}
        text::String
    end

    function overlay_parse(text)
        p = CSTParser.parse(text)
        OverlayNode(nothing, text, p, 0:p.fullspan, p.span-1)
    end

    function edit_text(text)
        replacements = collect(zip(all_templates, all_replacements))
        parsed_replacementes = map(x->(overlay_parse(x[1]),overlay_parse(x[2])), replacements)
        match = overlay_parse(text)
        function find_replacements(x, results)
            for (i,(t, r)) in enumerate(parsed_replacementes)
                if typeof(x) == typeof(t)
                    result = Dict{Any,Any}()
                    match_parameters(t, x, result)[1] || continue
                    buf = IOBuffer()
                    print_replacement(buf, reassemble_tree(r, result))
                    push!(results, TextReplacement(x.span, String(take!(buf))))
                end
            end
            for (i,(k, f)) in enumerate(custom_resolutions)
                if typeof(x) == EXPR{k}
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
        String(take!(buf))
    end


end # module
