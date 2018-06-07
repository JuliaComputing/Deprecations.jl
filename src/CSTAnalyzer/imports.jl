function import_has_dots(x)
    i = 2
    while CSTParser.is_dot(x.args[i])
        i +=1
    end
    return i - 2
end

function unwrap_imports(x)
    !all(CSTParser.is_colon(a) || a isa CSTParser.PUNCTUATION || a isa CSTParser.IDENTIFIER || a isa CSTParser.KEYWORD for a in x) && return []
    prefix = Symbol[]
    vars = Vector{Symbol}[]

    i = 2
    if any(CSTParser.is_colon(a) for a in x)
        while !CSTParser.is_colon(x.args[i])
            !(x.args[i] isa CSTParser.PUNCTUATION) && push!(prefix, Symbol(x.args[i].val))
            i += 1
        end
        i += 1
    end

    while i <= length(x.args)
        if CSTParser.is_comma(x.args[i])
            i += 1
        end
        var = copy(prefix)
        while i<= length(x.args) && !CSTParser.is_comma(x.args[i])
            !(x.args[i] isa CSTParser.PUNCTUATION) && push!(var, Symbol(x.args[i].val))
            i += 1
        end
        push!(vars, var)
    end
    return vars
end

function is_pkg_available(pkg::Symbol, S)
    string(pkg) in readdir(Pkg.dir())
end

function is_pkg_loaded(pkg::Symbol, S)
    string(pkg) in keys(loaded_mods)
end

function get_imports(x, S) end
function get_imports(x::CSTParser.EXPR{T}, S) where T <: Union{CSTParser.Using,CSTParser.Import,CSTParser.ImportAll}
    u = T == CSTParser.Using
    vars = unwrap_imports(x)
    for v in vars
        if u && string(v[1]) in keys(S.current_scope.names) && S.current_scope.names[string(v[1])][end].t == :Module
            mx = S.current_scope.names[string(v[1])][end].val.args[3].args
            for a in mx
                if a isa CSTParser.EXPR{CSTParser.Export}
                    for i = 2:length(a.args)
                        if a.args[i] isa CSTParser.IDENTIFIER
                            add_binding(x, CSTParser.str_value(a.args[i]), :Any, S::State, S.loc.offset + x.span)
                        end
                    end
                end
            end
        else
            add_binding(x, string(v[end]), :Any, S::State, S.loc.offset + x.span)
        end
    end
end


function mod_names(m::Module, d = Dict{String,Tuple{Set{Symbol},Set{Symbol}}}())
    ext = names(m)
    int = names(m, true, true)
    d[string(m)] = (Set(ext), Set(int))
    for n in int
        if isdefined(m, n) && getfield(m, n) isa Module && !(string(getfield(m, n)) in keys(d))
            mod_names(getfield(m, n), d)
        end
    end
    d
end
