struct DefaultWalker <: Walker; end

function trav(w::Walker, x::CSTParser.LeafNode, s, S::State)
    if x isa CSTParser.IDENTIFIER && !S.isquotenode
        binding = find_ref(CSTParser.str_value(x), S)
        push!(S.refs, Reference(x, binding, Location(S.loc.path, S.loc.offset + x.span)))
    end
    S.loc.offset += x.fullspan
end

function trav(w::Walker, x::CSTParser.EXPR{T}, s, S::State) where T <: Union{CSTParser.MacroName,CSTParser.Quote}
    S.loc.offset += x.fullspan
end

function trav(w::Walker, x, s, S::State)
    x isa CSTParser.EXPR{CSTParser.Quotenode} && (S.isquotenode = true)
    for a in x
        get_external_binding(a, s, S)
        if enter_scope(x, s, S)
            create_scope(a, s, S)
            lint_call(w, a, s, S)
            trav(w, a, S.current_scope, S)
            S.current_scope = s
        else
            S.loc.offset += a.fullspan
        end
    end
    x isa CSTParser.EXPR{CSTParser.Quotenode} && (S.isquotenode = false)
    s
end

trav(w::Walker, x::OverlayNode, s, S::State) = trav(w, x.expr, s, S)

function trav(w::Walker, x)
    S = State()
    trav(w, x, S.current_scope, S)
    find_bad_refs(S)
    return S
end

function trav(w::Walker, path::String)
    S = State{FileSystem}(Scope(), Location(path, 0), "", [], [], 0:0, false, Dict(path => File(path, nothing, [])), FileSystem())
    x = CSTParser.parse(read(path, String), true)
    trav(w, x, S.current_scope, S)
    find_bad_refs(S)
    return S
end

function enter_scope(x, s, S)
    isempty(S.target_file) && return true
    ns =  CSTParser.defines_function(x) ||
    # CSTParser.defines_module(x) ||
    CSTParser.defines_macro(x) ||
    CSTParser.defines_datatype(x) ||
    x isa CSTParser.EXPR{CSTParser.Let} ||
    x isa CSTParser.EXPR{CSTParser.Do} ||
    x isa CSTParser.EXPR{CSTParser.While} ||
    x isa CSTParser.EXPR{CSTParser.For} ||
    x isa CSTParser.EXPR{CSTParser.Try} ||
    x isa CSTParser.WhereOpCall ||
    x isa CSTParser.EXPR{CSTParser.Generator} ||
    x isa CSTParser.EXPR{CSTParser.Quote} ||
    CSTParser.defines_anon_function(x) ||
    CSTParser.is_assignment(x) && x.arg1 isa CSTParser.EXPR{CSTParser.Curly}

    if ns
        if S.loc.path == S.target_file
            return true
        else
            return false
        end
    else
        return true
    end

end
