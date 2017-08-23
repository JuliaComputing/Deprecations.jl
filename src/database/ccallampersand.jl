using CSTParser: UnarySyntaxOpCall, TimesOp

struct CCallAmpersand; end
register(CCallAmpersand, Deprecation(
    "The amersand (`&`) in ccall has been deprecated. Use Ref instead",
    "julia",
    v"0.7.0-DEV", v"0.7.0-DEV", typemax(VersionNumber)
))

match(CCallAmpersand, CSTParser.Call) do x
    # Match ccall only
    dep, expr, resolutions, context = x
    is_identifier(children(expr)[1], "ccall") || return
    has_cconv = any(x->is_identifier(children(expr)[5], x), ["cdecl", "stdcall", "fastcall", "thiscall", "llvmcall"])
    argtypes = children(expr)[7+(has_cconv ? 2 : 0)]
    expr′ = ChildReplacementNode(nothing, collect(children(expr)), expr)
    argtypes′ = children(expr′)[7+(has_cconv ? 2 : 0)] = ChildReplacementNode(expr′, collect(children(argtypes)), argtypes)
    found_any = false
    # Every other node is punctuation
    for (i, idx) in enumerate((9+(has_cconv ? 2 : 0)):2:length(children(expr)))
        node = children(expr)[idx]
        # Check if this is ampersand syntax
        if isexpr(node, UnarySyntaxOpCall) && isexpr(children(node)[1],
                OPERATOR{TimesOp,Tokens.AND})
            # Check if the argtype is `Ptr`.
            argT = children(argtypes)[2*i] # Every other node is punctuation
            isexpr(argT, Curly) || continue
            id = children(argT)[1]
            is_identifier(id, "Ptr") || continue
            # Rewrite to `Ref`
            children(argtypes′)[2*i] = replace_node(argT, id, ReplacementNode("Ref", id))
            # Remove ampersand
            children(expr′)[idx] = children(node)[2]
            found_any = true
        end
    end
    if found_any
        buf = IOBuffer()
        print_replacement(buf, expr′, false, false)
        push!(resolutions, TextReplacement(expr.span, String(take!(buf))))
    end
end