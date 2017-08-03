using Deprecations: Deprecation
using Compat
using CSTParser: FunctionDef

begin
    struct OldParametricSyntax; end
    register(OldParametricSyntax, Deprecation(
        "Parameteric syntax of the form f{T}(x::T) is deprecated and needs to be written using the `where` keyword",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.1143",
        typemax(VersionNumber)
    ))

    # Special purpose formatter to unindent multi-line arglists
    function format_arglist(tree, matches)
        nchars_moved = sum(charwidth, fullspan_text(first(matches[:T])))+2
        wheren = isexpr(tree, FunctionDef) ? children(tree)[2] : children(tree)[1]
        # We replace the call argument
        ocall = children(wheren)[1]
        call = children(wheren)[1] = ChildReplacementNode(tree, Any[], ocall)
        for c in children(ocall)
            ws = trailing_ws(c)
            if !('\n' in ws)
                push!(call.children, c)
                continue
            end
            push!(call.children, TriviaReplacementNode(call, c, leading_ws(c),
                unindent_ws(ws, nchars_moved)))
        end
        tree
    end

    match(OldParametricSyntax,
        "function \$F{\$T}(\$ARGS...)\n\$BODY...\nend",
        "function\$F(\$ARGS...) where \$T\n\$BODY!...\nend",
        format_arglist
    )
    match(OldParametricSyntax,
        "function \$F{\$T...}(\$ARGS...)\n\$BODY...\nend",
        "function\$F(\$ARGS...) where {\$T...}\n\$BODY!...\nend",
        format_arglist
    )
    match(OldParametricSyntax,
        "\$F{\$T...}(\$ARGS...) = \$BODY",
        "\$F(\$ARGS...) where {\$T...} =\$BODY",
        format_arglist
    )
end

using Tokenize.Tokens: GREATER, LESS, GREATER_EQ, GREATER_THAN_OR_EQUAL_TO, LESS_EQ, LESS_THAN_OR_EQUAL_TO
begin
    struct ObsoleteVersionCheck; end
    register(ObsoleteVersionCheck, Deprecation(
        "This version check is for a version of julia that is no longer supported by this package",
        "julia",
        typemin(VersionNumber), typemin(VersionNumber), typemax(VersionNumber)
    ))

    const comparisons = Dict(
         GREATER                  => >,
         LESS                     => <,
         GREATER_EQ               => >=,
         GREATER_THAN_OR_EQUAL_TO => ≥,
         LESS_EQ                  => <=,
         LESS_THAN_OR_EQUAL_TO    => ≤,
         #= TODO:
         EQEQ                     => ==,
         EQEQEQ                   => ===,
         IDENTICAL_TO             => ≡,
         NOT_EQ                   => !=,
         NOT_EQUAL_TO             => ≠,
         NOT_IS                   => !==
         =#)


    function detect_ver_arguments(VERSION_arg, v_arg)
        isa(VERSION_arg, EXPR{CSTParser.IDENTIFIER}) || return nothing
        VERSION_arg.val == "VERSION" || return nothing
        isa(v_arg, EXPR{CSTParser.x_Str}) || return nothing
        isa(v_arg.args[1], EXPR{CSTParser.IDENTIFIER}) || return nothing
        isa(v_arg.args[2], EXPR{CSTParser.LITERAL{Tokens.STRING}}) || return nothing
        v_arg.args[1].val == "v" || return nothing
        VersionNumber(v_arg.args[2].val)
    end

    version_range = (v"0.6.0",typemax(VersionNumber))

    opcode(x::EXPR{CSTParser.OPERATOR{6,op,false}}) where {op} = op

    match(ObsoleteVersionCheck, CSTParser.If) do x
        expr, resolutions = x
        comparison = children(expr)[2]
        isexpr(comparison, CSTParser.BinaryOpCall) || return
        isexpr(children(comparison)[2], CSTParser.OPERATOR{6,op,false} where op) || return
        comparison = comparison.expr
        opc = opcode(comparison.args[2])
        haskey(comparisons, opc) || return
        r1 = detect_ver_arguments(comparison.args[1], comparison.args[3])
        if r1 !== nothing
            f = comparisons[opc]
            alwaystrue = f(first(version_range), r1) && f(last(version_range), r1)
            alwaysfalse = !f(first(version_range), r1) && !f(last(version_range), r1)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr)
            return
        end
        r2 = detect_ver_arguments(comparison.args[3], comparison.args[1])
        if r2 !== nothing
            f = comparisons[opc]
            alwaystrue = f(r2, first(version_range)) && f(r2, last(version_range))
            alwaysfalse = !f(r2, first(version_range)) && !f(r2, last(version_range))
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr)
        end
    end
end

begin
    struct ObsoleteCompatMacroTU; end
    register(ObsoleteCompatMacroTU, Deprecation(
        "This compat macro is no longer required",
        "julia",
        v"0.4.0", v"0.4.0", typemax(VersionNumber)
    ))

    match_macrocall(Compat, :@compat, ObsoleteCompatMacroTU,
        "@compat(Union{\$A...})",
        "Union{\$A...}",
    )

    match_macrocall(Compat, :@compat, ObsoleteCompatMacroTU,
        "@compat(Tuple{\$A...})",
        "Tuple{\$A...}",
    )

    match_macrocall(Compat, :@compat, ObsoleteCompatMacroTU,
        "@compat Union{\$A...}",
        "Union{\$A...}",
    )

    match_macrocall(Compat, :@compat, ObsoleteCompatMacroTU,
        "@compat Tuple{\$A...}",
        "Tuple{\$A...}",
    )
end

begin
    struct ObsoleteCompatString; end
    register(ObsoleteCompatString, Deprecation(
        "This Compat definition is deprecated",
        "julia",
        v"0.5.0", v"0.6.0", typemax(VersionNumber)
    ))

    match(ObsoleteCompatString,
        "Compat.UTF8String",
        "String",
    )

    match(ObsoleteCompatString,
        "Compat.ASCIIString",
        "String",
    )
end


#=
begin
    OneParamWrite = Deprecation(
        "An IO argument is now always required when calling the write function.",
        "julia",
        v"0.4.0",
        v"0.5.0-dev+5066",
        v"0.7.0"
    )

    call_match(Base, :write,
        match"write($X)",
        match"write(STDOUT, $X)"
    )
end


begin
    Delete!Env = Deprecation(
        "`delete!(ENV, k, def)` should be replaced with `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, while `delete!` returns `ENV` or `def`."
    )

    call_match(Delete!Env, Base, :delete!,
        match"delete!(::EnvHash, $B, $C)"statement,
        match"pop!($A, $B, $C)"
    )

    call_match(Delete!Env, Base, :delete!,
        match"delete!(::EnvHash, $B, $C)"
    )
end
=#
