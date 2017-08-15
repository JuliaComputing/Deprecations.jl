using Tokenize.Tokens: GREATER, LESS, GREATER_EQ, GREATER_THAN_OR_EQUAL_TO, LESS_EQ, LESS_THAN_OR_EQUAL_TO
begin
    struct ObsoleteVersionCheck; vers; end
    register(ObsoleteVersionCheck, Deprecation(
        "This version check is for a version of julia that is no longer supported by this package",
        "julia",
        typemin(VersionNumber), typemin(VersionNumber), typemax(VersionNumber)
    ))
    ObsoleteVersionCheck() = ObsoleteVersionCheck(Pkg.Reqs.parse(IOBuffer("julia 0.6"))["julia"])

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

    function dep_for_vers(::Type{ObsoleteVersionCheck}, vers)
        ObsoleteVersionCheck(vers["julia"])
    end

    function is_identifier(x::EXPR, id)
        isa(x, EXPR{IDENTIFIER}) || return false
        x.val == id
    end
    is_identifier(x::OverlayNode, id) = is_identifier(x.expr, id)

    opcode(x::EXPR{CSTParser.OPERATOR{6,op,false}}) where {op} = op

    match(ObsoleteVersionCheck, CSTParser.If) do x
        dep, expr, resolutions, context = x
        replace_expr = expr
        comparison = children(expr)[2]
        isexpr(comparison, CSTParser.BinaryOpCall) || return
        isexpr(children(comparison)[2], CSTParser.OPERATOR{6,op,false} where op) || return
        comparison = comparison.expr
        opc = opcode(comparison.args[2])
        haskey(comparisons, opc) || return
        # Also applies in @static context, but not necessarily in other macro contexts
        if context.in_macrocall
            context.top_macrocall == parent(expr) || return
            is_identifier(children(context.top_macrocall)[1], "@static") || return
            replace_expr = context.top_macrocall
        end
        r1 = detect_ver_arguments(comparison.args[1], comparison.args[3])
        if r1 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r1) && f(interval.upper, r1)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r1) && !f(interval.upper, r1)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr, replace_expr)
            return
        end
        r2 = detect_ver_arguments(comparison.args[3], comparison.args[1])
        if r2 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r2) && f(interval.upper, r2)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r2) && !f(interval.upper, r2)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr, replace_expr)
        end
    end
end
