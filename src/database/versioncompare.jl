using Tokenize.Tokens: GREATER, LESS, GREATER_EQ, GREATER_THAN_OR_EQUAL_TO, LESS_EQ, LESS_THAN_OR_EQUAL_TO
using CSTParser: MacroName
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
        isexpr(VERSION_arg, CSTParser.IDENTIFIER) || return nothing
        VERSION_arg.val == "VERSION" || return nothing
        isexpr(v_arg, CSTParser.x_Str) || return nothing
        isexpr(children(v_arg)[1], CSTParser.IDENTIFIER) || return nothing
        isexpr(children(v_arg)[2], CSTParser.LITERAL, Tokens.STRING) || return nothing
        children(v_arg)[1].val == "v" || return nothing
        VersionNumber(children(v_arg)[2].val)
    end

    function dep_for_vers(::Type{ObsoleteVersionCheck}, vers)
        ObsoleteVersionCheck(vers["julia"])
    end

    opcode(x::CSTParser.OPERATOR) = x.kind
    iscomparison(x::CSTParser.OPERATOR) = CSTParser.precedence(x) == 6
    iscomparison(x::OverlayNode) = iscomparison(x.expr)
    match(ObsoleteVersionCheck, CSTParser.If) do x
        dep, expr, resolutions, context = x
        replace_expr = expr
        comparison = children(expr)[2]
        isexpr(comparison, CSTParser.BinaryOpCall) || return
        iscomparison(children(comparison)[2]) || return
        comparison = comparison.expr
        opc = opcode(children(comparison)[2])
        haskey(comparisons, opc) || return
        # Also applies in @static context, but not necessarily in other macro contexts
        if context.in_macrocall
            context.top_macrocall == parent(expr) || return
            is_macroname(context.top_macrocall, "static") || return
            replace_expr = context.top_macrocall
        end
        r1 = detect_ver_arguments(children(comparison)[1], children(comparison)[3])
        if r1 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r1) && f(interval.upper, r1)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r1) && !f(interval.upper, r1)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(dep, resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(dep,resolutions, expr, replace_expr)
            return
        end
        r2 = detect_ver_arguments(children(comparison)[3], children(comparison)[1])
        if r2 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r2) && f(interval.upper, r2)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r2) && !f(interval.upper, r2)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(dep, resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(dep, resolutions, expr, replace_expr)
        end
    end
end
