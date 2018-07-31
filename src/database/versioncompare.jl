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

    function process_comparison(comparison, dep, context)
        isexpr(comparison, CSTParser.BinaryOpCall) || return nothing
        iscomparison(children(comparison)[2]) || return nothing
        comparison = comparison.expr
        opc = opcode(children(comparison)[2])
        haskey(comparisons, opc) || return nothing
        r1 = detect_ver_arguments(children(comparison)[1], children(comparison)[3])
        if r1 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r1) && f(interval.upper, r1)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r1) && !f(interval.upper, r1)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            return (alwaystrue, alwaysfalse)
        end
        r2 = detect_ver_arguments(children(comparison)[3], children(comparison)[1])
        if r2 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r2) && f(interval.upper, r2)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r2) && !f(interval.upper, r2)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            return (alwaystrue, alwaysfalse)
        end
        return nothing
    end

    function in_statement_position(expr)
        p = parent(expr)
        p === nothing && return true
        if isexpr(p, CSTParser.Begin) || isexpr(p, CSTParser.Block)
            # Could still be in value position if we're the last value in the
            # block and the block itself is in value position
            nextsibling(expr) !== nothing && return true
            return in_statement_position(p)
        end
        return false
    end

    match(ObsoleteVersionCheck, CSTParser.BinaryOpCall) do x
        dep, expr, resolutions, context = x
        result = process_comparison(expr, dep, context)
        result === nothing && return
        (alwaystrue, alwaysfalse) = result
        (alwaysfalse || alwaystrue) || return
        resolve_boolean(dep, resolutions, context, expr, alwaystrue)
    end
end
