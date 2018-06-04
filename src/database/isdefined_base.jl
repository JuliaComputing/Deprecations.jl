# Todo, check all packages for what they check isdefined on
using Tokenize.Tokens: COLON

# Lower commit introduced, upper commit removed
const DEFINES = Dict{Symbol, NTuple{2, VersionNumber}}()

# When a symbol changed name
function add_deprecation(from::Symbol, to::Symbol, v::VersionNumber)
    push!(DEFINES, from => (v"0.0", v))
    push!(DEFINES, to   => (v, typemax(VersionNumber)))
end

# When a symbol was removed
add_removal(sym::Symbol, v::VersionNumber) = push!(DEFINES, sym => (v"0.0", v))

# When a symbol was added
add_addition(sym::Symbol, v::VersionNumber) = push!(DEFINES, sym => (v, typemax(VersionNumber)))

let
    # STDLIB
    add_removal(:Test, v"0.7.0-DEV.2004")

    add_addition(:error_color, v"0.6.0-dev.1545") # 18628
    add_addition(:unwrap_unionall, v"0.6.0-dev.2123") # 18457
    add_addition(:xor, v"0.6.0-dev.1236") # 18977

    add_addition(Symbol("@nospecialize"), v"0.7.0-DEV.969")

    add_deprecation(:isleaftype, :isconcrete, v"0.7.0-DEV.1775") # 23666
    add_addition(:__precompile__, v"0.4.0-dev+6521")
end

begin
    struct IsDefinedBaseCheck; vers; end
    register(IsDefinedBaseCheck, Deprecation(
        "This isdefined check for Base is always true or false for the versions supported by this package",
        "julia",
        typemin(VersionNumber), typemin(VersionNumber), typemax(VersionNumber)))
        IsDefinedBaseCheck() = IsDefinedBaseCheck(Pkg.Reqs.parse(IOBuffer("julia 0.6"))["julia"])

    function dep_for_vers(::Type{IsDefinedBaseCheck}, vers)
        IsDefinedBaseCheck(vers["julia"])
    end

  get_symbol(x) = nothing
  function get_symbol(x::EXPR) #, id)
        c = children(x)

        if isexpr(x, CSTParser.Quotenode) && length(c) == 2 &&
           isexpr(c[1], OPERATOR, Tokens.COLON) && isexpr(c[2], CSTParser.IDENTIFIER)
           return c[2].val
        end

        if isexpr(x, CSTParser.Call) && isexpr(c[1], CSTParser.IDENTIFIER) &&
                c[1].val == "Symbol" && isexpr(c[3], CSTParser.LITERAL)
            return c[3].val
        end

        return nothing
    end

    match(IsDefinedBaseCheck, CSTParser.If) do x
        dep, expr, resolutions, context = x
        resolve_isdefined_call(children(expr)[2], dep, expr, resolutions, context)
    end

    match(IsDefinedBaseCheck, CSTParser.BinarySyntaxOpCall) do x
        dep, expr, resolutions, context = x
        !isexpr(children(expr)[2], OPERATOR, Tokens.LAZY_AND) && return
        resolve_isdefined_call(children(expr)[1], dep, expr, resolutions, context)
    end

    function resolve_isdefined_call(call, dep, expr, resolutions, context)
        call = call.expr

        isnegated = false
        if isexpr(call, CSTParser.UnaryOpCall)
            if isexpr(children(call)[1], OPERATOR, Tokens.NOT)
                call = children(call)[2]
                isnegated = true
            else
                return
            end
        end

        n_args = (length(children(call)) - 1  # function name
                                         - 3) # punctuation ( , )
        !(n_args == 2) && return
        function_identifier = children(call)[1]
        !is_identifier(function_identifier, "isdefined") && return
        first_arg, second_arg = children(call)[3], children(call)[5]
        symval = get_symbol(second_arg)
        symval == nothing && return
        !is_identifier(first_arg, "Base") && return
        sym = Symbol(symval)

        # Also applies in @static context, but not necessarily in other macro contexts
        replace_expr = expr
        !haskey(DEFINES, sym) && return
        if context.in_macrocall
            context.top_macrocall == parent(expr) || return
            is_macroname(context.top_macrocall, "static") || return
            replace_expr = context.top_macrocall
        end

        baselow, basehigh = DEFINES[sym]

        # bl ----------------- bh
        #     il ------- ih
        alwaysdefined = all(interval -> (interval.lower >= baselow && (interval.upper < basehigh || basehigh == typemax(VersionNumber))), dep.vers.intervals)

        # bl ---- bh                   bl ---- bh
        #             il ------- ih
        neverdefined = all(interval -> (interval.lower >= basehigh || interval.upper < baselow), dep.vers.intervals)
        if isnegated
            alwaysdefined, neverdefined = neverdefined, alwaysdefined
        end
        @assert !(alwaysdefined && neverdefined)
        if alwaysdefined
            resolve_inline_body(dep, resolutions, expr, replace_expr)
        elseif neverdefined
            resolve_delete_expr(dep, resolutions, expr, replace_expr)
        end
    end
end
