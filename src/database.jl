using Compat
using CSTParser: FunctionDef, OPERATOR, PUNCTUATION, Curly, BinaryOpCall, BinarySyntaxOpCall, ComparisonOp, DeclarationOp, MacroCall, MacroName


function is_where_expr(expr)
    isexpr(expr, BinarySyntaxOpCall) || isexpr(expr, CSTParser.WhereOpCall) || return false
    isexpr(children(expr)[2], OPERATOR, Tokens.WHERE) || return false
    return true
end

function is_macroname(x::OverlayNode{MacroCall}, name)
    c = children(x)[1]
    isexpr(c, MacroName) || return false
    return is_identifier(children(c)[2], name)
end

# Rewrites related to the new parametric type syntax on 0.6, including
# rewriting inner constructors
include("database/parametric.jl")
# Delete comparisons against VERSION for versions that are no longer supported
include("database/versioncompare.jl")
# Delete compat macro invocations that are no longer needed
include("database/oldcompat.jl")
# Grabbag of relatively simple rewrites (mostly handed using the AST matcher code)
include("database/simple.jl")
# Rewrites uses of `&x` in ccall to use the equivalent `Ref` instead
include("database/ccallampersand.jl")
