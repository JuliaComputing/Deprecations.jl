using Deprecations: Deprecation
using Compat
using CSTParser: FunctionDef, OPERATOR, PUNCTUATION, Curly, BinaryOpCall, BinarySyntaxOpCall, ComparisonOp, DeclarationOp

# Rewrites related to the new parametric type syntax on 0.6, including
# rewriting inner constructors
include("database/parametric.jl")
# Delete comparisons against VERSION for versions that are no longer supported
include("database/versioncompare.jl")
# Delete compat macro invocations that are no longer needed
include("database/oldcompat.jl")
# Grabbag of relatively simple rewrites (mostly handed using the AST matcher code)
include("database/simple.jl")