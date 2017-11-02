using AbstractTrees
import AbstractTrees: children
using CSTParser: FunctionDef, BinarySyntaxOpCall
using Tokenize: Tokens

struct OverlayNode{T}
    parent::Union{OverlayNode, Void}
    buffer::String
    expr::Union{EXPR{T}, T}
    fullspan::UnitRange{Int}
    span::UnitRange{Int}
end

Base.Expr(n::OverlayNode) = Base.Expr(n.expr)
AbstractTrees.parentlinks(::Type{<:OverlayNode}) = AbstractTrees.StoredParents()
AbstractTrees.siblinglinks(::Type{<:OverlayNode}) = AbstractTrees.ImplicitSiblings()
AbstractTrees.parent(node::OverlayNode) = node.parent
AbstractTrees.isroot(node::OverlayNode) = node.parent == nothing

OverlayNode(expr, buffer) = OverlayNode(nothing, buffer, expr, 0:(expr.fullspan-1), expr.span - 1)
AbstractTrees.printnode(io::IO, o::OverlayNode{T}) where {T} = print(io, sprint(AbstractTrees.printnode, o.expr), " -- ", o.fullspan, " (", o.span, ")")
Base.show(io::IO, o::OverlayNode) = AbstractTrees.print_tree(io, o)

Base.length(o::OverlayNode) = length(children(o.expr))
Base.endof(o::OverlayNode)  = endof(children(o.expr))

Base.length(::CSTParser.LITERAL) = 0
Base.length(::CSTParser.OPERATOR) = 0
Base.length(::CSTParser.KEYWORD) = 0
Base.length(::CSTParser.PUNCTUATION) = 0
Base.length(::CSTParser.IDENTIFIER) = 0

children(node::OverlayNode) = node

# CSTParser gives argument in filter in the opposite lexical order,
# remove this when fixed in CSTParser
children(filter::EXPR{CSTParser.Filter}) = reverse(filter.args)

function Base.getindex(node::OverlayNode, idx::Integer)
    offset = first(node.fullspan)
    c = children(node.expr)
    for i = 1:idx-1
        offset += c[i].fullspan
    end
    expr = children(node.expr)[idx]
    OverlayNode(node, node.buffer, expr, offset:(offset+expr.fullspan-1), offset - 1 + expr.span)
end
function Base.getindex(node::OverlayNode, range::Range)
    map(x->node[x], range)
end

Base.setindex!(o::OverlayNode, x::OverlayNode, idx) = Base.setindex!(o.expr.args, x.expr, idx)

Base.start(node::OverlayNode) = (start(node.expr), first(node.fullspan))
function Base.next(node::OverlayNode, state::Tuple{Int, Int})
    idx, offset = state
    expr, idx = next(children(node.expr), idx)
    (OverlayNode(node, node.buffer, expr, offset:(offset+expr.fullspan-1), offset - 1 + expr.span), (idx, offset + expr.fullspan))
end
Base.done(node::OverlayNode, state::Tuple{Int, Int}) = done(node.expr, state[1])

function function_def_call(expr)
    if isexpr(expr, FunctionDef)
        expr = children(expr)[2]
    else
        @assert (isexpr(expr, BinarySyntaxOpCall) || isexpr(expr, CSTParser.WhereOpCall)) && isexpr(children(expr)[2], CSTParser.OPERATOR, Tokens.EQ)
    end
    while isexpr(expr, BinarySyntaxOpCall) || isexpr(expr, CSTParser.WhereOpCall)
        expr = children(expr)[1]
    end
    return expr
end
#
