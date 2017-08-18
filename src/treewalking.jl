using AbstractTrees
import AbstractTrees: children
using CSTParser: FunctionDef, BinarySyntaxOpCall
using Tokenize: Tokens

struct OverlayNode{T}
    parent::Union{OverlayNode, Void}
    buffer::String
    expr::EXPR{T}
    fullspan::UnitRange{Int}
    span::UnitRange{Int}
end
Base.Expr(n::OverlayNode) = Base.Expr(n.expr)
AbstractTrees.parentlinks(::Type{<:OverlayNode}) = AbstractTrees.StoredParents()
AbstractTrees.siblinglinks(::Type{<:OverlayNode}) = AbstractTrees.ImplicitSiblings()
AbstractTrees.parent(node::OverlayNode) = node.parent
AbstractTrees.isroot(node::OverlayNode) = node.parent == nothing
OverlayNode(expr, buffer) = OverlayNode(nothing, buffer, expr, 0:(expr.fullspan-1), expr.span - 1)

AbstractTrees.printnode(io::IO, o::OverlayNode{T}) where {T} = print(io, T, "  ", o.fullspan, " (", o.span, ")")
Base.show(io::IO, o::OverlayNode) = AbstractTrees.print_tree(io, o)

Base.length(o::OverlayNode) = length(o.expr.args)
Base.endof(o::OverlayNode) = endof(o.expr.args)

children(node::OverlayNode) = node
function Base.getindex(node::OverlayNode, idx::Integer)
    offset = first(node.fullspan)
    for i = 1:idx-1
        offset += node.expr.args[i].fullspan
    end
    expr = node.expr.args[idx]
    OverlayNode(node, node.buffer, expr, offset:(offset+expr.fullspan-1), offset - 1 + expr.span)
end
function Base.getindex(node::OverlayNode, range::Range)
    map(x->node[x], range)
end

Base.setindex!(o::OverlayNode, x::OverlayNode, idx) = Base.setindex!(o.expr.args, x.expr, idx)

Base.start(node::OverlayNode) = (1, first(node.fullspan))
function Base.next(node::OverlayNode, state::Tuple{Int, Int})
    idx, offset = state
    expr = node.expr.args[idx]
    (OverlayNode(node, node.buffer, expr, offset:(offset+expr.fullspan-1), offset - 1 + expr.span), (idx+1, offset + expr.fullspan))
end
Base.done(node::OverlayNode, state::Tuple{Int, Int}) = state[1] > length(node.expr.args)

function function_def_call(expr)
    if isexpr(expr, FunctionDef)
        expr = children(expr)[2]
    else
        @assert isexpr(expr, BinarySyntaxOpCall) && isexpr(children(expr)[2], CSTParser.OPERATOR{1,Tokens.EQ,false})
    end
    while isexpr(expr, BinarySyntaxOpCall)
        expr = children(expr)[1]
    end
    return expr
end