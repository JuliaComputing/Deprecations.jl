using AbstractTrees
import AbstractTrees: children

struct OverlayNode{T}
    parent::Union{OverlayNode, Void}
    buffer::String
    expr::EXPR{T}
    fullspan::UnitRange{Int}
    span::UnitRange{Int}
end
AbstractTrees.parentlinks(::Type{<:OverlayNode}) = AbstractTrees.StoredParents()
AbstractTrees.siblinglinks(::Type{<:OverlayNode}) = AbstractTrees.ImplicitSiblings()
AbstractTrees.parent(node::OverlayNode) = node.parent
AbstractTrees.isroot(node::OverlayNode) = node.parent == nothing

OverlayNode(expr) = OverlayNode(nothing, expr, 0:expr.fullspan, expr.span - 1)
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


Base.start(node::OverlayNode) = (1, first(node.fullspan))
function Base.next(node::OverlayNode, state::Tuple{Int, Int})
    idx, offset = state
    expr = node.expr.args[idx]
    (OverlayNode(node, node.buffer, expr, offset:(offset+expr.fullspan-1), offset - 1 + expr.span), (idx+1, offset + expr.fullspan))
end
Base.done(node::OverlayNode, state::Tuple{Int, Int}) = state[1] > length(node.expr.args)
