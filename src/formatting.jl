struct ReplacementNode{T}
    text::String
    leading_trivia::String
    trailing_trivia::String
end

struct ChildReplacementNode
    parent
    children::Vector{Any}
    onode
end
AbstractTrees.printnode(io::IO, x::ChildReplacementNode) = AbstractTrees.printnode(io, x.onode)

struct TriviaInsertionNode
    trivia::String
end

struct TriviaReplacementNode
    parent
    onode
    leading_trivia::String
    trailing_trivia::String
end
function AbstractTrees.printnode(io::IO, x::TriviaReplacementNode)
    AbstractTrees.printnode(io, x.onode)
    print(io, " [", repr(x.leading_trivia), " ", repr(x.trailing_trivia), "]")
end
Base.show(io::IO, x::Union{ChildReplacementNode, TriviaReplacementNode}) = AbstractTrees.print_tree(io, x)

function unindent_ws(ws, nchars)
    buf = IOBuffer()
    i = start(ws)
    while i <= sizeof(ws)
        c = ws[i]
        write(buf, ws[i])
        i = nextind(ws, i)
        if c == '\n'
            # Skip whitespace until nchars is reached
            remaining_chars = nchars
            while i <= sizeof(ws) && remaining_chars > 0
                c = ws[i]
                !isspace(c) && break
                c == ' ' && (remaining_chars -= 1)
                c == '\t' && (remaining_chars -= 4)
                i = nextind(ws, i)
            end
        end
    end
    String(take!(buf))
end

isexpr(x::EXPR{T}, S) where {T} = T <: S
isexpr(x::OverlayNode{T}, S) where {T} = T <: S
isexpr(x::ChildReplacementNode, S) = isexpr(x.onode, S)
isexpr(x::TriviaReplacementNode, S) = isexpr(x.onode, S)

children(x::ChildReplacementNode) = x.children
children(x::TriviaReplacementNode) = children(x.onode)

trailing_ws(x::ChildReplacementNode) = trailing_ws(last(x.children))
trailing_ws(x::TriviaReplacementNode) = x.trailing_trivia

print_replacement(io::IO, node::OverlayNode, leading_trivia, trailing_trivia) = print(io, text(node, leading_trivia, trailing_trivia))
function print_replacement(io::IO, node::ChildReplacementNode, leading_trivia, trailing_trivia)
    length(node.children) == 0 && return
    length(node.children) == 1 && return print_replacement(io, first(node.children), leading_trivia, trailing_trivia)
    print_replacement(io, first(node.children), leading_trivia, true)
    foreach(c->print_replacement(io, c, true, true), node.children[2:end-1])
    print_replacement(io, last(node.children), true, trailing_trivia)
end
function print_replacement(io::IO, node::TriviaReplacementNode, leading_trivia, trailing_trivia)
    leading_trivia && print(io, node.leading_trivia)
    print_replacement(io, node.onode, false, false)
    trailing_trivia && print(io, node.trailing_trivia)
end
print_replacement(io::IO, node::TriviaInsertionNode, leading_trivia, trailing_trivia) = print(io, node.trivia)
function print_replacement(io::IO, node::ReplacementNode, leading_trivia, trailing_trivia)
    leading_trivia && print(io, node.leading_trivia)
    print(io, node.text)
    trailing_trivia && print(io, node.trailing_trivia)
end

print_replacement(io::IO, node) = print_replacement(io, node, false, false)
