struct ReplacementNode{T}
    text::String
    leading_trivia::String
    trailing_trivia::String
end

struct ChildReplacementNode
    parent
    children::Vector{Any}
    onode
    function ChildReplacementNode(a,b,c)
        # For now, always force Trivia replacement nodes to wrap child replacement nodes
        @assert !isa(c, TriviaReplacementNode)
        new(a,b,c)
    end
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
TriviaReplacementNode(parent, node::TriviaReplacementNode, onode) = TriviaReplacementNode(parent, onode, node.leading_trivia, node.trailing_trivia)
function AbstractTrees.printnode(io::IO, x::TriviaReplacementNode)
    AbstractTrees.printnode(io, x.onode)
    print(io, " [", repr(x.leading_trivia), " ", repr(x.trailing_trivia), "]")
end
Base.show(io::IO, x::Union{ChildReplacementNode, TriviaReplacementNode}) = AbstractTrees.print_tree(io, x)

function line_pos(node::OverlayNode, pos)
    buffer_pos = 1+pos
    find_pos = findprev(i->i=='\n', node.buffer, buffer_pos)
    (buffer_pos - find_pos) - 1
end

function countindent_ws(ws, indents)
    i = start(ws)
    while i <= sizeof(ws)
        c = ws[i]
        i = nextind(ws, i)
        if c == '\n'
            indent = 0
            while i <= sizeof(ws)
                c = ws[i]
                !isspace(c) && break
                c == ' ' && (indent += 1)
                c == '\t' && (indent += 4)
                i = nextind(ws, i)
            end
            push!(indents, indent)
        end
    end
end

function addindent_ws(ws, nchars)
    buf = IOBuffer()
    i = start(ws)
    while i <= sizeof(ws)
        c = ws[i]
        write(buf, ws[i])
        i = nextind(ws, i)
        if c == '\n'
            # Skip whitespace until nchars is reached
            remaining_chars = -nchars
            while i <= sizeof(ws) && remaining_chars > 0
                c = ws[i]
                !isspace(c) && break
                c == ' ' && (remaining_chars -= 1)
                c == '\t' && (remaining_chars -= 4)
                i = nextind(ws, i)
            end
            while remaining_chars < 0
                write(buf, ' ')
                remaining_chars += 1
            end
        end
    end
    String(take!(buf))
end

function setindent_ws(ws, nchars)
    buf = IOBuffer()
    i = start(ws)
    while i <= sizeof(ws)
        c = ws[i]
        write(buf, ws[i])
        i = nextind(ws, i)
        if c == '\n'
            # Skip whitespace until nchars is reached
            counted_ws = 0
            while i <= sizeof(ws) && counted_ws <= nchars
                c = ws[i]
                write(buf, c)
                !isspace(c) && break
                c == ' ' && (counted_ws += 1)
                c == '\t' && (counted_ws += 4)
                i = nextind(ws, i)
            end
            # Skip remaining whitespace
            if counted_ws == nchars
                while isspace(ws[i])
                    i = nextind(ws, i)
                end
            else
                # Add whitespace until we reach the desired indent
                while counted_ws < nchars
                    write(buf, ' ')
                    counted_ws += 1
                end
            end
        end
    end
    String(take!(buf))
end

function countindent_body(expr, indents = Int[])
    for c in children(expr)
        # For leaves, count indent
        if isempty(children(c))
            countindent_ws(trailing_ws(c), indents)
        else
            countindent_body(c, indents)
        end
    end
    indents
end

function _format_addindent_body(expr, nexpr, nindent)
    for c in children(expr)
        # For leaves, try to unindent
        if isempty(children(c))
            push!(nexpr.children, TriviaReplacementNode(next, c, leading_ws(c),
                addindent_ws(trailing_ws(c), nindent)))
        else
            push!(nexpr.children, format_addindent_body(c, nindent, nexpr))
        end
    end
end

function _format_setindent_body(expr, nexpr, nindent)
    for c in children(expr)
        # For leaves, try to unindent
        if isempty(children(c))
            push!(nexpr.children, TriviaReplacementNode(next, c, leading_ws(c),
                setindent_ws(trailing_ws(c), nindent)))
        else
            push!(nexpr.children, format_setindent_body(c, nindent, nexpr))
        end
    end
end

function format_addindent_body(expr, nindent, parent = nothing)
    nexpr = ChildReplacementNode(parent, Any[], expr)
    _format_addindent_body(expr, nexpr, nindent)
    nexpr
end
function format_addindent_body(expr::TriviaReplacementNode, nindent, parent = nothing)
    nexpr = ChildReplacementNode(nothing, Any[], expr.onode)
    _format_addindent_body(expr, nexpr, nindent)
    TriviaReplacementNode(parent, nexpr, leading_ws(expr), addindent_ws(trailing_ws(expr), nindent))
end

function format_setindent_body(expr, nindent, parent = nothing)
    nexpr = ChildReplacementNode(parent, Any[], expr)
    _format_setindent_body(expr, nexpr, nindent)
    nexpr
end
function format_setindent_body(expr::TriviaReplacementNode, nindent, parent = nothing)
    nexpr = ChildReplacementNode(nothing, Any[], expr.onode)
    _format_setindent_body(expr, nexpr, nindent)
    TriviaReplacementNode(parent, nexpr, leading_ws(expr), setindent_ws(trailing_ws(expr), nindent))
end


isexpr(x::EXPR{T}, S) where {T} = T <: S
isexpr(x::OverlayNode{T}, S) where {T} = T <: S
isexpr(x::ChildReplacementNode, S) = isexpr(x.onode, S)
isexpr(x::TriviaReplacementNode, S) = isexpr(x.onode, S)

children(x::ChildReplacementNode) = x.children
children(x::TriviaReplacementNode) = children(x.onode)

trailing_ws(x::ChildReplacementNode) = trailing_ws(last(x.children))
trailing_ws(x::TriviaReplacementNode) = x.trailing_trivia
trailing_ws(x::ReplacementNode) = x.trailing_trivia

leading_ws(x::ChildReplacementNode) = leading_ws(last(x.children))
leading_ws(x::TriviaReplacementNode) = x.leading_trivia
leading_ws(x::ReplacementNode) = x.leading_trivia

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

