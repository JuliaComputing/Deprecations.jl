struct ChildReplacementNode
    parent
    children::Vector{Any}
    onode
end

struct TriviaReplacementNode
    parent
    onode
    leading_trivia::String
    trailing_trivia::String
end

function unindent_ws(ws, nchars)
    buf = IOBuffer()
    i = start(ws)
    while i < sizeof(ws)
        c = ws[i]
        write(buf, ws[i])
        i = nextind(ws, i)
        if c == '\n'
            # Skip whitespace until nchars is reached
            remaining_chars = nchars
            while remaining_chars > 0
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

isexpr(x::EXPR{T}, S) where {T} = T == S
isexpr(x::OverlayNode{T}, S) where {T} = T == S
isexpr(x::ChildReplacementNode, S) = isexpr(x.onode, S)
isexpr(x::TriviaReplacementNode, S) = isexpr(x.onode, S)

children(x::ChildReplacementNode) = x.children
children(x::TriviaReplacementNode) = children(x.onode)
