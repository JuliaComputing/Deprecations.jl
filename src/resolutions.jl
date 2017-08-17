using CSTParser: KEYWORD

"Strip a newline from the body if present."
function maybe_strip_newline(body)
    ws = trailing_ws(body)
    (isempty(ws) || ws[end] != '\n') && return body
    TriviaReplacementNode(nothing, body, "", ws[1:end-1])
end

function except_first_line(ws)
    idx = findfirst(ws, '\n')
    idx == 0 && return ""
    ws[idx+1:end]
end

function resolve_inline_body(resolutions, expr, replace_expr)
    indent = sum(charwidth, trailing_ws(children(expr)[2])) - line_pos(replace_expr, first(replace_expr.span))
    body = format_addindent_body(children(expr)[3], -indent)
    body = maybe_strip_newline(body)
    buf = IOBuffer()
    print_replacement(buf, body, true, true)
    push!(resolutions, TextReplacement(replace_expr.span, String(take!(buf))))
end

function resolve_delete_expr(resolutions, expr, replace_expr)
    if length(children(expr)) <= 4
        push!(resolutions, TextReplacement(replace_expr.fullspan, except_first_line(trailing_ws(replace_expr))))
    elseif isexpr(children(expr)[4], KEYWORD{Tokens.ELSE})
        # Inline else body
        indent = sum(charwidth, trailing_ws(children(expr)[2])) - line_pos(replace_expr, first(replace_expr.span))
        body = format_addindent_body(children(expr)[5], -indent)
        body = maybe_strip_newline(body)
        buf = IOBuffer()
        print_replacement(buf, body, true, true)
        push!(resolutions, TextReplacement(replace_expr.span, String(take!(buf))))
    else
        indent = sum(charwidth, trailing_ws(children(expr)[2]))
        repl = ChildReplacementNode(nothing, Any[], expr)
        eif = children(expr)[4]
        @assert isexpr(eif, KEYWORD{Tokens.ELSEIF})
        push!(repl.children, ReplacementNode{KEYWORD{Tokens.IF}}("if", leading_ws(eif), trailing_ws(eif)))
        append!(repl.children, children(expr)[5:end])
        buf = IOBuffer()
        print_replacement(buf, repl, false, true)
        # Here we actually replace expr, rather than replace_expr, since we're not removing the
        # expr, entirely, just removing a branch.
        push!(resolutions, TextReplacement(expr.fullspan, String(take!(buf))))
    end
end

function replace_node(tree, node, replacement, current=tree, parent=nothing)
    (current == node) && return replacement
    isempty(children(current)) && return current
    newtree = ChildReplacementNode(parent, Any[], current)
    for c in children(current)
        if c == node
            push!(newtree.children, replacement)
        else
            push!(newtree.children, replace_node(tree, node, replacement, c, newtree))
        end
    end
    newtree
end

"""
    Aligns new lines to the opening parenthesis of a function call's argument list
"""
function format_align_arguments(tree)
    leading_trivia = string(prev_node_ws(tree), leading_ws(tree))
    # Find the amount the number characters from the last newline to the start
    # expr's span.
    lastn = rsearch(leading_trivia, '\n')
    call = function_def_call(tree)
    lparen = children(call)[2]
    nindent = mapreduce(charwidth, +, 0, tree.buffer[1 + (first(tree.span):first(lparen.span))]) +
              mapreduce(charwidth, +, 0, leading_trivia[lastn+1:end])
    ftree = format_setindent_body(call, nindent)
    rtree = replace_node(tree, call, ftree)
end

function apply_formatter(f, tree)
    rtree = f(tree)
    buf = IOBuffer()
    print_replacement(buf, rtree)
    TextReplacement(tree.span, String(take!(buf)))
end