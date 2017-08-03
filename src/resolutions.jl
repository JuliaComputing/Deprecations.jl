using CSTParser: KEYWORD

function format_unindent_body(expr, nindent, parent = nothing)
    nexpr = ChildReplacementNode(parent, Any[], expr)
    for c in children(expr)
        # For leaves, try to unindent
        if isempty(children(c))
            push!(nexpr.children, TriviaReplacementNode(next, c, leading_ws(c),
                unindent_ws(trailing_ws(c), nindent)))
        else
            push!(nexpr.children, format_unindent_body(c, nindent, nexpr))
        end
    end
    nexpr
end

function resolve_inline_body(resolutions, expr)
    indent = sum(charwidth, trailing_ws(children(expr)[2]))
    body = format_unindent_body(children(expr)[3], indent)
    buf = IOBuffer()
    print_replacement(buf, body)
    push!(resolutions, TextReplacement(expr.span, String(take!(buf))))
end

function resolve_delete_expr(resolutions, expr)
    if length(children(expr)) <= 4
        push!(resolutions, TextReplacement(expr.fullspan, ""))
    elseif isexpr(children(expr)[4], KEYWORD{Tokens.ELSE})
        # Inline else body
        push!(resolutions, TextReplacement(expr.fullspan, fullspan_text(children(expr)[5])))
    else
        repl = ChildReplacementNode(nothing, Any[], expr)
        eif = children(expr)[4]
        @assert isexpr(eif, KEYWORD{Tokens.ELSEIF})
        push!(repl.children, ReplacementNode{KEYWORD{Tokens.IF}}("if", leading_ws(eif), trailing_ws(eif)))
        append!(repl.children, children(expr)[5:end])
        buf = IOBuffer()
        print_replacement(buf, repl, false, true)
        push!(resolutions, TextReplacement(expr.fullspan, String(take!(buf))))
    end
end