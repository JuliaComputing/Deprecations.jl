using CSTParser: KEYWORD

function resolve_inline_body(resolutions, expr)
    body = children(expr)[3]
    push!(resolutions, TextReplacement(expr.fullspan, fullspan_text(body)))
end

function resolve_delete_expr(resolutions, expr)
    if length(children(expr)) <= 3
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
