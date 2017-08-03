function resolve_inline_body(resolutions, expr)
    body = children(expr)[3]
    push!(resolutions, TextReplacement(expr.fullspan, fullspan_text(body)))
end

function resolve_delete_expr(resolutions, expr)
    push!(resolutions, TextReplacement(expr.fullspan, ""))
end
