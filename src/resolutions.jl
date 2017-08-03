function resolve_inline_body(resolutions, orig_text, expr)
    body = children(expr)[3]
    push!(resolutions, TextReplacement(expr.fullspan, fullspan_text(body)))
end
