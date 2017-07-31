function resolve_inline_body(resolutions, orig_text, expr)
    body = expr.args[3]
    push!(resolutions,
        TextReplacement(expr.fullspan,
            orig_text[1 + body.fullspan]
        ))
end
