struct TryCatch; end
register(TryCatch, Deprecation(
    "`try` without `catch` has been deprecated. Add a catch block.",
    "julia",
    v"0.1.0", v"0.7.0-DEV", typemax(VersionNumber)
))

match(TryCatch, CSTParser.Try) do x
    # Match ccall only
    dep, expr, resolutions, context = x
    # Don't match try blocks that already have a catch for finally clause
    for c in children(expr)
        if isexpr(c, CSTParser.KEYWORD, Tokens.FINALLY) ||
           isexpr(c, CSTParser.KEYWORD, Tokens.CATCH)
           return
        end
    end

    # Look for trivia between the END keyword and the preceeding node
    end_kw = children(expr)[end]
    ws_in_between = string(prev_node_ws(end_kw), leading_trivia(end_kw))
    rn = ChildReplacementNode(nothing, children(expr)[1:end-1], expr)
    if !('\n' in ws_in_between)
        # Add the `catch; ` right before the end
        push!(children(rn), ReplacementNode("catch", leading_trivia(end_kw),"; "))
        push!(children(rn), TriviaReplacementNode(rn, end_kw, "", trailing_trivia(end_kw)))
    else
        # Add `catch\n` on the line before the end with the same indent
        idx = findlast(ws_in_between, '\n')
        indent = ws_in_between[idx:end]
        push!(children(rn), ReplacementNode("catch", leading_trivia(end_kw),""))
        push!(children(rn), TriviaReplacementNode(rn, end_kw, indent, trailing_trivia(end_kw)))
    end
    buf = IOBuffer()
    print_replacement(buf, rn, false, false)
    push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
end
