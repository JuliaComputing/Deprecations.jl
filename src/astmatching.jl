using Tokenize: Tokens

function is_template_expr(expr)
    isexpr(expr, CSTParser.UnarySyntaxOpCall) || return (false, nothing, false)
    if isexpr(children(expr)[2], OPERATOR, Tokens.DDDOT)
        isexpr(children(expr)[1], CSTParser.UnarySyntaxOpCall) || return (false, nothing, false)
        isexpr(children(children(expr)[1])[1], OPERATOR, Tokens.EX_OR) || return (false, nothing, false)
        return (true, Symbol( children(children(expr)[1])[2].val), true)
    end
    isexpr(children(expr)[1], OPERATOR, Tokens.EX_OR) || return (false, nothing, false)
    return (true, Symbol(children(expr)[2].val), false)
end

function matches_template(x, y)
    if typeof(x) != typeof(y)
        return false
    end
    if typeof(x) == CSTParser.IDENTIFIER
        return strip(x.val) == strip(y.val)
    end
    if typeof(x) in (CSTParser.KEYWORD, CSTParser.PUNCTUATION, CSTParser.OPERATOR)
        return x.kind == y.kind
    end
    true
end

matches_template(x::OverlayNode, y::OverlayNode) = matches_template(x.expr, y.expr)

function matches_template2(x, y)
    if (typeof(x) == BinarySyntaxOpCall && typeof(y) == CSTParser.WhereOpCall) ||
            (typeof(x) == CSTParser.WhereOpCall && typeof(y) == BinarySyntaxOpCall)
        return true
    end
    if typeof(x) != typeof(y)
        return false
    end
    if typeof(x) in (CSTParser.KEYWORD, CSTParser.PUNCTUATION, CSTParser.OPERATOR)
        if x.kind != y.kind
            return false
        end
    end
    if typeof(x) == CSTParser.IDENTIFIER
        if x.val != y.val
            return false
        end
    end
    true
end
matches_template2(x::OverlayNode, y::OverlayNode) = matches_template2(x.expr, y.expr)

struct EmptyMatch
    parent
end

function match_parameters(template, match, result)
    !matches_template2(template, match) && error("Shouldn't have gotten here $template, $match")
    j = 1
    for (i,x) in enumerate(children(template))
        if j > length(children(match)) && i == length(children(template))
            ret, sym, slurp = is_template_expr(x)
            if ret
                result[sym] = (("", "", ""), (EmptyMatch(match),))
                break
            end
        end
        if j > length(children(match))
            return false
        end
        y = children(match)[j]
        ret, sym, slurp = is_template_expr(x)
        if !ret && matches_template(x, y)
            ok = match_parameters(x, y, result)
            ok || return ok
            j += 1
        else
            ws = is_last_leaf(x), string(prev_node_ws(x), leading_ws(x)), string(trailing_ws(x), next_node_ws(x))
            if ret
                if !slurp
                    result[sym] =  (ws, (y,))
                    j += 1
                else
                    matched_exprs = Any[]
                    if i == length(children(template))
                        result[sym] = (ws, children(match)[j:end])
                        j = length(children(match))
                    else
                        nextx = children(template)[i + 1]
                        startj = j
                        while j <= length(children(match)) && !matches_template2(nextx, children(match)[j])
                            push!(matched_exprs, children(match)[j])
                            j += 1
                        end
                        result[sym] = (ws, isempty(matched_exprs) ? (EmptyMatch(children(match)[j]),) : matched_exprs)
                    end
                end
            else
                return false
            end
        end
    end
    if j < length(children(match))
        return false
    end
    return true
end

function leaf_is_template_expr(x)
    isempty(children(x)) && return (false, nothing, false)
    ret, sym, slurp = is_template_expr(x)
    ret || return leaf_is_template_expr(children(x)[1])
    ret, sym, slurp
end
is_template_expr(x::OverlayNode) = is_template_expr(x.expr)
leaf_is_template_expr(x::OverlayNode) = leaf_is_template_expr(x.expr)

using AbstractTrees: prevsibling, nextsibling

function prev_node_ws(node)
    ws = ""
    while true
        sib = prevsibling(node)
        if sib != nothing
            ws = string(trailing_ws(sib), ws)
            !isempty(sib.span) && return ws
            node = sib
            continue
        end
        node.parent == nothing && return ws
        node = node.parent
    end
end

function next_node_ws(node)
    while true
        sib = nextsibling(node)
        sib != nothing && return leading_ws(sib)
        node.parent == nothing && return ""
        node = node.parent
    end
end

function next_is_template(node)
    while true
        sib = nextsibling(node)
        sib != nothing && return leaf_is_template_expr(sib)
        node.parent == nothing && return (false, nothing, false)
        node = node.parent
    end
end

function is_last_leaf(node)
    while true
        sib = nextsibling(node)
        sib != nothing && return false
        node.parent == nothing && return true
        node = node.parent
    end
end

function reassemble(out::IO, replacement, matches)
    if isempty(children(replacement))
        ret, sym, _ = next_is_template(replacement)
        skip_trailing_ws = ret && endswith(String(sym), "!")
        rt = text(replacement, true, !skip_trailing_ws)
        write(out, rt)
    end
    i = 1
    while i <= length(children(replacement))
        x = children(replacement)[i]
        ret, sym, slurp = is_template_expr(x)
        if !ret
            reassemble(out, x, matches)
        else
            endswith(String(sym), "!") && (sym = Symbol(String(sym)[1:end-1]))
            exprs = matches[sym]
            write(out, prev_node_ws(first(exprs)))
            for expr in exprs
                write(out, fullspan_text(expr))
            end
        end
        i += 1
    end
    replacement
end

function skip_next_ws(c)
    ret, sym, _ = next_is_template(c)
    ret
end

"""
    For whitespace before or after a template parameter, given the whitespace:
        1. In the template
        2. In the corresponding replacement
        3. In the match

    Compute the whitespace that should end up in the final replacement.
"""
function process_whitespace(template, replacement, match)
    # The idea here to apply the "diff" between template and replacement
    # to match. For now, we require one to be a substring of the other.
    if template == replacement
        return match
    elseif length(replacement) < length(template)
        @assert startswith(template, replacement)
        # Any characters at the start of match that match `replacement` are fine
        i = 0
        while i < min(length(replacement), length(match))
            i = nextind(match, i)
            if replacement[i] != match[j]
                return match
            end
        end
        i > length(match) && return match
        j = nextind(match, i)
        # Then we cut any characters of the match that match the template
        while j <= min(length(template), length(match))
            if template[j] != match[j]
                break
            end
            j = nextind(match, j)
        end
        return string(match[1:i],match[j:end])
    elseif length(template) < length(replacement)
        @assert endswith(replacement, template)
        return string(replacement[end-sizeof(template):end], match)
    end
end

function reassemble_tree(replacement, matches, parent = nothing)
    # Whitespace hanlding: For any match, we need to process both the leading
    # and the trailing whitespace. Sometimes one template exprs trailing
    # whitespace is the next's leading whitespace. In that case, we break the
    # ties in favor of always processing leading ws (i.e. we skip trailing whitespace)
    if isempty(children(replacement))
        return skip_next_ws(replacement) ?
            TriviaReplacementNode(parent, replacement, leading_ws(replacement), "") :
            replacement
    end
    ret = ChildReplacementNode(parent, Any[], replacement)
    for (i, x) in enumerate(children(replacement))
        istemp, sym, slurp = is_template_expr(x)
        if !istemp
            push!(ret.children, reassemble_tree(x, matches, ret))
        else
            use_template_trailing_ws = true
            if endswith(String(sym), "!")
                use_template_trailing_ws = false
                sym = Symbol(String(sym)[1:end-1])
            end
            (no_trailing_ws, template_leading_ws, template_trailing_ws), exprs = matches[sym]
            expr = first(exprs)
            if typeof(expr) == EmptyMatch
                push!(ret.children, TriviaInsertionNode(prev_node_ws(expr.parent)))
                continue
            end
            lws = process_whitespace(template_leading_ws,
                string(prev_node_ws(x), leading_ws(x)),
                string(prev_node_ws(expr), leading_ws(expr)),
            )
            tws = process_whitespace(template_trailing_ws,
                string(next_node_ws(x), trailing_ws(x)),
                string(next_node_ws(last(exprs)), trailing_ws(last(exprs))),
            )
            if length(exprs) == 1
                push!(ret.children, TriviaReplacementNode(ret, expr,
                    lws, skip_next_ws(x) ? "" : tws))
            else
                push!(ret.children, TriviaReplacementNode(ret, expr,
                    lws, trailing_ws(expr)))
                append!(ret.children, exprs[2:end-1])
                lexpr = last(exprs)
                push!(ret.children, TriviaReplacementNode(ret, lexpr,
                    leading_ws(lexpr), skip_next_ws(x) ? "" : tws))
            end
        end
    end
    ret
end

function inspect_matches(result, text)
    for (sym, ((pre_ws, offset), expr)) in result
        print(STDOUT, sym, ": ")
        span = isa(expr, Array) ? (first(first(expr).fullspan):last(last(expr).fullspan)) : expr.fullspan
        show(STDOUT, text[1 + span])
        println(STDOUT)
    end
end
