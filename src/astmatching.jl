using Tokenize: Tokens

function is_template_expr(expr, allow_slurp = true)
    isa(expr, EXPR{CSTParser.UnarySyntaxOpCall}) || return (false, nothing, false)
    if isa(expr.args[2], EXPR{CSTParser.OPERATOR{7, Tokens.DDDOT, false}})
        isa(expr.args[1], EXPR{CSTParser.UnarySyntaxOpCall}) || return (false, nothing, false)
        isa(expr.args[1].args[1], EXPR{CSTParser.OPERATOR{9,Tokens.EX_OR,false}}) || return (false, nothing, false)
        return (true, Symbol(expr.args[1].args[2].val), true)
    end
    isa(expr.args[1], EXPR{CSTParser.OPERATOR{9,Tokens.EX_OR,false}}) || return (false, nothing, false)
    return (true, Symbol(expr.args[2].val), false)
end

function matches_template(x, y)
    typeof(x) == typeof(y) || return false
    if typeof(x) == EXPR{CSTParser.IDENTIFIER}
        return strip(x.val) == strip(y.val)
    end
    true
end

matches_template(x::OverlayNode, y::OverlayNode) = matches_template(x.expr, y.expr)

function match_parameters(template, match, result, start_offset = 0, pre_ws_offset = 0)
    (typeof(template) != typeof(match)) && return
    offset = start_offset
    j = 1
    for (i,x) in enumerate(children(template))
        y = children(match)[j]
        if matches_template(x, y)
            ok, pre_ws_offset = match_parameters(x, y, result, offset, pre_ws_offset)
            ok || return (ok, pre_ws_offset)
            j += 1
        else
            ret, sym, slurp = is_template_expr(x)
            if ret
                if !slurp
                    result[sym] = ((pre_ws_offset, offset), y)
                    j += 1
                else
                    matched_exprs = Any[]
                    if i == length(children(template))
                        result[sym] = ((pre_ws_offset, offset), children(match)[j:end])
                    else
                        nextx = children(template)[i + 1]
                        startj = j
                        while j <= length(children(match)) && typeof(nextx) != typeof(children(match)[j])
                            push!(matched_exprs, children(match)[j])
                            j += 1
                        end
                        result[sym] = ((pre_ws_offset, offset), matched_exprs)
                    end
                end
            else
                return (false, pre_ws_offset)
            end
        end
        pre_ws_offset = last(y.span)+1
        offset = last(y.fullspan)+1
    end
    return (true, pre_ws_offset)
end

function leaf_is_template_expr(x::EXPR)
    isempty(x.args) && return (false, nothing, false)
    ret, sym, slurp = is_template_expr(x)
    ret || return leaf_is_template_expr(x.args[1])
    ret, sym, slurp
end
is_template_expr(x::OverlayNode) = is_template_expr(x.expr)
leaf_is_template_expr(x::OverlayNode) = leaf_is_template_expr(x.expr)

function reassemble(out::IO, replacement, matches, replacement_text, orig_text, skip_trailing_ws = false)
    if isempty(children(replacement))
        rt = replacement_text[1 + (skip_trailing_ws ? (first(replacement.fullspan):last(replacement.span)) : replacement.fullspan)]
        write(out, rt)
    end
    i = 1
    while i <= length(children(replacement))
        x = children(replacement)[i]
        ret, sym, slurp = is_template_expr(x)
        if !ret
            if i + 1 <= length(children(replacement))
                nextx = children(replacement)[i+1]
                r, s, sl = leaf_is_template_expr(nextx)
                new_skip_trailing_ws = r && endswith(String(s), "!")
            end
            new_skip_trailing_ws |= (skip_trailing_ws && i == length(children(replacement)))
            reassemble(out, x, matches, replacement_text, orig_text, new_skip_trailing_ws)
        else
            endswith(String(sym), "!") && (sym = Symbol(String(sym)[1:end-1]))
            if !slurp
                (pre_ws_off, off), expr = matches[sym]
                write(out, orig_text[1+ (pre_ws_off:last(expr.fullspan))])
            else
                ((pre_ws_off, off), exprs) = matches[sym]
                write(out, orig_text[1+(pre_ws_off:last(last(exprs).fullspan))])
            end
        end
        i += 1
    end
    replacement
end

function inspect_matches(result, text)
    for (sym, ((pre_ws, offset), expr)) in result
        print(STDOUT, sym, ": ")
        span = isa(expr, Array) ? (first(first(expr).fullspan):last(last(expr).fullspan)) : expr.fullspan
        show(STDOUT, text[1 + span])
        println(STDOUT)
    end
end
