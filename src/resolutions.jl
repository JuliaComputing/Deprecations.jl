using CSTParser: KEYWORD

"Strip a trailing line of purely whitespace (if present)."
function maybe_strip_trailing_trivia_line(body)
    ws = trailing_trivia(body)
    idx = findlast(c->c=='\n', ws)
    idx == 0 && return body
    any(c->!isspace(c), ws[idx:end]) && return body
    TriviaReplacementNode(nothing, body, leading_trivia(body), ws[1:prevind(ws, idx)])
end

function last_line(ws)
    idx = findlast(c->c=='\n', ws)
    idx == 0 && return ""
    ws[idx+1:end]
end

function except_first_line(ws)
    idx = findfirst(ws, '\n')
    idx == 0 && return ""
    ws[idx+1:end]
end

function except_leading_ws(ws)
    idx = findfirst(!isspace, ws)
    idx == 0 && return ""
    ws[idx:end]
end

function compute_resolve_inline_body(expr, inline_body, replace_expr)
    # Inline else body
    _last_line = last_line(trailing_trivia(children(expr)[2]))
    indent = (isempty(_last_line) ? 0 : sum(charwidth, _last_line)) - line_pos(replace_expr, first(replace_expr.span))
    # Keep any trivia before the first statement of the body
    ws = string(prev_node_ws(inline_body), leading_trivia(inline_body))
    body = format_addindent_body(TriviaReplacementNode(nothing, inline_body,
        ws, trailing_trivia(inline_body)), -indent)
    body = maybe_strip_trailing_trivia_line(body)
    # Strip the first line (the content after the if/else)
    body = TriviaReplacementNode(nothing, body, except_leading_ws(except_first_line(leading_trivia(body))), trailing_trivia(body))
    buf = IOBuffer()
    print_replacement(buf, body, true, true)
    String(take!(buf))
end

function resolve_inline_body(dep, resolutions, expr, replace_expr)
    push!(resolutions, TextReplacement(dep, replace_expr.span, compute_resolve_inline_body(expr, children(expr)[3], replace_expr)))
end

function resolve_delete_expr(dep, resolutions, expr, replace_expr, prefix="")
    if length(children(expr)) <= 4
        # Find our current indentation
        prev = findprev_str(i -> !isspace(i), replace_expr.buffer, prevind(replace_expr.buffer, first(replace_expr.fullspan)))
        if prev == 0
            prev = 1
        else
            # Essentially, we drop everything except for the content up until
            # the first newline from the leading whitespace and do the opposite
            # for the trailing whitespace
            while true
                prev = nextind(replace_expr.buffer, prev)
                c = replace_expr.buffer[prev]
                !isspace(c) && break
                if c == '\n'
                    prev = nextind(replace_expr.buffer, prev)
                    break
                end
            end
        end
        push!(resolutions, TextReplacement(dep, prev:last(replace_expr.fullspan),
                                           string(prefix, except_first_line(trailing_trivia(replace_expr)))))
    elseif isexpr(children(expr)[4], KEYWORD, Tokens.ELSE)
        # Inline else body
        push!(resolutions, TextReplacement(dep, replace_expr.span, string(prefix,
            compute_resolve_inline_body(expr, children(expr)[5], replace_expr))))
    else
        # elseif -> if
        indent = sum(charwidth, trailing_trivia(children(expr)[2]))
        repl = ChildReplacementNode(nothing, Any[], expr)
        eif = children(expr)[4]
        @assert isexpr(eif, KEYWORD, Tokens.ELSEIF)
        push!(repl.children, ReplacementNode("if", leading_trivia(eif), trailing_trivia(eif)))
        append!(repl.children, children(expr)[5:end])
        buf = IOBuffer()
        print_replacement(buf, repl, false, true)
        # Here we actually replace expr, rather than replace_expr, since we're not removing the
        # expr, entirely, just removing a branch.
        push!(resolutions, TextReplacement(dep, expr.fullspan, string(prefix, String(take!(buf)))))
    end
end

    # Could be implemented in the future
    function is_statically_effect_free(expr)
        if isexpr(expr, CSTParser.Call)
            is_identifier(children(expr)[1], "isdefined") || return false
            all(is_statically_effect_free, children(expr)[2:end]) || return false
            return true
        end
        # TODO: Only if guaranteed to be defined
        isexpr(expr, CSTParser.IDENTIFIER) && return true
        isexpr(expr, CSTParser.PUNCTUATION) && return true
        isexpr(expr, CSTParser.Quotenode) && return true
        return false
    end

    function resolve_boolean(dep, resolutions, context, expr, alwaystruefalse, effects = Any[])
        p = parent(expr)
        if isexpr(p, CSTParser.BinarySyntaxOpCall)
            context.in_macrocall && return
            is_and = isexpr(children(p)[2], OPERATOR, Tokens.LAZY_AND)
            is_or = isexpr(children(p)[2], OPERATOR, Tokens.LAZY_OR)
            (is_and || is_or) || @goto out
            if expr == children(p)[1]
                if is_and ? alwaystruefalse : !alwaystruefalse
                    !isempty(effects) && return
                    push!(resolutions,
                        TextReplacement(dep,
                        first(p.span):(first(children(p)[3].span)-1), ""))
                    return
                elseif is_and ? !alwaystruefalse : alwaystruefalse
                    resolve_boolean(dep, resolutions, context, p, is_and ? false : true, effects)
                end
            else
                @assert expr == children(p)[3]
                if is_and ? alwaystruefalse : !alwaystruefalse
                    !isempty(effects) && return
                    push!(resolutions,
                        TextReplacement(dep),
                        first(children(p)[2].span):last(children(p)[3].span),
                        "")
                elseif is_and ? !alwaystruefalse : alwaystruefalse
                    fcond = children(p)[1]
                    if !is_statically_effect_free(fcond)
                        length(effects) >= 2 && return
                        if length(effects) == 1
                            effects[1] = ChildReplacementNode(nothing, Any[children(p)[1:2]..., effects[1]], p)
                        else
                            pushfirst!(effects, TriviaReplacementNode(
                                nothing,
                                fcond,
                                leading_trivia(fcond), ""))
                        end
                    end
                    resolve_boolean(dep, resolutions, context, p, is_and ? false : true, effects)
                end
            end
            return
        elseif isexpr(p, CSTParser.ConditionalOpCall)
                        is_ternary = isexpr(children(p)[2], OPERATOR, Tokens.CONDITIONAL)
            if alwaystruefalse
                push!(resolutions,
                    TextReplacement(dep,
                    first(p.span):(first(children(p)[3].span)-1), ""))
                push!(resolutions,
                    TextReplacement(dep,
                    (last(children(p)[3].span)+1):last(p.span), ""))
            else
                push!(resolutions,
                    TextReplacement(dep,
                    first(p.span):(first(children(p)[end].span)-1), ""))
            end
            return
        elseif isexpr(p, CSTParser.If)
            replace_expr = p
            if context.in_macrocall
                context.top_macrocall == parent(p) || return nothing
                is_macroname(context.top_macrocall, "static") || return nothing
                replace_expr = context.top_macrocall
            end
            indent = 0
            buf = IOBuffer()
            anythingleft = alwaystruefalse || length(children(p)) > 4
            ind = indentation(replace_expr)
            for (i, node) in enumerate(effects)
                print_replacement(buf, node, true, true)
                if i !== length(effects) || anythingleft
                    print(buf, "\n", ind)
                end
            end
            if alwaystruefalse
                repl = compute_resolve_inline_body(p, children(p)[3], replace_expr)
                push!(resolutions, TextReplacement(dep, replace_expr.span, string(String(take!(buf)), repl)))
            else
                resolve_delete_expr(dep, resolutions, p, replace_expr, String(take!(buf)))
            end
            return
        end

        @label out
        if in_statement_position(expr)
            resolve_delete_expr(dep, resolutions, expr, expr)
        else
            push!(resolutions,
                TextReplacement(dep,
                expr.span, alwaystruefalse ? "true" : "false"))
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

function indentation(tree)
    all_leading_trivia = string(prev_node_ws(tree), leading_trivia(tree))
    # Find the amount the number characters from the last newline to the start
    # expr's span.
    lastn = rsearch(all_leading_trivia, '\n')
    all_leading_trivia[lastn+1:end]
end

"""
    Aligns new lines to the opening parenthesis of a function call's argument list
"""
function format_align_arguments(tree)

    call = function_def_call(tree)
    lparen = children(call)[2]
    nindent = mapreduce(charwidth, +, 0, tree.buffer[1 + (first(tree.span):first(lparen.span))]) +
              mapreduce(charwidth, +, 0, indentation(tree))
    ftree = format_setindent_body(call, nindent)
    rtree = replace_node(tree, call, ftree)
end

function apply_formatter(f, tree)
    rtree = f(tree)
    buf = IOBuffer()
    print_replacement(buf, rtree)
    TextReplacement(nothing, tree.span, String(take!(buf)))
end

