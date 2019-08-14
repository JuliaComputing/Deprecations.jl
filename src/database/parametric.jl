# Special purpose formatter to unindent multi-line arglists
function format_arglist!(wheren, nchars_moved, lcurly_pos = 0, body_indent = 4)
    # We replace the call argument
    ocall = children(wheren)[1]
    (length(children(ocall)) == 2) && return
    firstarg = children(ocall)[3]
    # Heuristic: If the first argument is on a new line, don't try to change the indentation
    if ('\n' in leading_trivia(firstarg))
        return
    end
    # Heuristic: If the arugment indents (more spefically the first indented line), is further
    # left than the original lbrace, don't unindent, it's unlikely they were aligned to the
    # lparen.
    if lcurly_pos != 0
        indents = countindent_body(ocall)
        if !isempty(indents) && (indents[1] < lcurly_pos)
            return
        end
        # Heuristic: If this formatting would the argument list past the indent
        # of the body (or 4 spaces if there is no body provided), don't perform
        # the unindent
        if any(x->(x + nchars_moved) < body_indent, indents)
            return
        end
    end
    children(wheren)[1] = format_addindent_body(ocall, nchars_moved, nothing)
    nothing
end

function format_shortfunc_body!(tree, funcdef, nchars_moved)
    # Heurisitc: In short format, it is possible that people aligned after the equal
    # sign. If the whole body is after the equal sign, indent it.
    body = children(funcdef)[3]
    indents = Int[]
    countindent_body(body, indents)
    while length(indents) != 0 && indents[end] == 0
        pop!(indents)
    end
    eq_sign_offset = line_pos(funcdef, first(children(funcdef)[2].span))
    if length(indents) != 0 && all(indents .> eq_sign_offset)
        new_body = format_addindent_body(body, textwidth(" where "), parent(body))
        children(tree)[3] = new_body
    end
    nothing
end

CSTParser.defines_function(node::OverlayNode) = CSTParser.defines_function(node.expr)
CSTParser.has_sig(node::OverlayNode) = CSTParser.has_sig(node.expr)
function CSTParser.get_id(x::OverlayNode{BinarySyntaxOpCall})
    if isexpr(children(x)[2], OPERATOR ,Tokens.ISSUBTYPE) || isexpr(children(x)[2], OPERATOR, Tokens.DECLARATION)
        return CSTParser.get_id(children(x)[1])
    else
        return x
    end
end

CSTParser.get_id(x::OverlayNode{Curly}) = CSTParser.get_id(children(x)[1])

function compute_new_line_pos(tree, node, offset = 0, consider_trailing=true)
    if isempty(children(tree))
        if consider_trailing
            ws = trailing_trivia(tree)
            if '\n' in ws
                return length(last_line(ws))
            end
        end
        x = sprint(print_replacement, tree, true, consider_trailing)
        return offset + length(x)
    end
    for (i,c) in enumerate(children(tree))
        if c == node
            return offset
        end
        is_last = i == length(children(tree))
        offset = compute_new_line_pos(c, node, offset, !isa(c, TriviaReplacementNode) &&
            !(is_last && !consider_trailing))
        if isa(c, TriviaReplacementNode) && !(!consider_trailing && is_last)
            ws = trailing_trivia(c)
            if '\n' in ws
                offset = length(last_line(ws))
            end
        end
    end
    offset
end

begin
    struct OldParametricSyntax; end
    register(OldParametricSyntax, Deprecation(
        "Parameteric syntax of the form f{T}(x::T) is deprecated and needs to be written using the `where` keyword",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.1143",
        typemax(VersionNumber)
    ))
    applies_in_macrocall(dep::OldParametricSyntax, context) = true


    function get_struct_parent(expr)
        expr.parent == nothing && return nothing
        (isexpr(expr.parent, CSTParser.Struct) ||
            isexpr(expr.parent, CSTParser.Mutable)) && return expr.parent
        # Nested function are ok
        CSTParser.defines_function(expr.parent) && return nothing
        return get_struct_parent(expr.parent)
    end

    function struct_name(expr)
        i = 1
        while isexpr(children(expr)[i], KEYWORD)
            i += 1
        end
        children(expr)[i]
    end

    function extract_identifiers(exprs)
        identifiers = Symbol[]
        for expr in exprs
            isexpr(expr, PUNCTUATION) && continue
            id = CSTParser.get_id(expr)
            @assert isexpr(id, IDENTIFIER)
            push!(identifiers, Expr(id.expr))
        end
        identifiers
    end

    function deconflict_identifiers(exprs, conflicts)
        new_exprs = map(exprs) do expr
            isexpr(expr, PUNCTUATION) && return expr
            id_expr = CSTParser.get_id(expr)
            orig_id = id = Expr(id_expr.expr)
            @assert isa(id, Symbol)
            while id in conflicts
                id = Symbol(string(id,"_"))
            end
            id == orig_id && return expr
            return replace_node(expr, id_expr, ReplacementNode(String(id), leading_trivia(id_expr), trailing_trivia(id_expr)))
        end
        new_exprs
    end

    function rewrite_param_syntax(dep, expr, resolutions)
        CSTParser.defines_function(expr) || return
        sp = get_struct_parent(expr)
        # If there's already a where expr, this is new syntax
        call = isexpr(expr, FunctionDef) ? children(expr)[2] : children(expr)[1]
        had_rt_annotation = false
        call_or_rt_expr = call
        if isexpr(call, BinarySyntaxOpCall) && isexpr(children(call)[2],
                CSTParser.OPERATOR, Tokens.DECLARATION)
            call_or_rt_expr = rt_expr = call
            call = children(call)[1]
            had_rt_annotation = true
        end
        is_where_expr(call) && return
        length(children(call)) == 0 && return
        had_curly = isexpr(children(call)[1], Curly)
        tparams = []
        if had_curly
            curly = children(call)[1]
            fname = children(curly)[1]
            # Include punctuation
            tparams = children(curly)[2:end]
        else
            fname = children(call)[1]
        end
        needs_new_curly = false
        if sp !== nothing
            needs_new_curly = isexpr(struct_name(sp), Curly) && (isexpr(fname, IDENTIFIER) && Expr(fname.expr) == Expr(children(struct_name(sp))[1].expr))
            if needs_new_curly
                # Includes puctuation
                new_curlies = struct_name(sp)[2:end]
                if isempty(tparams)
                    tparams = new_curlies
                else
                    new_curlies = deconflict_identifiers(new_curlies, extract_identifiers(tparams))
                    tparams = [tparams[1]; new_curlies[2:end-1]; ReplacementNode(",",""," "); tparams[2:end]]
                end
            end
        end
        if !had_curly && !needs_new_curly
            return
        end
        # In long form syntax, with only one parameter, strip the curly braces
        if isexpr(expr, FunctionDef) && length(tparams) == 3
            tparams = tparams[2:2]
        end
        new_tree = ChildReplacementNode(nothing, children(expr)[(isexpr(expr, FunctionDef) ? 3 : 2):end], expr)
        replace_op = CSTParser.OPERATOR(0, 0:1, Tokens.ERROR, false)
        replace_lit = CSTParser.LITERAL(0, 0:1, "", Tokens.ERROR)
        new_where = TriviaReplacementNode(new_tree, ChildReplacementNode(new_tree,
            [ReplacementNode("where"," "," "), tparams...], CSTParser.BinarySyntaxOpCall(replace_lit, replace_op, replace_lit)),
            "", trailing_trivia(call_or_rt_expr))
        pushfirst!(children(new_tree), new_where)
        call_parent = new_where
        isexpr(expr, FunctionDef) && pushfirst!(children(new_tree), children(expr)[1])
        if had_rt_annotation
            annotation = children(rt_expr)[end]
            call_parent = ChildReplacementNode(new_tree, [children(rt_expr)[2]], rt_expr)
            push!(children(call_parent), TriviaReplacementNode(call_parent, annotation, leading_trivia(annotation), ""))
            pushfirst!(children(new_where), call_parent)
        end
        new_call = TriviaReplacementNode(new_where, ChildReplacementNode(new_where, children(call)[2:end], call), "", "")
        pushfirst!(children(call_parent), new_call)
        if needs_new_curly
            new_curly = TriviaReplacementNode(new_call, ChildReplacementNode(new_call, [fname, new_curlies...], had_curly ? Curly : EXPR{Curly}(Expr[], 0, 0:1)),"","")
            pushfirst!(children(new_call), new_curly)
        else
            pushfirst!(children(new_call), fname)
        end
        nchars_moved = (needs_new_curly ? sum(expr->sum(textwidth, fullspan_text(expr)), children(new_curly)[2:end]) : 0) -
                       (had_curly ? line_pos(call, first(children(call)[2].span)) - (line_pos(curly, first(children(curly)[2].span))) : 0)
        heuristic_pos = had_curly ? line_pos(curly, first(children(curly)[2].span)) : line_pos(call, first(children(call)[2].span)-1)
        format_arglist!(new_where, nchars_moved, heuristic_pos, isexpr(expr, FunctionDef) ? 4 : 0)
        # If the parameter list is multi-line, it might need to be indented as well
        if had_curly
            new_lbrace_pos = compute_new_line_pos(new_where, children(new_where)[3])
            old_lbrace_pos = line_pos(curly, first(children(curly)[2].span))
            where_indent = new_lbrace_pos - old_lbrace_pos
            for i = 4:length(children(new_where))
                children(new_where)[i] = format_addindent_body(children(new_where)[i], where_indent, nothing)
            end
        end
        if !isexpr(expr, FunctionDef)
            format_shortfunc_body!(new_tree, expr, nchars_moved)
        end
        buf = IOBuffer()
        print_replacement(buf, new_tree, false, false)
        push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
    end

    match(OldParametricSyntax, CSTParser.FunctionDef) do x
        dep, expr, resolutions, context = x
        rewrite_param_syntax(dep, expr, resolutions)
    end

    match(OldParametricSyntax, CSTParser.BinarySyntaxOpCall) do x
        dep, expr, resolutions, context = x
        rewrite_param_syntax(dep, expr, resolutions)
    end
end
