# Special purpose formatter to unindent multi-line arglists
function format_arglist!(wheren, nchars_moved, lcurly_pos = 0)
    # We replace the call argument
    ocall = children(wheren)[1]
    (length(children(ocall)) == 2) && return
    firstarg = children(ocall)[3]
    # Heuristic: If the first argument is on a new line, don't try to change the indentation
    if ('\n' in leading_ws(firstarg))
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
    end
    children(wheren)[1] = format_addindent_body(ocall, nchars_moved, nothing)
    nothing
end

CSTParser.declares_function(node::OverlayNode) = CSTParser.declares_function(node.expr)
function CSTParser.get_id(x::OverlayNode{BinarySyntaxOpCall})
    if isexpr(children(x)[2], OPERATOR{ComparisonOp,Tokens.ISSUBTYPE,false}) || isexpr(children(x)[2], OPERATOR{DeclarationOp,Tokens.DECLARATION,false}) || isexpr(children(x)[2], EXPR{OPERATOR{WhereOp,Tokens.WHERE,false}})
        return CSTParser.get_id(children(x)[1])
    else
        return x
    end
end

CSTParser.get_id(x::OverlayNode{Curly}) = CSTParser.get_id(children(x)[1])

begin
    struct OldParametricSyntax; end
    register(OldParametricSyntax, Deprecation(
        "Parameteric syntax of the form f{T}(x::T) is deprecated and needs to be written using the `where` keyword",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.1143",
        typemax(VersionNumber)
    ))
    function is_where_expr(expr)
        isexpr(expr, BinaryOpCall) || return false
        isexpr(children(expr)[2], OPERATOR{15, Tokens.WHERE, false}) || return false
        return true
    end

    function get_struct_parent(expr)
        expr.parent == nothing && return nothing
        (isexpr(expr.parent, CSTParser.Struct) ||
            isexpr(expr.parent, CSTParser.Mutable)) && return expr.parent
        # Nested function are ok
        CSTParser.declares_function(expr.parent) && return nothing
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
            return replace_node(expr, id_expr, ReplacementNode{IDENTIFIER}(String(id), leading_ws(id_expr), trailing_ws(id_expr)))
        end
        new_exprs
    end

    function rewrite_param_syntax(expr, resolutions)
        CSTParser.declares_function(expr) || return
        sp = get_struct_parent(expr)
        # If there's already a where expr, this is new syntax
        is_where_expr(children(expr)[1]) && return
        call = isexpr(expr, FunctionDef) ? children(expr)[2] : children(expr)[1]
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
            needs_new_curly = isexpr(struct_name(sp), Curly)
            if needs_new_curly
                # Includes puctuation
                new_curlies = struct_name(sp)[2:end]
                if isempty(tparams)
                    tparams = new_curlies
                else
                    new_curlies = deconflict_identifiers(new_curlies, extract_identifiers(tparams))
                    tparams = [tparams[1]; new_curlies[2:end-1]; ReplacementNode{PUNCTUATION{Tokens.COMMA}}(",",""," "); tparams[2:end]]
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
        new_where = TriviaReplacementNode(new_tree, ChildReplacementNode(new_tree,
            [ReplacementNode{OPERATOR{15,Tokens.WHERE,false}}("where"," "," "), tparams...], EXPR{CSTParser.BinarySyntaxOpCall}(EXPR[],"")),
            "", trailing_ws(call))
        unshift!(children(new_tree), new_where)
        isexpr(expr, FunctionDef) && unshift!(children(new_tree), children(expr)[1])
        new_call = TriviaReplacementNode(new_where, ChildReplacementNode(new_where, children(call)[2:end], call), "", "")
        unshift!(children(new_where), new_call)
        if needs_new_curly
            new_curly = TriviaReplacementNode(new_call, ChildReplacementNode(new_call, [fname, new_curlies...], had_curly ? Curly : EXPR{Curly}(Expr[], "")),"","")
            unshift!(children(new_call), new_curly)
        else
            unshift!(children(new_call), fname)
        end
        nchars_moved = (needs_new_curly ? sum(expr->sum(charwidth, fullspan_text(expr)), children(new_curly)[2:end]) : 0) -
                       (had_curly ? sum(expr->sum(charwidth, fullspan_text(expr)), children(curly)[2:end]) : 0)
        heuristic_pos = had_curly ? line_pos(curly, first(curly.span)) : line_pos(call, first(children(call)[2].span)-1)
        format_arglist!(new_where, nchars_moved, heuristic_pos)
        buf = IOBuffer()
        print_replacement(buf, new_tree, false, false)
        push!(resolutions, TextReplacement(expr.span, String(take!(buf))))
    end

    match(OldParametricSyntax, CSTParser.FunctionDef) do x
        dep, expr, resolutions, context = x
        rewrite_param_syntax(expr, resolutions)
    end

    match(OldParametricSyntax, CSTParser.BinarySyntaxOpCall) do x
        dep, expr, resolutions, context = x
        rewrite_param_syntax(expr, resolutions)
    end

end
