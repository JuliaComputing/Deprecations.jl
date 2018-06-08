begin
    function format_new_call_expr(expr, orig_expr, call, orig_call)
        lparen_pos = line_pos(orig_call, first(children(orig_call)[2].span))
        # Heuristic: If the arugment indents (more spefically the first indented line), is further
        # left than the original lparen, don't unindent, it's unlikely they were aligned.
        if lparen_pos != 0
            indents = countindent_body(orig_expr)
            if !isempty(indents) && (indents[1] < lparen_pos)
                return (false, call)
            end
        end
        # A bit cheesy, but w/e
        buf = IOBuffer()
        print_replacement(buf, expr, false, false)
        x = String(take!(buf))
        nx = sizeof(x)-length(orig_expr.span)
        return true, format_addindent_body(call, nx, nothing, false)
    end

    # Special purpose formatter for `@compat` results
    function format_result(expr, orig_expr)
        CSTParser.defines_function(expr) || return expr
        call_idx = isexpr(expr, FunctionDef) ? 2 : 1
        callorwhere = children(expr)[call_idx]
        call = is_where_expr(callorwhere) ? children(callorwhere)[1] : callorwhere
        ret_expr = ChildReplacementNode(nothing, collect(children(expr)), expr)
        orig_call = children(children(orig_expr)[2])[call_idx]
        orig_call = is_where_expr(orig_call) ? children(orig_call)[1] : orig_call
        ok, call′ = format_new_call_expr(expr, orig_expr, call, orig_call)
        ok || return expr
        if is_where_expr(callorwhere)
            children(ret_expr)[call_idx] = ChildReplacementNode(ret_expr, collect(children(callorwhere)), callorwhere)
            children(children(ret_expr)[call_idx])[1] = call′
        else
            children(ret_expr)[call_idx] = call′
        end
        ret_expr
    end

    struct ObsoleteCompatMacro
        min_ver::VersionNumber
    end
    register(ObsoleteCompatMacro, Deprecation(
        "This compat macro is no longer required",
        "julia",
        v"0.5.0", v"0.5.0", typemax(VersionNumber)
    ))

    ObsoleteCompatMacro() = ObsoleteCompatMacro(v"0.6.0")
    function dep_for_vers(::Type{ObsoleteCompatMacro}, vers)
        ObsoleteCompatMacro(minimum(map(interval->interval.lower, vers["julia"].intervals)))
    end


    # This follows the structure of the _compat function from Compat.jl
    # to determine if the Compat macro would have done something
    istopsymbol(ex, mod, sym) = ex in (sym, Expr(:(.), mod, Expr(:quote, sym)))
    _compat(min_ver, ex) = false
    function new_style_typealias(ex::ANY)
        Base.Meta.isexpr(ex, :(=)) || return false
        ex = ex::Expr
        return length(children(ex)) == 2 && Base.Meta.isexpr(children(ex)[1], :curly)
    end
    is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (children(ex)[1] == :Compat || children(ex)[1] == :Base) &&
        children(ex)[2] == Expr(:quote, :IndexStyle))
    is_index_style(arg) = false
    withincurly(ex) = Base.Meta.isexpr(ex, :curly) ? children(ex)[1] : ex
    function _compat(min_ver, ex::Expr)
        if ex.head === :call
            f = children(ex)[1]
            if min_ver < v"0.6.0-dev.826" && length(children(ex)) == 3 && # julia#18510
                    istopsymbol(withincurly(children(ex)[1]), :Base, :Nullable)
                return true
            end
        elseif ex.head === :curly
            f = children(ex)[1]
            if min_ver < v"0.6.0-dev.2575" && any(i->isa(children(ex)[i], Expr) && children(ex)[i].head == Symbol("<:"), 2:length(children(ex))) #20414
                return true
            end
        elseif ex.head === :quote && isa(children(ex)[1], Symbol)
            # Passthrough
            return false
        elseif min_ver < v"0.6.0-dev.2782" && new_style_typealias(ex)
            return true
        elseif min_ver < v"0.6.0-dev.2782" && ex.head === :const && length(children(ex)) == 1 && new_style_typealias(children(ex)[1])
            return true
        end
        if min_ver < v"0.6.0-dev.2840"
            if ex.head == :(=) && isa(children(ex)[1], Expr) && children(ex)[1].head == :call
                a = children(children(ex)[1])[1]
                if is_index_style(a)
                    return true
                elseif isa(a, Expr) && a.head == :curly
                    if is_index_style(children(a)[1])
                        return true
                    end
                end
            end
        end
        if min_ver < v"0.7.0-DEV.880"
            if ex.head == :curly && children(ex)[1] == :CartesianRange && length(children(ex)) >= 2
                a = children(ex)[2]
                if a != :CartesianIndex && !(isa(a, Expr) && a.head == :curly && children(a)[1] == :CartesianIndex)
                    return true
                end
            end
        end
        if min_ver < v"0.7.0-DEV.2562"
            if ex.head == :call && children(ex)[1] == :finalizer
                return true
            end
        end
        return any(x->_compat(min_ver, x), children(ex))
    end

    function is_at_compat_needed(dep, expr)
        if isexpr(expr, CSTParser.Primitive) || isexpr(expr, CSTParser.Abstract)
            return dep.min_ver < v"0.6.0-dev.2746"
        else
            _compat(dep.min_ver, Expr(expr.expr))
        end
    end

    match(ObsoleteCompatMacro, CSTParser.MacroCall) do x
        dep, expr, resolutions, context = x

        is_macroname(expr, "compat") || return
        args = filter(x->!isexpr(x, CSTParser.PUNCTUATION), children(expr)[2:end])
        length(args) == 1 || return

        if !is_at_compat_needed(dep, args[1])
            buf = IOBuffer()
            print_replacement(buf, format_result(args[1], expr), false, false)
            push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
        end
    end

    struct ObsoleteCompatGetfield
        min_ver::VersionNumber
    end
    register(ObsoleteCompatGetfield, Deprecation(
        "This compat getfield is no longer required",
        "julia",
        v"0.5.0", v"0.5.0", typemax(VersionNumber)
    ))
    ObsoleteCompatGetfield() = ObsoleteCompatGetfield(v"0.6.0")
    function dep_for_vers(::Type{ObsoleteCompatGetfield}, vers)
        ObsoleteCompatGetfield(minimum(map(interval->interval.lower, vers["julia"].intervals)))
    end

    function should_replace(dep, refed)
        if (is_identifier(refed, "Test") ||
            is_identifier(refed, "Mmap") ||
            is_identifier(refed, "DelimitedFiles") ||
            is_identifier(refed, "SharedArrays")) && dep.min_ver > v"0.7.0-DEV.2005"
        elseif is_identifier(refed, "Dates") && dep.min_ver > v"0.7.0-DEV.2575"
        elseif is_identifier(refed, "Libdl") && dep.min_ver > v"0.7.0-DEV.3382"
        elseif is_identifier(refed, "Printf") && dep.min_ver > v"0.7.0-DEV.3052"
        else
            return false
        end
        return true
    end

    function process_compat_getfield(x)
        dep, expr, resolutions, context = x

        isexpr(children(expr)[2], OPERATOR, Tokens.DOT) || return

        is_identifier(children(expr)[1], "Compat") || return

        refed = children(expr)[3]
        should_replace(dep, refed) || return

        buf = IOBuffer()
        print_replacement(buf, format_result(args[1], expr), false, false)
        push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
    end

    function process_compat_using(x)
        dep, expr, resolutions, context = x
        i = 2
        while i < length(children(expr))
            id = children(expr)[i]
            i += 1
            if is_identifier(id, "Compat")
                if !isexpr(children(expr)[i], PUNCTUATION, Tokens.DOT)
                    i += 1
                    continue
                end
                i += 1
                rid = children(expr)[i]
                if should_replace(dep, rid)
                    buf = IOBuffer()
                    print_replacement(buf, rid, false, false)
                    push!(resolutions, TextReplacement(dep,
                        first(id.span):last(rid.span), String(take!(buf))))
                end
            end
            while i < length(children(expr)) && !isexpr(children(expr)[i], PUNCTUATION, Tokens.COMMA)
                i += 1
            end
            i += 1
        end
    end

    match(ObsoleteCompatGetfield, CSTParser.BinarySyntaxOpCall) do x
        process_compat_getfield(x)
    end

    match(ObsoleteCompatGetfield, CSTParser.Using) do x
        process_compat_using(x)
    end
end
