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
        CSTParser.declares_function(expr) || return expr
        call_idx = isexpr(expr, FunctionDef) ? 2 : 1
        call = children(expr)[call_idx]
        ret_expr = ChildReplacementNode(nothing, collect(children(expr)), expr)
        ok, call′ = format_new_call_expr(expr, orig_expr, call, children(children(orig_expr)[2])[call_idx])
        ok || return expr
        children(ret_expr)[call_idx] = call′
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
        return length(ex.args) == 2 && Base.Meta.isexpr(ex.args[1], :curly)
    end
    is_index_style(ex::Expr) = ex == :(Compat.IndexStyle) || ex == :(Base.IndexStyle) ||
    (ex.head == :(.) && (ex.args[1] == :Compat || ex.args[1] == :Base) &&
        ex.args[2] == Expr(:quote, :IndexStyle))
    is_index_style(arg) = false
    withincurly(ex) = Base.Meta.isexpr(ex, :curly) ? ex.args[1] : ex
    function _compat(min_ver, ex::Expr)
        if ex.head === :call
            f = ex.args[1]
            if min_ver < v"0.6.0-dev.826" && length(ex.args) == 3 && # julia#18510
                    istopsymbol(withincurly(ex.args[1]), :Base, :Nullable)
                return true
            end
        elseif ex.head === :curly
            f = ex.args[1]
            if min_ver < v"0.6.0-dev.2575" && any(i->isa(ex.args[i], Expr) && ex.args[i].head == Symbol("<:"), 2:length(ex.args)) #20414
                return true
            end
        elseif ex.head === :quote && isa(ex.args[1], Symbol)
            # Passthrough
            return false
        elseif min_ver < v"0.6.0-dev.2782" && new_style_typealias(ex)
            return true
        elseif min_ver < v"0.6.0-dev.2782" && ex.head === :const && length(ex.args) == 1 && new_style_typealias(ex.args[1])
            return true
        end
        if min_ver < v"0.6.0-dev.2840"
            if ex.head == :(=) && isa(ex.args[1], Expr) && ex.args[1].head == :call
                a = ex.args[1].args[1]
                if is_index_style(a)
                    return true
                elseif isa(a, Expr) && a.head == :curly
                    if is_index_style(a.args[1])
                        return true
                    end
                end
            end
        end
        if min_ver < v"0.7.0-DEV.880"
            if ex.head == :curly && ex.args[1] == :CartesianRange && length(ex.args) >= 2
                a = ex.args[2]
                if a != :CartesianIndex && !(isa(a, Expr) && a.head == :curly && a.args[1] == :CartesianIndex)
                    return true
                end
            end
        end
        return any(x->_compat(min_ver, x), ex.args)
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

        macroname = children(expr)[1]
        is_identifier(macroname, "@compat") || return
        args = filter(x->!isexpr(x, CSTParser.PUNCTUATION), children(expr)[2:end])
        length(args) == 1 || return

        if !is_at_compat_needed(dep, args[1])
            buf = IOBuffer()
            print_replacement(buf, format_result(args[1], expr), false, false)
            push!(resolutions, TextReplacement(expr.span, String(take!(buf))))
        end
    end
end
