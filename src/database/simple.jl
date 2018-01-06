begin
    struct OldStructSyntax; end
    register(OldStructSyntax, Deprecation(
        "The type-definition keywords (type, immutable, abstract) where changed in Julia 0.6",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.198",
        typemax(VersionNumber)
    ))
    # Despite having different syntax, all of these parse the same,
    # so macros can't distinguish
    applies_in_macrocall(dep::OldStructSyntax, context) = true

    # Special purpose formatter to unindent multi-line arglists
    function format_paramlist(orig_tree, tree, matches)
        if isexpr(tree, CSTParser.Struct)
            isexpr(children(tree)[2], CSTParser.Curly) || return tree
            children(tree)[2] = format_addindent_body(children(tree)[2], -3, nothing)
        else
            isexpr(children(tree)[3], CSTParser.Curly) || return tree
            children(tree)[3] = format_addindent_body(children(tree)[3], 10, nothing)
        end
        tree
    end

    match(OldStructSyntax,
        "immutable \$name\n\$BODY...\nend",
        "struct\$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldStructSyntax,
        "type \$name\n\$BODY...\nend",
        "mutable struct\$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldStructSyntax,
        "abstract \$name",
        "abstract type\$name end"
    )
    match(OldStructSyntax,
        "bitstype \$size \$name",
        "primitive type\$name \$size! end"
    )
end

begin
    struct OldTypeAliasSyntax; end
    register(OldTypeAliasSyntax, Deprecation(
        "The `typealias` keyword is deprecated in Julia 0.6",
        "julia",
        v"0.6.0",
        v"0.6.0",
        typemax(VersionNumber)
    ))

    match(OldTypeAliasSyntax,
        "typealias \$X{\$T...} \$B",
        "\$X{\$T...} = \$B"
    )
    match(OldTypeAliasSyntax,
        "typealias \$A \$B",
        "const\$A=\$B"
    )
end

begin
    struct OldStyleConstructor
        # In 0.6/0.7 F{T}() still means a parametric function
        # definition, so we can't perform this upgrade, if there's
        # no `where` clause.
        non_where_curly::Bool
        # In 0.6/0.7 F() inside a struct still refers to the old inner constructor syntax
        # so we can't perform this upgrade
        inner_constructor::Bool
    end
    OldStyleConstructor() = OldStyleConstructor(false, false)

    function dep_for_vers(::Type{OldStyleConstructor}, vers)
        is10 = all(interval->(v"1.0-DEV" <= interval.lower), vers["julia"].intervals)
        OldStyleConstructor(is10, is10)
    end

    register(OldStyleConstructor, Deprecation(
        "This constructor syntax is no longer required",
        "julia",
        v"0.6.0", v"0.6.0", typemax(VersionNumber)
    ))
    function format_old_constructor(orig_tree, tree, matches)
        ok, call′ = format_new_call_expr(tree, orig_tree, tree, orig_tree)
        ok || return tree
        return call′
    end
    function filter_non_where_curly(dep, tree, matches)
        dep.non_where_curly && return true
        isexpr(parent(tree), BinarySyntaxOpCall) || isexpr(parent(tree), CSTParser.WhereOpCall) || return false
        isexpr(children(parent(tree))[2], OPERATOR, Tokens.WHERE) || return false
        return true
    end
    match(OldStyleConstructor,
        "(::Type{\$NAME{\$T...}})(\$ARGS...)",
        "\$NAME{\$T...}(\$ARGS...)",
        format_old_constructor,
        filter = filter_non_where_curly
    )
    function filter_in_struct(dep, tree, matches)
        dep.inner_constructor && return true
        return get_struct_parent(tree.parent) === nothing
    end
    function filter_params(dep, tree, matches)
        # Handled by the above
        name = first(matches[:NAME][2])
        isexpr(name, Curly) && return false
        p = parent(tree)
        isexpr(p, BinarySyntaxOpCall)  || isexpr(p, CSTParser.WhereOpCall)  || return true
        isexpr(children(p)[2], OPERATOR, Tokens.WHERE) || return true
        # Get all the parameter names
        names = extract_identifiers(children(p)[3:end])
        !(Expr(name) in names)
    end
    match(OldStyleConstructor,
        "(::Type{\$NAME})(\$ARGS...)",
        "\$NAME(\$ARGS...)",
        format_old_constructor,
        filter = (args...)->filter_params(args...) && filter_in_struct(args...)
    )
end

begin
    struct ObsoleteCompatString; end
    register(ObsoleteCompatString, Deprecation(
        "This Compat definition is deprecated",
        "julia",
        v"0.5.0", v"0.6.0", typemax(VersionNumber)
    ))

    match(ObsoleteCompatString,
        "Compat.UTF8String",
        "String",
    )

    match(ObsoleteCompatString,
        "Compat.ASCIIString",
        "String",
    )
end

begin
    struct ConditionalWhitespace; end
    register(ConditionalWhitespace, Deprecation(
        "The ternary operator now requires whitespace on both sides of the punctuation.",
        "julia",
        v"0.4.0", v"0.7.0-DEV.797", typemax(VersionNumber)
    ))

    function add_ws_lead_trail!(ret, idx, exprs)
        any = false
        expr = exprs[2]
        allws = string(trailing_ws(exprs[1]), leading_ws(expr))
        if isempty(allws) || !isspace(allws[end])
            expr = children(ret)[idx] = TriviaReplacementNode(ret, expr, string(leading_ws(expr), " "), trailing_ws(expr))
            any = true
        end
        allws = string(trailing_ws(expr), leading_ws(exprs[3]))
        if isempty(allws) || !isspace(allws[end])
            expr = children(ret)[idx] = TriviaReplacementNode(ret, expr, leading_ws(expr), string(" ", trailing_ws(expr)))
            any = true
        end
        any
    end

    match(ConditionalWhitespace, CSTParser.ConditionalOpCall) do x
        dep, expr, resolutions, context = x
        ret = ChildReplacementNode(nothing, collect(children(expr)), expr)
        changed = add_ws_lead_trail!(ret, 2, children(expr)[1:3])
        changed |= add_ws_lead_trail!(ret, 4, children(expr)[3:5])
        if changed
            buf = IOBuffer()
            print_replacement(buf, ret, false, false)
            push!(resolutions, TextReplacement(expr.span, String(take!(buf))))
        end
    end
end

begin
    struct GeneratorWhitespace; end
    register(GeneratorWhitespace, Deprecation(
        "Generators and comprehensions now require whitespace before the `for`",
        "julia",
        v"0.4.0", v"0.7.0-DEV.797", typemax(VersionNumber)
    ))

    match(GeneratorWhitespace, CSTParser.Generator) do x
        dep, expr, resolutions, context = x
        ret = ChildReplacementNode(nothing, collect(children(expr)), expr)
        body, fornode, iterand = children(expr)
        allws = string(trailing_ws(body), leading_ws(fornode))
        if isempty(allws) || !isspace(allws[end])
            children(ret)[2] = TriviaReplacementNode(ret, fornode, string(leading_ws(fornode), " "), trailing_ws(fornode))
            buf = IOBuffer()
            print_replacement(buf, ret, false, false)
            repl = String(take!(buf))
            push!(resolutions, TextReplacement(expr.span, repl))
        end
    end
end

begin
    struct TupleSplat; end

    register(TupleSplat, Deprecation(
        "Splat of single value in tuple needs trailing comma",
        "julia",
        v"0.7.0-DEV.2559", v"1.0", typemax(VersionNumber)
    ))

    function filter_funcdef_and_multiarg(dep, expr, matches)
        length(children(expr)) == 3     || return false  # 3 = '(' + ')' + 1 arg
        CSTParser.has_sig(parent(expr)) && return false
        return true
    end

    match(TupleSplat,
          "(\$ID...)",
          "(\$ID...,)",
          filter = filter_funcdef_and_multiarg
    )

end
