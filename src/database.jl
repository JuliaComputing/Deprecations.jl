using Deprecations: Deprecation
using Compat
using CSTParser: FunctionDef

begin
    struct OldParametricSyntax; end
    register(OldParametricSyntax, Deprecation(
        "Parameteric syntax of the form f{T}(x::T) is deprecated and needs to be written using the `where` keyword",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.1143",
        typemax(VersionNumber)
    ))

    # Special purpose formatter to unindent multi-line arglists
    function format_arglist(tree, matches)
        nchars_moved = sum(match->sum(charwidth, fullspan_text(match)),matches[:T][2])+2
        wheren = isexpr(tree, FunctionDef) ? children(tree)[2] : children(tree)[1]
        # We replace the call argument
        ocall = children(wheren)[1]
        (length(children(ocall)) == 2) && return
        firstarg = children(ocall)[3]
        # If the first argument is on a new line, don't try to change the indentation
        if ('\n' in leading_ws(firstarg))
            return tree
        end
        children(wheren)[1] = format_addindent_body(ocall, -nchars_moved, nothing)
        tree
    end

    match(OldParametricSyntax,
        "function \$F{\$T}(\$ARGS...)\n\$BODY...\nend",
        "function\$F(\$ARGS...) where \$T\n\$BODY!...\nend",
        format_arglist
    )
    match(OldParametricSyntax,
        "function \$F{\$T...}(\$ARGS...)\n\$BODY...\nend",
        "function\$F(\$ARGS...) where {\$T...}\n\$BODY!...\nend",
        format_arglist
    )
    match(OldParametricSyntax,
        "\$F{\$T...}(\$ARGS...) = \$BODY",
        "\$F(\$ARGS...) where {\$T...} =\$BODY",
        format_arglist
    )
end

begin
    struct OldStructSyntax; end
    register(OldStructSyntax, Deprecation(
        "The type-definition keywords (type, immutable, abstract) where changed in Julia 0.6",
        "julia",
        v"0.6.0",
        v"0.7.0-DEV.198",
        typemax(VersionNumber)
    ))

    # Special purpose formatter to unindent multi-line arglists
    function format_paramlist(tree, matches)
        if isexpr(tree, CSTParser.Struct)
            isexpr(children(tree)[2], CSTParser.Curly) || return tree
            children(tree)[2] = format_addindent_body(children(tree)[2], -3, nothing)
        else
            isexpr(children(tree)[3], CSTParser.Curly) || return tree
            children(tree)[3] = format_addindent_body(children(tree)[3], 10, nothing)
        end
        tree
    end

    match(OldParametricSyntax,
        "immutable \$name\n\$BODY...\nend",
        "struct\$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldParametricSyntax,
        "type \$name\n\$BODY...\nend",
        "mutable struct\$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldParametricSyntax,
        "abstract \$name",
        "abstract type\$name end"
    )
    match(OldParametricSyntax,
        "bitstype \$name \$size",
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
        "const \$A = \$B"
    )
end

using Tokenize.Tokens: GREATER, LESS, GREATER_EQ, GREATER_THAN_OR_EQUAL_TO, LESS_EQ, LESS_THAN_OR_EQUAL_TO
begin
    struct ObsoleteVersionCheck; vers; end
    register(ObsoleteVersionCheck, Deprecation(
        "This version check is for a version of julia that is no longer supported by this package",
        "julia",
        typemin(VersionNumber), typemin(VersionNumber), typemax(VersionNumber)
    ))
    ObsoleteVersionCheck() = ObsoleteVersionCheck(Pkg.Reqs.parse(IOBuffer("julia 0.6"))["julia"])

    const comparisons = Dict(
         GREATER                  => >,
         LESS                     => <,
         GREATER_EQ               => >=,
         GREATER_THAN_OR_EQUAL_TO => ≥,
         LESS_EQ                  => <=,
         LESS_THAN_OR_EQUAL_TO    => ≤,
         #= TODO:
         EQEQ                     => ==,
         EQEQEQ                   => ===,
         IDENTICAL_TO             => ≡,
         NOT_EQ                   => !=,
         NOT_EQUAL_TO             => ≠,
         NOT_IS                   => !==
         =#)


    function detect_ver_arguments(VERSION_arg, v_arg)
        isa(VERSION_arg, EXPR{CSTParser.IDENTIFIER}) || return nothing
        VERSION_arg.val == "VERSION" || return nothing
        isa(v_arg, EXPR{CSTParser.x_Str}) || return nothing
        isa(v_arg.args[1], EXPR{CSTParser.IDENTIFIER}) || return nothing
        isa(v_arg.args[2], EXPR{CSTParser.LITERAL{Tokens.STRING}}) || return nothing
        v_arg.args[1].val == "v" || return nothing
        VersionNumber(v_arg.args[2].val)
    end

    function dep_for_vers(::Type{ObsoleteVersionCheck}, vers)
        ObsoleteVersionCheck(vers["julia"])
    end

    function is_identifier(x::EXPR, id)
        isa(x, EXPR{IDENTIFIER}) || return false
        x.val == id
    end
    is_identifier(x::OverlayNode, id) = is_identifier(x.expr, id)

    opcode(x::EXPR{CSTParser.OPERATOR{6,op,false}}) where {op} = op

    match(ObsoleteVersionCheck, CSTParser.If) do x
        dep, expr, resolutions, context = x
        replace_expr = expr
        comparison = children(expr)[2]
        isexpr(comparison, CSTParser.BinaryOpCall) || return
        isexpr(children(comparison)[2], CSTParser.OPERATOR{6,op,false} where op) || return
        comparison = comparison.expr
        opc = opcode(comparison.args[2])
        haskey(comparisons, opc) || return
        # Also applies in @static context, but not necessarily in other macro contexts
        if context.in_macrocall
            context.top_macrocall == parent(expr) || return
            is_identifier(children(context.top_macrocall)[1], "@static") || return
            replace_expr = context.top_macrocall
        end
        r1 = detect_ver_arguments(comparison.args[1], comparison.args[3])
        if r1 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r1) && f(interval.upper, r1)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r1) && !f(interval.upper, r1)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr, replace_expr)
            return
        end
        r2 = detect_ver_arguments(comparison.args[3], comparison.args[1])
        if r2 !== nothing
            f = comparisons[opc]
            alwaystrue = all(interval->(f(interval.lower, r2) && f(interval.upper, r2)), dep.vers.intervals)
            alwaysfalse = all(interval->(!f(interval.lower, r2) && !f(interval.upper, r2)), dep.vers.intervals)
            @assert !(alwaystrue && alwaysfalse)
            alwaystrue && resolve_inline_body(resolutions, expr, replace_expr)
            alwaysfalse && resolve_delete_expr(resolutions, expr, replace_expr)
        end
    end
end

begin
    struct ObsoleteCompatMacro
        min_ver::VersionNumber
    end
    register(ObsoleteCompatMacro, Deprecation(
        "This compat macro is no longer required",
        "julia",
        v"0.4.0", v"0.4.0", typemax(VersionNumber)
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
            if min_ver < v"0.6.0-dev.2575" && any(i->ex.args[i].head == Symbol("<:"), 2:length(ex.args)) #20414
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
        if dep.min_ver >= v"0.6.0-dev.2746" &&
                (isexpr(expr, CSTParser.Primitive) || isexpr(expr, CSTParser.Abstract))
            return false
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
            print_replacement(buf, args[1], true, true)
            push!(resolutions, TextReplacement(expr.fullspan, String(take!(buf))))
        end
    end
end

begin
    struct OldStyleConstructor; end
    register(OldStyleConstructor, Deprecation(
        "This constructor syntax is no longer required",
        "julia",
        v"0.6.0", v"0.6.0", typemax(VersionNumber)
    ))
    match(OldStyleConstructor,
        "(::Type{\$NAME{\$T...}})(\$ARGS...)",
        "\$NAME{\$T...}(\$ARGS...)"
    )
    match(OldStyleConstructor,
        "(::Type{\$NAME})(\$ARGS...)",
        "\$NAME(\$ARGS...)"
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


#=
begin
    OneParamWrite = Deprecation(
        "An IO argument is now always required when calling the write function.",
        "julia",
        v"0.4.0",
        v"0.5.0-dev+5066",
        v"0.7.0"
    )

    call_match(Base, :write,
        match"write($X)",
        match"write(STDOUT, $X)"
    )
end


begin
    Delete!Env = Deprecation(
        "`delete!(ENV, k, def)` should be replaced with `pop!(ENV, k, def)`. Be aware that `pop!` returns `k` or `def`, while `delete!` returns `ENV` or `def`."
    )

    call_match(Delete!Env, Base, :delete!,
        match"delete!(::EnvHash, $B, $C)"statement,
        match"pop!($A, $B, $C)"
    )

    call_match(Delete!Env, Base, :delete!,
        match"delete!(::EnvHash, $B, $C)"
    )
end
=#
