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
    function filter_non_where_curly(S, dep, tree, matches)
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
    function filter_in_struct(S, dep, tree, matches)
        dep.inner_constructor && return true
        return get_struct_parent(tree.parent) === nothing
    end
    function filter_params(S, dep, tree, matches)
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
    struct ReadString; end
    register(ReadString, Deprecation(
        "readstring(x) is deprecated to read(x, String)",
        "julia",
        v"0.7.0-DEV.1053", v"1.0", typemax(VersionNumber)
    ))

    match(ReadString,
        "readstring(\$A)",
        "read(\$A, String)"
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
            push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
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
            push!(resolutions, TextReplacement(dep, expr.span, repl))
        end
    end
end


# This doesn't work becuase the `...` what we want to match against
# is also used as a wildcard sentinel
begin
    struct TupleSplat; end

    register(TupleSplat, Deprecation(
        "Splat of single value in tuple needs trailing comma",
        "julia",
        v"0.7.0-DEV.2559", v"1.0", typemax(VersionNumber)
    ))

    function filter_funcdef_and_multiarg(dep, expr, matches)
        length(children(expr)) == 3     || return false  # 3 = '(' + ')' + 1 arg
        isexpr(children(expr)[2], UnarySyntaxOpCall) || return false
        CSTParser.has_sig(parent(expr)) && return false
        return true
    end

    match(TupleSplat,
          "(\$ID...)",
          "(\$ID...,)",
          filter = filter_funcdef_and_multiarg
    )
end

begin
    struct Void2Nothing; end
    register(Void2Nothing, Deprecation(
        "The type `Void` is renamed to `Nothing` (and a synonym `Cvoid` is added)",
        "julia",
        v"0.7.0-DEV.3137",
        v"1.0",
        typemax(VersionNumber)
    ))

    match(Void2Nothing,
        "Void",
        "Nothing",
    )

    match(Void2Nothing,
        "Ptr{Void}",
        "Ptr{Cvoid}",
    )

    match(Void2Nothing, CSTParser.Call) do x
        # Match ccall only
        dep, expr, resolutions, context = x
        is_identifier(children(expr)[1], "ccall") || return
        _replace_void_cvoid!(dep, resolutions, expr)
    end

    function _replace_void_cvoid!(dep, resolutions, expr)
        if expr.expr isa CSTParser.IDENTIFIER && expr.expr.val == "Void"
            push!(resolutions, TextReplacement(dep, expr.span, "Cvoid"))
        end
        isempty(children(expr)) && return
        for c in children(expr)
            _replace_void_cvoid!(dep, resolutions, c)
        end
        return
    end
end

begin
    struct IssubtypeToInfix; end
    register(IssubtypeToInfix, Deprecation(
        "issubtype is deprecated for `<:`",
        "julia",
        v"0.7.0-DEV.1162", v"1.0", typemax(VersionNumber)
    ))

    match(IssubtypeToInfix,
        "issubtype(\$A, \$B)",
        "\$A <: \$B!",
    )
end

function filter_base_id(S, dep, expr, matches)
    @assert isa(expr, OverlayNode{CSTParser.IDENTIFIER})
    binding = resolve(S, expr)
    return binding.t == "BaseCore"
end

macro add_rename(from, to, version)
    StructName = Symbol(from, "_2_", to)
    return quote
        struct $(StructName); end
        register($(StructName),
            Deprecation(
                string($(string(esc(from))), " got renamed to ", $(string(esc(to))), " in version ", $version),
                "julia",
                $version,
                v"1.0",
                typemax(VersionNumber)
            )
        )

        match($(StructName),
              $(string(from)),
              $(string(to)),
              filter = filter_base_id
        )
    end
end

@add_rename ipermute!      invpermute!      v"0.7.0-DEV.3173"
@add_rename unshift!       pushfirst!       v"0.7.0-DEV.3155"
@add_rename JULIA_HOME     Sys.BINDIR       v"0.7.0-DEV.3073"
@add_rename CartesianRange CartesianIndices v"0.7.0-DEV.3025"
@add_rename sub2ind        CartesianIndices v"0.7.0-DEV.3025"
@add_rename ind2sub        LinearIndices    v"0.7.0-DEV.3025"
@add_rename Display        AbstractDisplay  v"0.7.0-DEV.2695"
@add_rename strwidth       textwidth        v"0.7.0-DEV.1930"
@add_rename charwidth      textwidth        v"0.7.0-DEV.1930"
@add_rename sqrtm          sqrt             v"0.7.0-DEV.1599"
@add_rename logm           log              v"0.7.0-DEV.1597"
@add_rename expm           exp              v"0.7.0-DEV.1486"
@add_rename Complex32      ComplexF16       v"0.7.0-DEV.2919"
@add_rename Complex64      ComplexF32       v"0.7.0-DEV.2919"
@add_rename Complex128     ComplexF64       v"0.7.0-DEV.2919"
@add_rename EnvHash        EnvDict          v"0.7.0-DEV.2265"
@add_rename ctranspose     adjoint          v"0.7.0-DEV.1415"
@add_rename ctranspose!    adjoint!         v"0.7.0-DEV.1415"
@add_rename writecsv       writedlm         v"0.7.0-DEV.1737"
@add_rename readcsv        readdlm          v"0.7.0-DEV.1740"
@add_rename is_linux       Sys.islinux      v"0.7.0-DEV.914"
@add_rename is_bsd         Sys.isbsd        v"0.7.0-DEV.914"
@add_rename is_apple       Sys.isapple      v"0.7.0-DEV.914"
@add_rename is_unix        Sys.isunix       v"0.7.0-DEV.914"
@add_rename is_windows     Sys.iswindows    v"0.7.0-DEV.914"
@add_rename AbstractIOBuffer GenericIOBuffer v"0.7.0-DEV.961"
@add_rename select         partialsort      v"0.7.0-DEV.1535"
@add_rename select!        partialsort!     v"0.7.0-DEV.1535"
@add_rename selectperm     partialsortperm  v"0.7.0-DEV.1535"
@add_rename selectperm!    partialsortperm! v"0.7.0-DEV.1535"
@add_rename Range          AbstractRange    v"0.7.0-DEV.1721"
@add_rename isleaftype     isconcretetype   v"0.7.0-DEV.1775"
@add_rename isnumber       isnumeric        v"0.7.0-DEV.1775"
@add_rename Associative    AbstractDict     v"0.7.0-DEV.2951"
@add_rename find           findall          v"0.7.0-DEV.3415"
@add_rename module_parent   parentmodule    v"0.7.0-DEV.3460"
@add_rename datatype_module parentmodule    v"0.7.0-DEV.3460"
@add_rename function_module parentmodule    v"0.7.0-DEV.3460"
@add_rename indmin         argmin           v"0.7.0-DEV.3516"
@add_rename indmax         argmax           v"0.7.0-DEV.3516"
@add_rename module_name    nameof           v"0.7.0-DEV.3539"
@add_rename function_name  nameof           v"0.7.0-DEV.3539"
@add_rename datatype_name  nameof           v"0.7.0-DEV.3539"
@add_rename method_exist   hasmethod        v"0.7.0-DEV.3455"
@add_rename object_id      objectid         v"0.7.0-DEV.3455"
@add_rename DevNull        devnull          v"0.7.0-DEV.4068"
@add_rename STDIN          stdin            v"0.7.0-DEV.4068"
@add_rename STDOUT         stdout           v"0.7.0-DEV.4068"
@add_rename STDERR         stderr           v"0.7.0-DEV.4068"
@add_rename reprmime       repr             v"0.7.0-DEV.4010"

begin
    struct KeywordsUnlocked; end
    register(KeywordsUnlocked, Deprecation(
        "a bunch of functions got keyword arguments instead of positional arguments",
        "julia",
        v"0.7.0-DEV.3526", v"1.0", typemax(VersionNumber)
    ))
    match(KeywordsUnlocked,
        "Timer(\$timeout, \$repeat)",
        "Timer(\$timeout, interval = \$repeat!)"
    )
    match(KeywordsUnlocked,
        "Timer(\$callback, \$delay, \$repeat)",
        "Timer(\$callback, \$delay!, interval = \$repeat!)"
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all)",
        "names(\$m, all = \$all!)"
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all, \$imported)",
        "names(\$m, all = \$all!, imported = \$imported!)"
    )
    match(KeywordsUnlocked,
        "code_native(\$io, \$f, \$types, \$syntax)",
        "code_native(\$io, \$f!, \$types!, syntax = \$syntax!)"
        )
    match(KeywordsUnlocked,
        "code_native(\$f, \$types, \$syntax)",
        "code_native(\$f, \$types!, syntax = \$syntax!)"
        )
    match(KeywordsUnlocked,
        "eachmatch(\$re, \$str, \$overlap)",
        "eachmatch(\$re, \$str!, overlap = \$overlap!)"
        )
    match(KeywordsUnlocked,
        "matchall(\$re, \$str, \$overlap)",
        "matchall(\$re, \$str!, overlap = \$overlap!)"
        )
    match(KeywordsUnlocked,
        "chop(\$s, \$head)",
        "chop(\$s, head = \$head!)"
        )
    match(KeywordsUnlocked,
        "chop(\$s, \$head, \$tail)",
        "chop(\$s, head = \$head!, tail = \$tail!)"
        )
    match(KeywordsUnlocked,
        "tryparse(\$T, \$s, \$base)",
        "tryparse(\$T, \$s!, base = \$base!)"
        )
    match(KeywordsUnlocked,
        "mkdir(\$path, \$mode)",
        "mkdir(\$path, mode = \$mode!)"
        )
    match(KeywordsUnlocked,
        "mkpath(\$path, \$mode)",
        "mkpath(\$path, mode = \$mode!)"
        )
    match(KeywordsUnlocked,
        "countlines(\$x, \$eol)",
        "countlines(\$x, eol = \$eol!)"
        )
    match(KeywordsUnlocked,
        "PipeBuffer(\$data, \$maxsize)",
        "PipeBuffer(\$data, maxsize = \$maxsize!)"
        )
    match(KeywordsUnlocked,
        "unsafe_wrap(\$T, \$pointer, \$dims, \$own)",
        "unsafe_wrap(\$T, \$pointer!, \$dims!, own = \$own!)"
        )
end
