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
        "struct \$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldStructSyntax,
        "type \$name\n\$BODY...\nend",
        "mutable struct \$name\n\$BODY!...\nend",
        format_paramlist
    )
    match(OldStructSyntax,
        "abstract \$name",
        "abstract type \$name end"
    )
    match(OldStructSyntax,
        "bitstype \$size \$name",
        "primitive type \$name \$size! end"
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
        (isexpr(parent(tree), BinarySyntaxOpCall) || isexpr(parent(tree), CSTParser.WhereOpCall)) || return false
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
        # Filter out (::Type{<:Foo})
        if isexpr(name, CSTParser.UnarySyntaxOpCall) &&
            isexpr(children(name)[1], CSTParser.OPERATOR, Tokens.ISSUBTYPE)
            return false
        end
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
        allws = string(trailing_trivia(exprs[1]), leading_trivia(expr))
        if isempty(allws) || !isspace(allws[end])
            expr = children(ret)[idx] = TriviaReplacementNode(ret, expr, string(leading_trivia(expr), " "), trailing_trivia(expr))
            any = true
        end
        allws = string(trailing_trivia(expr), leading_trivia(exprs[3]))
        if isempty(allws) || !isspace(allws[end])
            expr = children(ret)[idx] = TriviaReplacementNode(ret, expr, leading_trivia(expr), string(" ", trailing_trivia(expr)))
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
        allws = string(trailing_trivia(body), leading_trivia(fornode))
        if isempty(allws) || !isspace(allws[end])
            children(ret)[2] = TriviaReplacementNode(ret, fornode, string(leading_trivia(fornode), " "), trailing_trivia(fornode))
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

    function filter_funcdef_broadcast_multiarg(S, dep, expr, matches)
        length(children(expr)) == 3     || return false  # 3 = '(' + ')' + 1 arg
        isexpr(children(expr)[2], UnarySyntaxOpCall) || return false
        CSTParser.has_sig(parent(expr)) && return false
        # AST Interpolation $(x...)
        if isexpr(CSTParser.parent(expr), CSTParser.UnarySyntaxOpCall) &&
            isexpr(children(CSTParser.parent(expr))[1], CSTParser.OPERATOR, Tokens.EX_OR)
            return false
        # Broadcasting
        elseif isexpr(CSTParser.parent(expr), CSTParser.BinarySyntaxOpCall) &&
            isexpr(children(CSTParser.parent(expr))[2], CSTParser.OPERATOR, Tokens.DOT)
            return false
        end
        return true
    end

    match(TupleSplat,
          "(\$ID...)",
          "(\$ID...,)",
          filter = filter_funcdef_broadcast_multiarg
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

function is_scope_Base(scope)
    while scope !== nothing
        if scope.t == "Module" && scope.namespace == "Base"
            return true
        end
        scope = scope.parent
    end
    return false
end

function filter_base_id(analysis, expr)
    S, file_scope = analysis
    @assert isa(expr, OverlayNode{CSTParser.IDENTIFIER})
    isexpr(parent(expr), CSTParser.Quotenode) && return false
    # Not on the LHS of a keyword arg
    if isexpr(parent(expr), CSTParser.Kw)
        expr == children(parent(expr))[1] && return false
    end
    scope = CSTAnalyzer.find_scope(file_scope, expr.span)
    binding = CSTAnalyzer.find_ref(span_text(expr), scope, S)
    !isa(binding, CSTAnalyzer.Binding) && return false
    return binding.t == "BaseCore" || is_scope_Base(scope)
end

struct SimpleRename
    min_ver::VersionNumber
end
SimpleRename() = SimpleRename(v"0.6.0")
function dep_for_vers(::Type{SimpleRename}, vers)
    SimpleRename(minimum(map(interval->interval.lower, vers["julia"].intervals)))
end
register(SimpleRename, Deprecation(
    "a number of identifiers were renamed",
    "julia",
    v"0.7.0-DEV.1", v"0.7.0-DEV.1", typemax(VersionNumber)
))

macro add_rename(pattern, replacement, ver, hascompat = false)
    :(($ver, $(string(pattern))=>$(string(replacement)), $hascompat))
end

match(SimpleRename, CSTParser.IDENTIFIER) do x
    dep, expr, resolutions, context, analysis = x
    context.in_macrocall && return
    for rename in renames
        if rename[1] <= dep.min_ver && is_identifier(expr, rename[2][1])
            filter_base_id(analysis, expr) || return
            push!(resolutions, TextReplacement(dep, expr.span, rename[2][2]))
        end
    end
end

const renames = [
    @add_rename ipermute!      invpermute!      v"0.7.0-DEV.3173" true
    @add_rename unshift!       pushfirst!       v"0.7.0-DEV.3155" true
    @add_rename shift!         popfirst!        v"0.7.0-DEV.3155" true
    @add_rename JULIA_HOME     Sys.BINDIR       v"0.7.0-DEV.3073" true
    @add_rename CartesianRange CartesianIndices v"0.7.0-DEV.3025" true
    @add_rename Display        AbstractDisplay  v"0.7.0-DEV.2695" true
    @add_rename strwidth       textwidth        v"0.7.0-DEV.1930" true
    @add_rename charwidth      textwidth        v"0.7.0-DEV.1930" true
    @add_rename sqrtm          sqrt             v"0.7.0-DEV.1599"
    @add_rename logm           log              v"0.7.0-DEV.1597"
    @add_rename expm           exp              v"0.7.0-DEV.1486"
    @add_rename Complex32      ComplexF16       v"0.7.0-DEV.2919" true
    @add_rename Complex64      ComplexF32       v"0.7.0-DEV.2919" true
    @add_rename Complex128     ComplexF64       v"0.7.0-DEV.2919" true
    @add_rename EnvHash        EnvDict          v"0.7.0-DEV.2265"
    @add_rename ctranspose     adjoint          v"0.7.0-DEV.1415" true
    @add_rename ctranspose!    adjoint!         v"0.7.0-DEV.1415" true
    @add_rename writecsv       writedlm         v"0.7.0-DEV.1737"
    @add_rename readcsv        readdlm          v"0.7.0-DEV.1740"
    @add_rename is_linux       Sys.islinux      v"0.7.0-DEV.914" true
    @add_rename is_bsd         Sys.isbsd        v"0.7.0-DEV.914" true
    @add_rename is_apple       Sys.isapple      v"0.7.0-DEV.914" true
    @add_rename is_unix        Sys.isunix       v"0.7.0-DEV.914" true
    @add_rename is_windows     Sys.iswindows    v"0.7.0-DEV.914" true
    @add_rename AbstractIOBuffer GenericIOBuffer v"0.7.0-DEV.961"
    @add_rename select         partialsort      v"0.7.0-DEV.1535" true
    @add_rename select!        partialsort!     v"0.7.0-DEV.1535" true
    @add_rename selectperm     partialsortperm  v"0.7.0-DEV.1535" true
    @add_rename selectperm!    partialsortperm! v"0.7.0-DEV.1535" true
    @add_rename Range          AbstractRange    v"0.7.0-DEV.1721" true
    @add_rename isleaftype     isconcretetype   v"0.7.0-DEV.1775" true
    @add_rename isnumber       isnumeric        v"0.7.0-DEV.1775" true
    @add_rename Associative    AbstractDict     v"0.7.0-DEV.2951" true
    @add_rename find           findall          v"0.7.0-DEV.3415" true
    @add_rename module_parent   parentmodule    v"0.7.0-DEV.3460" true
    @add_rename datatype_module parentmodule    v"0.7.0-DEV.3460" true
    @add_rename function_module parentmodule    v"0.7.0-DEV.3460" true
    @add_rename indmin         argmin           v"0.7.0-DEV.3516" true
    @add_rename indmax         argmax           v"0.7.0-DEV.3516" true
    @add_rename module_name    nameof           v"0.7.0-DEV.3539" true
    @add_rename function_name  nameof           v"0.7.0-DEV.3539" true
    @add_rename datatype_name  nameof           v"0.7.0-DEV.3539" true
    @add_rename method_exist   hasmethod        v"0.7.0-DEV.3455" true
    @add_rename object_id      objectid         v"0.7.0-DEV.3455" true
    @add_rename DevNull        devnull          v"0.7.0-DEV.4068" true
    @add_rename STDIN          stdin            v"0.7.0-DEV.4068" true
    @add_rename STDOUT         stdout           v"0.7.0-DEV.4068" true
    @add_rename STDERR         stderr           v"0.7.0-DEV.4068" true
    @add_rename reprmime       repr             v"0.7.0-DEV.4010"
    @add_rename isabstract     isabstracttype   v"0.7.0-DEV.1775" true
    @add_rename iteratorsize   IteratorSize     v"0.7.0-DEV.3309" true
    @add_rename iteratoreltype IteratorEltype   v"0.7.0-DEV.3309" true
    @add_rename nb_available   bytesavailable   v"0.7.0-DEV.3477" true
    @add_rename broadcast_indices broadcast_axes  v"0.7.0-DEV.4936"
    @add_rename atan2          atan             v"0.7.0-alpha.44"
]

for (ver, repl, hascompat) in renames
    if hascompat
        match(SimpleRename, "Compat.$(repl.second)", repl.second)
    end
end


function filter_no_kw_already(S, dep, tree, matches)
    @assert isexpr(tree, CSTParser.Call)
    for c in children(tree)
        isexpr(c, CSTParser.Kw) && return false
    end
    return true
end

function filter_not_def(S, dep, tree, matches)
    p = parent(tree)
    isexpr(p, CSTParser.WhereOpCall) && (p = parent(p))
    isexpr(p, CSTParser.Macro) && return false
    CSTParser.defines_function(p) && return false
    return true
end

function resolve_qualified_expr(id, analysis)
    S, file_scope = analysis
    if isexpr(id, CSTParser.BinarySyntaxOpCall) && isexpr(children(id)[2], CSTParser.OPERATOR, Tokens.DOT)
        isexpr(children(id)[3], CSTParser.Quotenode) || return nothing
        cid = children(children(id)[3])[1]
        isexpr(cid, CSTParser.IDENTIFIER) || return nothing
        return (is_identifier(children(id)[1], "Base") ||
                is_identifier(children(id)[1], "Core")) ? (cid, "BaseCore") : nothing
    else
        isexpr(id, CSTParser.IDENTIFIER) || return nothing
        binding = CSTAnalyzer.resolve(S, file_scope, id)
        !isa(binding, CSTAnalyzer.Binding) && return nothing
        return (id, binding.t)
    end
end

const BaseCoreNumberTypes = [
    # Core
    :AbstractFloat, :Bool, :Float16, :Float32, :Float64, :Int, :Int128,
    :Int16, :Int32, :Int64, :Int8, :Integer, :Number, :Real, :Signed,
    :UInt, :UInt128, :UInt16, :UInt32, :UInt64, :UInt8, :Unsigned,
    # Base
    :BigFloat, :BigInt, :Cchar, :Cdouble, :Cfloat, :Cint, :Cintmax_t,
    :Clong, :Clonglong, :Complex, :Complex128, :Complex32, :Complex64,
    :Cptrdiff_t, :Cshort, :Csize_t, :Cssize_t, :Cuchar, :Cuint, :Cuintmax_t,
    :Culong, :Culonglong, :Cushort, :Cwchar_t, :Irrational, :Rational
]

# Ideally we'd do the opposite and find things that are likely ::Number, but
# that's harder
function filter_not_likely_type(analysis, dep, tree, matches)
    @assert isexpr(tree, CSTParser.Call)
    x = resolve_qualified_expr(children(tree)[3], analysis)
    x === nothing && return true
    (id, t) = x
    t == :DataType && return false
    t == "BaseCore" || return true
    (Symbol(id_name(id)) in BaseCoreNumberTypes) && return false
    return true
end

const RoundingMode = [
    :RoundNearest, :RoundToZero, :RoundUp, :RoundDown,
    :RoundFromZero, :RoundNearestTiesAway, :RoundNearestTiesUp
]

function filter_not_rounding_mode(analysis, dep, tree, matches)
    @assert isexpr(tree, CSTParser.Call)
    x = resolve_qualified_expr(children(tree)[end - 1], analysis)
    x === nothing && return true
    (id, t) = x
    t == "BaseCore" || return true
    (Symbol(id_name(id)) in RoundingMode) && return false
    return true
end

keyword_default_filter(args...) = filter_no_kw_already(args...) && filter_not_def(args...)

struct KeywordsUnlocked; end
begin
    register(KeywordsUnlocked, Deprecation(
        "a bunch of functions got keyword arguments instead of positional arguments",
        "julia",
        v"0.7.0-DEV.1", v"1.0", typemax(VersionNumber)
    ))
    match(KeywordsUnlocked,
        "Timer(\$timeout, \$repeat)",
        "Timer(\$timeout, interval=\$repeat!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "Timer(\$callback, \$delay, \$repeat)",
        "Timer(\$callback, \$delay!, interval=\$repeat!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all)",
        "names(\$m, all=\$all!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all, \$imported)",
        "names(\$m, all=\$all!, imported=\$imported!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "code_native(\$io, \$f, \$types, \$syntax)",
        "code_native(\$io, \$f!, \$types!, syntax=\$syntax!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "code_native(\$f, \$types, \$syntax)",
        "code_native(\$f, \$types!, syntax=\$syntax!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "eachmatch(\$re, \$str, \$overlap)",
        "eachmatch(\$re, \$str!, overlap=\$overlap!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "matchall(\$re, \$str, \$overlap)",
        "matchall(\$re, \$str!, overlap=\$overlap!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "chop(\$s, \$head)",
        "chop(\$s, head=\$head!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "chop(\$s, \$head, \$tail)",
        "chop(\$s, head=\$head!, tail=\$tail!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "tryparse(\$T, \$s, \$base)",
        "tryparse(\$T, \$s!, base=\$base!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "mkdir(\$path, \$mode)",
        "mkdir(\$path, mode=\$mode!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "mkpath(\$path, \$mode)",
        "mkpath(\$path, mode=\$mode!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "countlines(\$x, \$eol)",
        "countlines(\$x, eol=\$eol!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "PipeBuffer(\$data, \$maxsize)",
        "PipeBuffer(\$data, maxsize=\$maxsize!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "unsafe_wrap(\$T, \$pointer, \$dims, \$own)",
        "unsafe_wrap(\$T, \$pointer!, \$dims!, own=\$own!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "linspace(\$start, \$stop)",
        "range(\$start, stop=\$stop, length=50)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "logspace(\$start, \$stop)",
        "exp10.(range(\$start!, stop=\$stop!, length=50))",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "linspace(\$start, \$stop, \$length)",
        "range(\$start!, stop=\$stop!, length=\$length)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "logspace(\$start, \$stop, \$length)",
        "exp10.(range(\$start, stop=\$stop, length=\$length))",
        filter = keyword_default_filter
    )
    for f in ("trunc", "floor", "ceil", "round")
        match(KeywordsUnlocked,
            "$f(\$x, \$digits)",
            "$f(\$x!, digits=\$digits!)",
            filter = (args...)->filter_no_kw_already(args...) &&
                                filter_not_likely_type(args...) &&
                                filter_not_def(args...) &&
                                filter_not_rounding_mode(args...)
        )
        match(KeywordsUnlocked,
            "$f(\$x, \$digits, \$base)",
            "$f(\$x!, digits=\$digits!, base=\$base!)",
            filter = (args...)->filter_no_kw_already(args...) &&
                                filter_not_likely_type(args...) &&
                                filter_not_def(args...) &&
                                filter_not_rounding_mode(args...)
        )
    end
    match(KeywordsUnlocked,
        "signif(\$x, \$digits)",
        "round(\$x!, sigdigits=\$digits!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "signif(\$x, \$digits, \$base)",
        "round(\$x!, sigdigits=\$digits!, base=\$base!)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "method_exists(\$f, \$t, \$world)",
        "hasmethod(\$f, \$t, world=\$world)",
        filter = keyword_default_filter
    )
    match(KeywordsUnlocked,
        "print_with_color(\$color, \$args...)",
        "printstyled(\$args!..., color=\$color)",
        filter = keyword_default_filter
    )
end

function filter_func_base_id(analysis, dep, tree, matches)
    @assert isexpr(tree, CSTParser.Call)
    id = first(children(tree))
    return filter_base_id(analysis, id)
end

struct Misc07; end
begin
    register(Misc07, Deprecation(
        "a number of function APIs were changed",
        "julia",
        v"0.7.0-DEV.1", v"1.0", typemax(VersionNumber)
    ))
    match(Misc07,
        "ismatch(\$r, \$s)",
        "occursin(\$r, \$s)"
    )
    match(Misc07,
        "findin(\$a, \$b)",
        "findall(in(\$b), \$a)"
    )
    match(Misc07,
        "method_exists(\$f, \$t)",
        "hasmethod(\$f, \$t)"
    )
    match(Misc07,
        "parse(\$s)",
        "Meta.parse(\$s)",
        filter = filter_func_base_id
    )
end

struct RenamedKeyword
    min_ver::VersionNumber
end
RenamedKeyword() = RenamedKeyword(v"0.6.0")
function dep_for_vers(::Type{RenamedKeyword}, vers)
    RenamedKeyword(minimum(map(interval->interval.lower, vers["julia"].intervals)))
end
register(RenamedKeyword, Deprecation(
    "a number of keyword arguments were renamed",
    "julia",
    v"0.7.0-DEV.1", v"0.7.0-DEV.1", typemax(VersionNumber)
))

function process_renamed_keyword(x)
    dep, expr, resolutions, context, analysis = x
    context.in_macrocall && return
    for rename in keyword_renames
        if rename[1] <= dep.min_ver && is_identifier(expr, rename[3][1])
            # Check if we're in keyword context
            p = parent(expr)
            isexpr(p, CSTParser.Kw) || return
            # Check if the call we're in is on the list
            pp = parent(p)
            isexpr(pp, CSTParser.Parameters) && (pp = parent(pp))
            isexpr(pp, CSTParser.Call) || return
            call_id = children(pp)[1]
            any(name->is_identifier(call_id, name), rename[2]) || return
            push!(resolutions, TextReplacement(dep, expr.span, rename[3][2]))
        end
    end
end

match(RenamedKeyword, CSTParser.IDENTIFIER) do x
    process_renamed_keyword(x)
end

const keyword_renames = [
    (v"0.7.0-DEV.3995", ["cp", "mv", "cptree"], "remove_destination"=>"force")
    (v"0.7.0-DEV.4724", ["split", "rsplit"], "keep"=>"keepempty")
]

# =======================
struct LocalConst; end
register(LocalConst, Deprecation(
    "The `const` keyword in local scope has been deprecated (it never worked anyway)",
    "julia",
    v"0.1.0", v"0.7.0-DEV.1", typemax(VersionNumber)
))

match(LocalConst, CSTParser.Const) do x
    dep, expr, resolutions, context, analysis = x
    context.in_macrocall && return
    S, file_scope = analysis
    isexpr(parent(expr), CSTParser.Global) && return
    isexpr(children(expr)[2], CSTParser.Global) && return
    scope = CSTAnalyzer.find_scope(file_scope, expr.span)
    if !(scope.t in ("__toplevel__", "Module")) && !(scope.t in ("Quote", "@eval"))
        buf = IOBuffer()
        print_replacement(buf, children(expr)[2], false, false)
        push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
    end
    return
end

# =======================
struct ArrayUndef; end
register(ArrayUndef, Deprecation(
    "Arrays now require an explicit `undef` initializer",
    "julia",
    v"0.7.0-alpha", v"0.7.0-DEV.1", typemax(VersionNumber)
))

match(ArrayUndef, CSTParser.Call) do x
    dep, expr, resolutions, context, analysis = x
    context.in_macrocall && return
    curly = children(expr)[1]
    isexpr(curly, CSTParser.Curly) || return
    isexpr(children(curly)[1], CSTParser.IDENTIFIER) || return
    id = Symbol(id_name(children(curly)[1]))
    (id == :Array || id == :Vector) || return
    args = children(expr)[3:end-1]
    is_vector = id == :Vector || (
        length(children(curly)) == 6 &&
        isexpr(children(curly)[5], CSTParser.LITERAL) &&
        Expr(children(curly)[5]) == 1
    )
    # Vector{T}(0) -> Vector{T}()
    if is_vector
        if length(args) == 1
            arg = args[1]
            if isexpr(arg, CSTParser.LITERAL) && Expr(arg.expr) == 0
                repl = ChildReplacementNode(nothing, [
                    children(expr)[1:2]...,
                    children(expr)[4:end]...
                ], expr)
                buf = IOBuffer()
                print_replacement(buf, repl, false, false)
                push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
                return
            end
        elseif length(args) == 0
            # This is ok
            return
        end
    end
    if all(args) do arg
                isexpr(arg, CSTParser.PUNCTUATION, Tokens.COMMA) && return true
                isexpr(arg, CSTParser.LITERAL) || return false
                isa(Expr(arg.expr), Integer) || return false
                return true
            end
        repl = ChildReplacementNode(nothing, [
            children(expr)[1:2]...,
            ReplacementNode("undef", "", ""),
            (isempty(args) ? () : (ReplacementNode(",","", " "),))...,
            children(expr)[3:end]...
        ], expr)
        buf = IOBuffer()
        print_replacement(buf, repl, false, false)
        push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
    end
end

# =======================
struct LoggingMacros; end
register(LoggingMacros, Deprecation(
    "Several logging functions became macros",
    "julia",
    v"0.7.0-DEV.2979", v"0.7.0-DEV.1", typemax(VersionNumber)
))

match(LoggingMacros, CSTParser.Call) do x
    dep, expr, resolutions, context, analysis = x
    context.in_macrocall && return
    fname = children(expr[1])
    (is_identifier(fname, "warn") || is_identifier(fname, "info")) || return
    filter_base_id(analysis, fname) || return
    # For now, only support a single string argument
    length(children(expr)) == 4 || return
    isexpr(children(expr)[3], CSTParser.LITERAL) || return
    repl = ChildReplacementNode(nothing, [
        ReplacementNode(string("@",id_name(fname)), leading_trivia(fname), trailing_trivia(fname)),
        children(expr)[2:end]...
    ], expr)
    buf = IOBuffer()
    print_replacement(buf, repl, false, false)
    push!(resolutions, TextReplacement(dep, expr.span, String(take!(buf))))
end
