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

    function filter_funcdef_and_multiarg(S, dep, expr, matches)
        length(children(expr)) == 3     || return false  # 3 = '(' + ')' + 1 arg
        isexpr(children(expr)[2], UnarySyntaxOpCall) || return false
        CSTParser.has_sig(parent(expr)) && return false
        if isexpr(CSTParser.parent(expr), CSTParser.UnarySyntaxOpCall) &&
            isexpr(children(CSTParser.parent(expr))[1], CSTParser.OPERATOR, Tokens.EX_OR)
            return false
        end
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

function filter_base_id(analysis, expr)
    S, file_scope = analysis
    @assert isa(expr, OverlayNode{CSTParser.IDENTIFIER})
    isexpr(parent(expr), CSTParser.Quotenode) && return false
    # Not on the LHS of a keyword arg
    if isexpr(parent(expr), CSTParser.Kw)
        expr == children(parent(expr))[1] && return false
    end
    binding = CSTAnalyzer.resolve(S, file_scope, expr)
    return binding.t == "BaseCore"
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
    @add_rename sub2ind        CartesianIndices v"0.7.0-DEV.3025" true
    @add_rename ind2sub        LinearIndices    v"0.7.0-DEV.3025" true
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
    CSTParser.defines_function(parent(tree)) && return false
    return true
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
    S, file_scope = analysis
    id = children(tree)[3]
    isexpr(id, CSTParser.IDENTIFIER) || return true
    binding = CSTAnalyzer.resolve(S, file_scope, id)
    if binding.t == "BaseCore"
        if Symbol(id.expr.val) in BaseCoreNumberTypes
            return false
        end
    end
    return true
end

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
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "Timer(\$callback, \$delay, \$repeat)",
        "Timer(\$callback, \$delay!, interval=\$repeat!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all)",
        "names(\$m, all=\$all!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "names(\$m, \$all, \$imported)",
        "names(\$m, all=\$all!, imported=\$imported!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "code_native(\$io, \$f, \$types, \$syntax)",
        "code_native(\$io, \$f!, \$types!, syntax=\$syntax!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "code_native(\$f, \$types, \$syntax)",
        "code_native(\$f, \$types!, syntax=\$syntax!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "eachmatch(\$re, \$str, \$overlap)",
        "eachmatch(\$re, \$str!, overlap=\$overlap!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "matchall(\$re, \$str, \$overlap)",
        "matchall(\$re, \$str!, overlap=\$overlap!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "chop(\$s, \$head)",
        "chop(\$s, head=\$head!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "chop(\$s, \$head, \$tail)",
        "chop(\$s, head=\$head!, tail=\$tail!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "tryparse(\$T, \$s, \$base)",
        "tryparse(\$T, \$s!, base=\$base!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "mkdir(\$path, \$mode)",
        "mkdir(\$path, mode=\$mode!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "mkpath(\$path, \$mode)",
        "mkpath(\$path, mode=\$mode!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "countlines(\$x, \$eol)",
        "countlines(\$x, eol=\$eol!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "PipeBuffer(\$data, \$maxsize)",
        "PipeBuffer(\$data, maxsize=\$maxsize!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "unsafe_wrap(\$T, \$pointer, \$dims, \$own)",
        "unsafe_wrap(\$T, \$pointer!, \$dims!, own=\$own!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "linspace(\$start, \$stop)",
        "range(\$start, stop=\$stop, length=50)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "logspace(\$start, \$stop)",
        "exp10.(range(\$start!, stop=\$stop!, length=50))",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "linspace(\$start, \$stop, \$length)",
        "range(\$start!, stop=\$stop!, length=\$length)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "logspace(\$start, \$stop, \$length)",
        "exp10.(range(\$start, stop=\$stop, length=\$length))",
        filter = filter_no_kw_already
    )
    for f in ("trunc", "floor", "ceil", "round")
        match(KeywordsUnlocked,
            "$f(\$x, \$digits)",
            "$f(\$x!, digits=\$digits!)",
            filter = (args...)->filter_no_kw_already(args...) &&
                                filter_not_likely_type(args...) &&
                                filter_not_def(args...)
        )
        match(KeywordsUnlocked,
            "$f(\$x, \$digits, \$base)",
            "$f(\$x!, digits=\$digits!, base=\$base!)",
            filter = (args...)->filter_no_kw_already(args...) &&
                                filter_not_likely_type(args...) &&
                                filter_not_def(args...)
        )
    end
    match(KeywordsUnlocked,
        "signif(\$x, \$digits)",
        "round(\$x!, sigdigits=\$digits!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "signif(\$x, \$digits, \$base)",
        "round(\$x!, sigdigits=\$digits!, base=\$base!)",
        filter = filter_no_kw_already
    )
    match(KeywordsUnlocked,
        "method_exists(\$f, \$t, \$world)",
        "hasmethod(\$f, \$t, world=\$world)",
        filter = filter_no_kw_already
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
]
