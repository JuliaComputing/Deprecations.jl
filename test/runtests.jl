using Deprecations
using Deprecations: edit_text, overlay_parse, apply_formatter, changed_text
using Base.Test

@test edit_text("""
function foobar{T}(x, y::T)
    println(x, y)
end
""")[2] == """
function foobar(x, y::T) where T
    println(x, y)
end
"""

@test edit_text("""
module Test
    function foobar{T}(x, y::T)
        println(x, y)
    end
end
""")[2] == """
module Test
    function foobar(x, y::T) where T
        println(x, y)
    end
end
"""

@test edit_text("""
function foobar{S,T}(x::S, y::T)
    println(x, y)
end
""")[2] == """
function foobar(x::S, y::T) where {S,T}
    println(x, y)
end
"""

@test edit_text("""
module Test
    function foobar{S,T}(x::S, y::T)
        println(x, y)
    end
end
""")[2] == """
module Test
    function foobar(x::S, y::T) where {S,T}
        println(x, y)
    end
end
"""

@test edit_text("f{T}(x::T) = x")[2] == "f(x::T) where {T} = x"

@test edit_text("""
if VERSION >= v"0.1.0"
    do_magic()
end
""")[2] == "do_magic()\n"

@test edit_text("@compat(Union{Int64,Float64})")[2] == "Union{Int64,Float64}"
@test edit_text("@compat Union{Int64,Float64}")[2] == "Union{Int64,Float64}"
@test edit_text("Compat.UTF8String")[2] == "String"

@test edit_text("""
jlconvert(::Type{Symbol}, file::JldFile, ptr::Ptr) = Symbol(jlconvert(Compat.UTF8String, file, ptr))
""")[2] == """
jlconvert(::Type{Symbol}, file::JldFile, ptr::Ptr) = Symbol(jlconvert(String, file, ptr))
"""

@test edit_text("""
# Some comment
const A = 1
const B = Compat.UTF8String
""")[2] == """
# Some comment
const A = 1
const B = String
"""

@test edit_text("""
@sprintf("%08d", id)
Union{Type{Compat.ASCIIString},Type{Compat.UTF8String}}
""")[2] == """
@sprintf("%08d", id)
Union{Type{String},Type{String}}
"""

@test edit_text(raw"""
"ABC$T"
if cset == HDF5.H5T_CSET_ASCII
    return Compat.ASCIIString
elseif cset == HDF5.H5T_CSET_UTF8
    return Compat.UTF8String
end
""")[2] == raw"""
"ABC$T"
if cset == HDF5.H5T_CSET_ASCII
    return String
elseif cset == HDF5.H5T_CSET_UTF8
    return String
end
"""

@test edit_text(raw"""
T.mutable && print("Ok")
return Compat.ASCIIString
""")[2] == """
T.mutable && print("Ok")
return String
"""

@test edit_text(raw"""
error("$T")
return Compat.ASCIIString
""")[2] == raw"""
error("$T")
return String
"""

@test edit_text(raw"""
error(\"\"\"$T is not supported\"\"\")
Compat.ASCIIString
""")[2] == raw"""
error(\"\"\"$T is not supported\"\"\")
String
"""

@test edit_text(raw"""
###############################################
## Reading and writing Julia data .jld files ##
###############################################

module JLD00
write{T<:Union{HDF5BitsKind, String}}(parent::Union{JldFile, JldGroup}, name::String, data::Union{T, Array{T}}) =
    write(parent, name, data, full_typename(typeof(data)))
end
""")[2] == raw"""
###############################################
## Reading and writing Julia data .jld files ##
###############################################

module JLD00
write(parent::Union{JldFile, JldGroup}, name::String, data::Union{T, Array{T}}) where {T<:Union{HDF5BitsKind, String}} =
    write(parent, name, data, full_typename(typeof(data)))
end
"""

@test edit_text("""
function foo{T}(a::A,
                b::T)
end
""")[2] == """
function foo(a::A,
             b::T) where T
end
"""

@test edit_text("""
f{T}(a::A,
     b::T) =
        a
""")[2] == """
f(a::A,
  b::T) where {T} =
        a
"""

@test edit_text(raw"""
using Compat: @compat
@save f li lidict
if VERSION > v"0.1.0"
    true
end
""")[2] == """
using Compat: @compat
@save f li lidict
true
"""

@test edit_text("""
if VERSION < v"0.1.0"
    true
else
    false
end
""")[2] == "false\n"

@test edit_text("""
if VERSION > v"0.1.0"
    true
else
    false
end
""")[2] == "true\n"

@test edit_text("""
if VERSION < v"0.1.0"
    true
elseif true
    false
end
""")[2] == """
if true
    false
end
"""

@test edit_text("""
if VERSION > v"0.1.0"
    a
    b
    c
end
""")[2] == """
a
b
c
"""

@test edit_text("""
immutable DTable{K,V}
    subdomains::Vector{IndexSpace{K}}
    chunks::Vector
end
""")[2] == """
struct DTable{K,V}
    subdomains::Vector{IndexSpace{K}}
    chunks::Vector
end
"""

@test edit_text("""
function naturaljoin{I1, I2, D1, D2}(left::DTable{I1,D1},
                                     right::DTable{I2,D2},
                                     op, ascolumns=false)
    out_subdomains = Any[]
end
""")[2] == """
function naturaljoin(left::DTable{I1,D1},
                     right::DTable{I2,D2},
                     op, ascolumns=false) where {I1, I2, D1, D2}
    out_subdomains = Any[]
end
"""

@test edit_text("""
function Base.getindex{K}(t::DTable{K}, idxs...)
    if typeof(idxs) <: astuple(K)
        _getindex_scalar(t, idxs)
    else
        _getindex(t, idxs)
    end
end
""")[2] == """
function Base.getindex(t::DTable{K}, idxs...) where K
    if typeof(idxs) <: astuple(K)
        _getindex_scalar(t, idxs)
    else
        _getindex(t, idxs)
    end
end
"""

t = """
function foobar(a,
         b::T,
         c::T) where T
    (a, b, c)
end
"""
x = apply_formatter(Deprecations.format_align_arguments, overlay_parse(t, false))
@test changed_text(t, [x])[2] == """
function foobar(a,
                b::T,
                c::T) where T
    (a, b, c)
end
"""

@test edit_text("abstract FooBar")[2] == "abstract type FooBar end"
@test edit_text("abstract type FooBar end")[2] == "abstract type FooBar end"

@test edit_text("""
if VERSION < v"0.6-"
    true
end
""", [Deprecations.dep_for_vers(
    Deprecations.ObsoleteVersionCheck,
    Pkg.Reqs.parse(IOBuffer("julia 0.5"))
)])[2] == """
if VERSION < v"0.6-"
    true
end
"""

@test edit_text("""
if VERSION < v"0.6-"
    true
end
""", [Deprecations.dep_for_vers(
    Deprecations.ObsoleteVersionCheck,
    Pkg.Reqs.parse(IOBuffer("julia 0.6"))
)])[2] == ""

@test edit_text("""
@static if VERSION < v"0.6.0-dev.1015"
    nodarg = Symbol(join(drop(string(arg), 1)))
else
    nodarg = Symbol(join(Base.Iterators.drop(string(arg), 1)))
end
""")[2] == """
nodarg = Symbol(join(Base.Iterators.drop(string(arg), 1)))
"""

@test edit_text("""
@static if VERSION < v"0.1.0"
    true # Comment A
else
    true # Comment B
end
""")[2] == """
true # Comment B
"""

@test edit_text("""
@static if VERSION > v"0.1.0"
    true # Comment A
else
    true # Comment B
end
""")[2] == """
true # Comment A
"""

@test edit_text("""
if VERSION < v"0.6.0-dev.1632"
    false
end


##### Utility functions
true
""")[2] == """


##### Utility functions
true
"""

@test edit_text("""
function Base.repeat{T,N}(A::DataArray{T,N};
                          inner = ntuple(x->1, ndims(A)),
                          outer = ntuple(x->1, ndims(A)))
    nothing
end
""")[2] == """
function Base.repeat(A::DataArray{T,N};
                     inner = ntuple(x->1, ndims(A)),
                     outer = ntuple(x->1, ndims(A))) where {T,N}
    nothing
end
"""

@test edit_text("""
immutable Foo{A,
              B}
    x::Tuple{A,B}
end
""")[2] == """
struct Foo{A,
           B}
    x::Tuple{A,B}
end
"""

@test edit_text("""
type Foo{A,
         B}
    x::Tuple{A,B}
end
""")[2] == """
mutable struct Foo{A,
                   B}
    x::Tuple{A,B}
end
"""

@test edit_text("""
function Base.repeat{T,N}(A::DataArray{T,N};
                          inner = ntuple(x->1, ndims(A)),
                          outer = ntuple(x->1, ndims(A)))
    nothing
end
""")[2] == """
function Base.repeat(A::DataArray{T,N};
                     inner = ntuple(x->1, ndims(A)),
                     outer = ntuple(x->1, ndims(A))) where {T,N}
    nothing
end
"""

@test edit_text("""
function foobar{S,T}(
        a::S,
        b::T)
    nothing
end
""")[2] == """
function foobar(
        a::S,
        b::T) where {S,T}
    nothing
end
"""

@test edit_text("""
immutable KfoldState
    i::Int      # the i-th of the subset
    s::Int      # starting index
    e::Int      # ending index
end
""")[2] == """
struct KfoldState
    i::Int      # the i-th of the subset
    s::Int      # starting index
    e::Int      # ending index
end
"""

function edit_text_converge(t)
    while true
        new_t = edit_text(t)[2]
        t == new_t && return new_t
        t = new_t
    end
end

@test edit_text_converge("""
@compat (::Type{Array{T,N}}){T,N}(a::AFArray{T,N}) = convert(Array{T,N}, a)
""") == """
Array{T,N}(a::AFArray{T,N}) where {T,N} = convert(Array{T,N}, a)
"""

@test edit_text_converge("""
@compat (::Type{Array}){T,N}(a::AFArray{T,N}) = convert(Array{T,N}, a)
""") == """
Array(a::AFArray{T,N}) where {T,N} = convert(Array{T,N}, a)
"""

@test edit_text("""
mean(d::MvLogNormal) = @compat(exp.(mean(d.normal) + var(d.normal)/2))

\"\"\"
Some doc
\"\"\"
nothing
""")[2] == """
mean(d::MvLogNormal) = exp.(mean(d.normal) + var(d.normal)/2)

\"\"\"
Some doc
\"\"\"
nothing
"""

@test edit_text("""
@compat abstract type FooBar end
""", [Deprecations.dep_for_vers(
    Deprecations.ObsoleteCompatMacro,
    Pkg.Reqs.parse(IOBuffer("julia 0.5"))
)])[2] == """
@compat abstract type FooBar end
"""

@test edit_text("""
@compat abstract type FooBar end
""", [Deprecations.dep_for_vers(
    Deprecations.ObsoleteCompatMacro,
    Pkg.Reqs.parse(IOBuffer("julia 0.6"))
)])[2] == """
abstract type FooBar end
"""

@test edit_text("""
(::Type{A})(a, b) where A<:Array
""")[2] == """
(::Type{A})(a, b) where A<:Array
"""

@test edit_text("""
(::Type{A})(a, b::B) where B<:Array
""")[2] == """
A(a, b::B) where B<:Array
"""

@test edit_text("""
immutable Foo
    primitive::Int
end
""")[2] == """
struct Foo
    primitive::Int
end
"""

@test edit_text("""
bitstype 32 \$typename <: CEnum.Cenum{UInt32}
""")[2] == """
primitive type \$typename <: CEnum.Cenum{UInt32} 32 end
"""

@test edit_text("""
typealias SpvId UInt32
""")[2] == """
const SpvId = UInt32
"""

@test edit_text("""
function widget{T<:Colorant}(colormap::VecTypes{T}, window;
        area = (300, 30),
        slider_colors = (
            RGBA{Float32}(0.78125,0.1796875,0.41796875),
            RGBA{Float32}(0.41796875,0.78125,0.1796875),
            RGBA{Float32}(0.1796875,0.41796875,0.78125),
            RGBA{Float32}(0.9,0.9,0.9)
        ),
        knob_scale = 9f0,
        kw_args...
    )
    nothing
end
""")[2] == """
function widget(colormap::VecTypes{T}, window;
        area = (300, 30),
        slider_colors = (
            RGBA{Float32}(0.78125,0.1796875,0.41796875),
            RGBA{Float32}(0.41796875,0.78125,0.1796875),
            RGBA{Float32}(0.1796875,0.41796875,0.78125),
            RGBA{Float32}(0.9,0.9,0.9)
        ),
        knob_scale = 9f0,
        kw_args...
    ) where T<:Colorant
    nothing
end
"""

@test edit_text("""
function (x::FacElemMon{S}){S}()
    z = FacElem{elem_type(S), S}()
    z.fac = Dict{elem_type(S), fmpz}()
    z.parent = x
    return z
end
""")[2] == """
function (x::FacElemMon{S})() where S
    z = FacElem{elem_type(S), S}()
    z.fac = Dict{elem_type(S), fmpz}()
    z.parent = x
    return z
end
"""

@test edit_text("""
struct S{T}
    foo
    S{S}(v::Vector{S}) = 42
end
""")[2] == """
struct S{T}
    foo
    S{T}(v::Vector{S}) where {T, S} = 42
end
"""

@test edit_text("""
struct S{T}
    foo
    S{T}(v::Vector{T}) = 42
end
""")[2] == """
struct S{T}
    foo
    S{T_}(v::Vector{T}) where {T_, T} = 42
end
"""

@test edit_text("""
struct S{T}
    foo
    S(v::Vector{T}) = 42
end
""")[2] == """
struct S{T}
    foo
    S{T}(v::Vector{T}) where {T} = 42
end
"""

text_not_edited(t) = edit_text(t)[2] == t

@test text_not_edited("""
struct GLVisualizeShader <: AbstractLazyShader
    paths::Tuple
    kw_args::Dict{Symbol, Any}
    function GLVisualizeShader(paths::String...; view = Dict{String, String}(), kw_args...)
        nothing
    end
end
""")

@test text_not_edited("""
function foo end
""")

@test text_not_edited("""
struct LabelMap{K}
    vs::Vector{K}
    v2i::Dict{K,Int}

    function LabelMap{K}(vs, v2i) where K
        nothing
    end
end
""")

@test edit_text("""
function fit_mle{T <: Real}(::Type{EmpiricalUnivariateDistribution},
                           x::Vector{T})
end
""")[2] == """
function fit_mle(::Type{EmpiricalUnivariateDistribution},
                x::Vector{T}) where T <: Real
end
"""

@test edit_text("""
begin
    if VERSION > v"0.1.0"
        a
        b
    else
        c
    end
end
""")[2] == """
begin
    a
    b
end
"""

@test edit_text("""
begin
    if VERSION < v"0.1.0"
        a
        b
    else
        c
        d
    end
end
""")[2] == """
begin
    c
    d
end
"""

@test edit_text("""
begin
    begin
        if VERSION > v"0.1.0"
            a
            b
        else
            c
            d
        end
    end
end
""")[2] == """
begin
    begin
        a
        b
    end
end
"""

@test edit_text_converge("""
struct ReshapedOneD{T,N,Npre,V}
    data::V

    function (::Type{ReshapedOneD{T,N,Npre,V}}){T,N,Npre,V}(data::V)
    end
end
""") == """
struct ReshapedOneD{T,N,Npre,V}
    data::V

    function ReshapedOneD{T,N,Npre,V}(data::V) where {T,N,Npre,V}
    end
end
"""