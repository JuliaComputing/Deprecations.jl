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