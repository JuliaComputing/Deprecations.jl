using Deprecations
using Deprecations: edit_text
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

edit_text("""
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

edit_text("f{T}(x::T) = x")[2] == "f(x::T) where {T} = x"

edit_text("""
if VERSION >= v"0.1.0"
    do_magic()
end
""")[2] == "do_magic()"

edit_text("@compat(Union{Int64,Float64})")[2] == "Union{Int64,Float64}"
edit_text("@compat Union{Int64,Float64}")[2] == "Union{Int64,Float64}"
