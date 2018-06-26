foo() = 1
const bar = 2
# const_b has a function scope where the const on the previous line is
# We test here that we don't confuse the two files
include("const_b.jl")