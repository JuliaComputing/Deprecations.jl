using Test
import Deprecations
import Tokenize.Tokens
import CSTParser: KEYWORD, OPERATOR, LITERAL
##########
# isexpr #
##########
let
    expr = CSTParser.parse("if true elseif false end")
    elseif_arg = expr.args[4]
    @test Deprecations.isexpr(elseif_arg, KEYWORD, Tokens.ELSEIF)

    expr = CSTParser.parse("true")
    @test Deprecations.isexpr(expr, LITERAL, Tokens.TRUE)

    expr = CSTParser.parse("+")
    @test Deprecations.isexpr(expr, OPERATOR, Tokens.PLUS)
end


let
    x = CSTParser.parse("foo")
    y = CSTParser.parse(":foo")
    @test !Deprecations.matches_template(x, y)
end

let
    x = CSTParser.parse("foo")
    y = CSTParser.parse("foo")
    @test Deprecations.matches_template(x, y)
end

let
    x = CSTParser.parse("\$name")
    @test Deprecations.is_template_expr(x) == (true, :name, false)

    x = CSTParser.parse("using name...")
    @test Deprecations.is_template_expr(x) == (false, nothing, false)

    x = CSTParser.parse("\$BODY...")
    @test Deprecations.is_template_expr(x) == (true, :BODY, true)

    x = CSTParser.parse("::Type{\$NAME{\$T...}}")
    @test Deprecations.is_template_expr(x) == (false, nothing, false)
end


# VERSIONCOMPARE
let
    x = CSTParser.parse("VERSION")
    y = CSTParser.parse("v\"0.2.0\"")
    @test Deprecations.detect_ver_arguments(x, y) == v"0.2.0"

    x = CSTParser.parse("foo")
    @test Deprecations.is_identifier(x, "foo") == true
end

# OLDCOMPAT
let
    x = CSTParser.parse("f(x::T) where{T}")
    @test Deprecations.is_where_expr(x)
end

