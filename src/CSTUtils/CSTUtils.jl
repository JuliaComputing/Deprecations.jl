module CSTUtils
    export OverlayNode
    export function_def_call, isexpr, text, trailing_trivia, leading_trivia,
        span_text, fullspan_text

    include("treewalking.jl")

    function text(o::OverlayNode, leading_trivia = true, trailing_trivia = true)
        o.buffer[((leading_trivia ? first(o.fullspan) : first(o.span)):(trailing_trivia ? last(o.fullspan) : last(o.span)))]
    end

    function trailing_trivia(o::OverlayNode)
        o.buffer[(1+last(o.span):last(o.fullspan))]
    end

    function leading_trivia(o::OverlayNode)
        o.buffer[(first(o.fullspan):first(o.fullspan)-1)]
    end

    span_text(o::OverlayNode) = text(o, false, false)
    fullspan_text(o::OverlayNode) = text(o, true, true)

    isexpr(x::EXPR{T}, ::Type{S}) where {T, S} = T == S

    isexpr(x::T, ::Type{T}) where {T} = true
    isexpr(x::T1, ::Type{T2}) where {T1, T2} = false
    # Operator
    isexpr(x::CSTParser.OPERATOR, o::Type{CSTParser.OPERATOR}, op::Tokens.Kind) = x.kind == op && x.dot == false
    isexpr(x, o::Type{CSTParser.OPERATOR}, op::Tokens.Kind) = false
    # Keyword
    isexpr(x::CSTParser.KEYWORD, k::Type{CSTParser.KEYWORD}, kw::Tokens.Kind) = x.kind == kw
    isexpr(x, k::Type{CSTParser.KEYWORD}, kw::Tokens.Kind) = false
    # Literal
    isexpr(x::CSTParser.LITERAL, k::Type{CSTParser.LITERAL}, lit::Tokens.Kind) = x.kind == lit
    isexpr(x, k::Type{CSTParser.LITERAL}, lit::Tokens.Kind) = false

    isexpr(x::CSTParser.PUNCTUATION, k::Type{CSTParser.PUNCTUATION}, lit::Tokens.Kind) = x.kind == lit
    isexpr(x, k::Type{CSTParser.PUNCTUATION}, lit::Tokens.Kind) = false

    isexpr(x::OverlayNode, ::Type{S}, kind::Tokens.Kind) where {S} = isexpr(x.expr, S, kind)
    isexpr(x::OverlayNode, ::Type{CSTParser.OPERATOR}, kind::Tokens.Kind)  = isexpr(x.expr, CSTParser.OPERATOR, kind)
    isexpr(x::OverlayNode, ::Type{CSTParser.KEYWORD}, kind::Tokens.Kind)  = isexpr(x.expr, CSTParser.KEYWORD, kind)
    isexpr(x::OverlayNode, ::Type{CSTParser.LITERAL}, kind::Tokens.Kind) = isexpr(x.expr, CSTParser.LITERAL, kind)
    isexpr(x::OverlayNode, ::Type{CSTParser.PUNCTUATION}, kind::Tokens.Kind) = isexpr(x.expr, CSTParser.PUNCTUATION, kind)
    isexpr(x::OverlayNode, ::Type{S}) where {S} = isexpr(x.expr, S)
end
