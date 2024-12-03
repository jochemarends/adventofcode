import java.io.File

data class Token(val lexeme: String, val type: T) {
    enum class Type {
        MUL, PAUSE, RESUME, // commands
        LPAREN, RPAREN,     // grouping
        NUMBER,
        COMMA,
        CORRUPT
    }
}

typealias T = Token.Type

tailrec fun tokenize(input: String, toks: List<Token> = emptyList<Token>()): List<Token> {
    if (input.isEmpty()) return toks 
    val tok = when (val c = input.first()) {
        ',' -> Token(",", T.COMMA)
        '(' -> Token("(", T.LPAREN)
        ')' -> Token(")", T.RPAREN)
        else -> {
            val digits = input.takeWhile { it.isDigit() }
            when {
                input.startsWith("mul") -> Token("mul", T.MUL)
                input.startsWith("don't") -> Token("don't", T.PAUSE)
                input.startsWith("do") -> Token("do", T.RESUME)
                digits.length in 1..3 -> Token(digits, T.NUMBER)
                else -> Token(c.toString(), T.CORRUPT)
            }
        }
    }
    return tokenize(input.drop(tok.lexeme.length), toks + tok)
}

enum class Part {
    ONE, TWO
}

tailrec fun compute(input: List<Token>, part: Part): Int {
    return when {
        input.isEmpty() -> 0
        part == Part.TWO && input.first().type == T.PAUSE -> {
            return input
                .dropWhile { it.type != T.RESUME }
                .let { compute(it, part) }
        }
        input.take(6).map { it.type } == listOf(T.MUL, T.LPAREN, T.NUMBER, T.COMMA, T.NUMBER, T.RPAREN) -> {
            return input
                .take(6)
                .filter { it.type == T.NUMBER }
                .map { it.lexeme.toInt() }
                .reduce { acc, v -> acc * v }
                .let { it + compute(input.drop(6), part) }
        }
        else -> compute(input.drop(1), part)
    }
}

val input = File("./input.txt").readText()
print(compute(tokenize(input), Part.ONE))
print(compute(tokenize(input), Part.TWO))

