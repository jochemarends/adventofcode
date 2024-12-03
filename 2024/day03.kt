import java.io.File

data class Token(val lexeme: String, val type: Type) {
    enum class Type {
        MUL, PAUSE, RESUME, // commands
        LPAREN, RPAREN,     // grouping
        NUMBER,
        COMMA,
        CORRUPT
    }
}

typealias T = Token.Type

tailrec fun tokenize(input: String, toks: MutableList<Token> = mutableListOf()): List<Token> {
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
    return tokenize(input.drop(tok.lexeme.length), toks.apply { add(tok) })
}

enum class Part {
    ONE, TWO
}

tailrec fun compute(input: List<Token>, part: Part, sum: Int = 0): Int {
    return when {
        input.isEmpty() -> sum
        part == Part.TWO && input.first().type == T.PAUSE -> {
            val leftovers = input
                .dropWhile { it.type != T.RESUME }
            return compute(leftovers, part, sum)
        }
        input.take(6).map { it.type } == listOf(T.MUL, T.LPAREN, T.NUMBER, T.COMMA, T.NUMBER, T.RPAREN) -> {
            val term = input
                .take(6)
                .filter { it.type == T.NUMBER }
                .map { it.lexeme.toInt() }
                .reduce { acc, v -> acc * v }
            return compute(input.drop(6), part, sum + term)
        }
        else -> compute(input.drop(1), part, sum)
    }
}

fun main() {
    val input = File("./input.txt").readText()
    println("part 1: ${compute(tokenize(input), Part.ONE)}")
    println("part 2: ${compute(tokenize(input), Part.TWO)}")
}

