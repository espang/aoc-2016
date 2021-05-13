import java.io.File

var content = File("input9.txt").readText(Charsets.UTF_8)

data class Token(val data: String, val times: Long = 1L)

fun lengthOf(tokens: List<Token>) = tokens.fold (0L) {
    acc, t ->
    acc + t.times * t.data.length
}

val regexMarker = """^\((\d+)x(\d+)\)(.+)""".toRegex()
fun parseMarker(s: String): Pair<Token?, String> {
    val match = regexMarker.find(s)
    if (match == null) {
        return Pair(null, s)
    }
    val (nbr, times, rest) = match.destructured
    val numberOfCharacters = nbr.toInt()
    return Pair(
        Token(rest.substring(0..numberOfCharacters - 1), times.toLong()),
        rest.substring(numberOfCharacters)
    )
}

fun parseToken(s: String): Pair<Token, String> {
    val (result, rest) = parseMarker(s)
    if (result != null) {
        return Pair(result, rest)
    }

    val firstParens = s.indexOf('(')
    when (firstParens) {
        -1 -> return Pair(Token(s), "")
        0 -> return Pair(
            Token(s.substring(0..firstParens)), s.substring(firstParens + 1),
        )
        else -> return Pair(
            Token(s.substring(0..firstParens - 1)), s.substring(firstParens),
        )
    }
}

fun parse(txt: String): List<Token> {
    val result = mutableListOf<Token>()
    var s = txt
    while (s.isNotEmpty()) {
        val (t, rest) = parseToken(s)
        result.add(t)
        s = rest
    }
    return result
}

/**
 * This algorithm only works because "inner" markers are smaller and
 * don't overlap. For example "(27x12)(20x12)(13x14)(7x10)(1x12)A" can
 * be reduced to:
 *  12 * "(20x12)(13x14)(7x10)(1x12)A"
 *  12 * 12 * "(13x14)(7x10)(1x12)A"
 *  12 * 12 * 14 * "(7x10)(1x12)A"
 *  12 * 12 * 14 * 10 * "(1x12)A"
 *  12 * 12 * 14 * 10 * 12 * "A"
 *  The following wouldn't work when any of the inner markers would
 *  go over the outer "token".
 */
fun decompressedLength(token: Token): Long {
    val tokens = parse(token.data)
    if (tokens.size == 1 && tokens[0].data == token.data) {
        return token.data.length * token.times
    }

    return tokens.fold(0L) { acc, t ->
        acc + token.times * decompressedLength(t)
    }
}


val tokens = parse(content)
println("length: ${lengthOf(tokens)}")
println("length2: ${decompressedLength(Token(content))}")

