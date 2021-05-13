import java.io.File

class rectangle(private val width: Int, private val height: Int) {
    private val arr: Array<Array<Char>>

    init {
        arr = Array(height) { Array(width) { ' ' } }
    }

    fun show() {
        arr.forEach {
            it.forEach { cell ->
                print(cell)
            }
            println("")
        }
        println("")
    }

    fun rect(width: Int, height: Int) {
        for (row in 0 until height) {
            for (col in 0 until width) {
                arr[row][col] = '#'
            }
        }
    }

    fun rotateColumn(col: Int, by: Int) {
        val columnBefore = Array(height) { i -> arr[i][col] }
        for (row in 0 until height) {
            arr[(row + by) % height][col] = columnBefore[row]
        }
    }

    fun rotateRow(row: Int, by: Int) {
        val rowBefore = Array(width) { i -> arr[row][i] }
        for (col in 0 until width) {
            arr[row][(col + by) % width] = rowBefore[col]
        }
    }

    fun count(c: Char) = arr.sumBy { it.count { cell -> cell == c } }
}

val regexRect = """rect (\d+)x(\d+)""".toRegex()
val regexCol = """rotate column x=(\d+) by (\d+)""".toRegex()
var regexRow = """rotate row y=(\d+) by (\d+)""".toRegex()
fun applyLine(r: rectangle, l: String) {
    when {
        l.startsWith("rect ") -> {
            val (width, height) = regexRect.find(l)!!.destructured
            r.rect(width.toInt(), height.toInt())
        }
        l.startsWith("rotate row ") -> {
            val (row, by) = regexRow.find(l)!!.destructured
            r.rotateRow(row.toInt(), by.toInt())
        }
        l.startsWith("rotate column ") -> {
            val (col, by) = regexCol.find(l)!!.destructured
            r.rotateColumn(col.toInt(), by.toInt())
        }
        else -> println("unhandled line $l")
    }
}

val r = rectangle(50, 6)
File("input8.txt").forEachLine { line -> applyLine(r, line) }
r.show()
println("Pixels on: ${r.count('#')}")
