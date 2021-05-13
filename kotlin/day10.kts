import java.io.File

sealed class Action {
    data class Value(val v : Int, val bot : Int) : Action()
    data class Bot(
        val id : Int,
        val low : Int,
        val lowToBot: Boolean,
        val high : Int,
        val highToBot: Boolean) : Action()
}

val regexValue = """value (\d+) goes to bot (\d+)""".toRegex()
val regexBot = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".toRegex()
fun parseLine(s : String) : Action {
    when {
        s.startsWith("value") -> {
            val (v, bID) = regexValue.find(s)!!.destructured
            return Action.Value(v.toInt(), bID.toInt())
        }
        s.startsWith("bot") -> {
            val (bID, low, lowID, high, highID) = regexBot.find(s)!!.destructured
            return Action.Bot(
                bID.toInt(),
                lowID.toInt(),
                low == "bot",
                highID.toInt(),
                high == "bot"
            )
        }
        else -> throw Exception("unhandled line: $s")
    }
}

object output {
    private val bins = mutableMapOf<Int,Int>()

    fun add(value : Int, bin : Int) {
        bins.put(bin, value)
    }

    fun get(bin: Int) = bins.getOrDefault(bin, 0)
}

object bots {
    private val bots = mutableMapOf<Int, bot>()

    fun register(bID: Int, b: bot) {
        if (!bots.containsKey(bID)) {
            bots.put(bID, b)
        }
    }

    fun getBot(bID: Int) = bots.getOrPut(bID) { bot(bID) }

    fun findBotWith(low : Int, high : Int) : Int {
        val chips = setOf(low, high)
        for ((i, bot)in bots) {
            if (bot.hadChips(chips))
                return i
        }
        return -1
    }
}

class bot(val id : Int) {
    private var action : Action.Bot? = null
    private val chips = mutableSetOf<Int>()
    private var hasChips = true

    override fun toString() = "bot($id, $action, $hasChips, $chips)"

    fun hadChips(c : Set<Int>) = !hasChips && c.equals(chips)

    fun addAction(ba : Action.Bot) { action = ba }

    fun give(value : Int) {
        if (chips.size == 2)
            throw Exception("bot $id got more than 2 chips")
        chips.add(value)
    }

    fun trigger() {
        val action = action
        if (hasChips && chips.size == 2 && action != null) {
            val lower : Int = chips.minOrNull() ?: 0
            val higher : Int = chips.maxOrNull() ?: 0

            if (action.highToBot) {
                bots.getBot(action.high).give(higher)
                bots.getBot(action.high).trigger()
            } else {
                output.add(higher, action.high)
            }

            if (action.lowToBot) {
                bots.getBot(action.low).give(lower)
                bots.getBot(action.low).trigger()
            } else {
                output.add(lower, action.low)
            }
            hasChips = false
        }
    }
}

fun applyActions(a : List<Action>) {
    a.forEach {
        action : Action ->
        when (action) {
            is Action.Value -> {
                bots.register(action.bot, bot(action.bot))
                bots.getBot(action.bot).give(action.v)
                bots.getBot(action.bot).trigger()
            }
            is Action.Bot -> {
                bots.register(action.id, bot(action.id))
                bots.getBot(action.id).addAction(action)
                bots.getBot(action.id).trigger()
            }
        }
    }
}

val actions = mutableListOf<Action>()
File("input10.txt").forEachLine {
    actions.add(parseLine(it))
}
applyActions(actions)
println("bot: ${bots.findBotWith(17, 61)}")
val part2 = output.get(0) * output.get(1) * output.get(2)
println("part2: ${output.get(0)} * ${output.get(1)} * ${output.get(2)} = $part2")