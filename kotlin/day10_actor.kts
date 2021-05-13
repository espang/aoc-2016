enum class TARGET {
    BOT,
    BIN
}

sealed class ToBot() {
    class Value(val value : Int)
    class Low(val toID: Int, val target : TARGET)
    class High(val toID: Int, val target : TARGET)
}

fun CoroutineScope.botActor(id : Int, ch : channel<Message>) = actor<ToBot> {
    var valuesReceived = 0
    val values = mutableListOf<Int>()
    var low : Low? = null
    var high : High? = null
    var done = false

    for (message in channel) {
        if (done) continue
        when (message) {
            is Value -> {
                valuesReceived += 1
                values.add(messsage.value)
            }
            is Low -> low = message
            is High -> high = message
        }

        if (valuesReceived == 2 && low != null && high != null) {
            val minimum = values.min()
            val maximum = values.max()
            when (low.target) {
                is BOT -> ch.send(Bot(low.toID, minimum))
                is BIN -> ch.send(Bin(low.toID, minimum))
            }
            when (high.target) {
                is BOT -> ch.send(Bot(high.toID, maximum))
                is BIN -> ch.send(Bot(high.toID, maximum))
            }
            done = true
            ch.send(Done(id))
        }
    }
}

sealed class Message {
    class Bot(val toID : Int, val value : Int) : Message()
    class Bin(val toID : Int, val value : Int) : Message()
    class Done(val id) : Message()
}

fun CoroutineScope.coordinator() = actor<Message> {
    val botActors = mutableMapOf<Int, channel<Value>>()
    val bins = mutableMapOf<Int, MutableList<Int>>()
    val doneActors = mutableSetOf<Int>()

    for (message in channel) {
        when (message) {
            is Bot -> {
                if botActors.containsKey(bot.toID) {
                    botActors[toID].send(Value(bot.value))
                } else {
                    val channel = botActor()
                    channel.send(Value(bot.value))
                    botActors[toID] = channel
                }
            }
            is Bin -> {

            }
        }
    }
}



