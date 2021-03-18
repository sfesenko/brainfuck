package brainfuck

import java.io.InputStream
import java.io.OutputStream
import kotlin.system.exitProcess

sealed class Op {
    data class Add(val v: Int) : Op()
    data class Move(val v: Int) : Op()
    object Input : Op()
    data class Print(val n: Int) : Op()
    data class Load(val v: Int) : Op()
    data class Loop(val code: List<Op>) : Op()

    companion object {
        val FORTH = Move(1)
        val BACK = Move(-1)
        val INC = Add(1)
        val DEC = Add(-1)
        val PRINT = Print(1)
        val INPUT = Input
        val SET0 = Load(0)
    }
}

fun parse(s: String): List<Op> {
    fun parse1(start: Int): Pair<List<Op>, Int> {
        val codes = mutableListOf<Op>()
        var i = start
        while (i in s.indices) {
            val op = when (s[i]) {
                '>' -> Op.FORTH
                '<' -> Op.BACK
                '+' -> Op.INC
                '-' -> Op.DEC
                '.' -> Op.PRINT
                ',' -> Op.INPUT
                '[' -> {
                    val (ops, newI) = parse1(i + 1)
                    i = newI
                    Op.Loop(ops)
                }
                ']' -> {
                    assert(start > 0) { "Unexpected end of Loop" }
                    return codes to i
                }
                else -> null
            }
            if (op != null) {
                codes += op
            }
            i += 1
        }
        assert(start == 0) { "Unexpected end of program (inside Loop)" }
        return codes to i
    }
    return parse1(0).first
}


class Tape {
    private var pos: Int = 0
    private var data = byteArrayOf(0)

    private fun checkResize() {
        if (pos >= data.size) {
            data = data.copyOf(pos * 2)
        }
    }

    fun add(n: Int) {
        current = (current + n)
    }

    fun move(n: Int) {
        pos += n
    }

    var current
        get() = (data.getOrNull(pos) ?: 0).toInt()
        set(n) {
            checkResize()
            data[pos] = n.toByte()
        }
}

fun run(tape: Tape, input: InputStream, output: OutputStream, program: List<Op>) {
    fun print(n: Int, v: Int) {
        repeat(n) { output.write(v) }
        output.flush()
    }
    for (op in program) {
        when (op) {
            is Op.Add -> tape.add(op.v)
            is Op.Move -> tape.move(op.v)
            is Op.Print -> print(op.n, tape.current)
            is Op.Input -> tape.current = input.read()
            is Op.Loop -> while (tape.current != 0) run(tape, input, output, op.code)
            is Op.Load -> tape.current = op.v
        }
    }
}

tailrec fun optimize(code: List<Op>): List<Op> {
    var i = 0
    val result = mutableListOf<Op>()
    while (i in code.indices) {
        val c = code[i]
        val p = result.lastOrNull()
        when {
            c is Op.Add && c.v == 0 -> {
            }
            c is Op.Move && c.v == 0 -> {
            }
            c is Op.Loop && c.code.size == 1 && c.code[0] is Op.Add -> {
                result += Op.SET0
            }
            c is Op.Loop && c.code.size == 1 && c.code[0] is Op.Loop -> {
                result += c.code[0]
            }
            p is Op.Load && p.v == 0 && c is Op.Loop -> {
            }
            c is Op.Add && p is Op.Add -> {
                result.removeLast()
                result += Op.Add(c.v + p.v)
            }
            c is Op.Move && p is Op.Move -> {
                result.removeLast()
                result += Op.Move(c.v + p.v)
            }
            (p is Op.Add || p is Op.Input) && (c is Op.Load || c is Op.Input) -> {
                result.removeLast()
                result += c
            }
            p is Op.Load && c is Op.Add -> {
                result.removeLast()
                result += Op.Load(p.v + c.v)
            }
            p is Op.Print && c is Op.Print -> {
                result.removeLast()
                result += Op.Print(p.n + c.n)
            }
            c is Op.Loop -> {
                val ops = optimize(c.code)
                result += Op.Loop(ops)
            }
            else -> result += c
        }
        i += 1
    }
    return if (result == code) code else optimize(result)
}

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("bf <brainfuck.bf>")
        exitProcess(1)
    }
    val f = java.io.File(args[0])
    val text = f.readText()
    val code = parse(text)
    val optimized = optimize(code)
    val tape = Tape()
    run(tape, System.`in`, System.out, optimized)
}
