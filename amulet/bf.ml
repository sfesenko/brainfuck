open import "prelude.ml"

let fst (a, _) = a
let snd (_, b) = b
let ( % ) x y = x - (y * (x // y))

module Lua =
    external val io_write: int -> ()  = "function(c) io.write(string.char(c)) io.flush() end"
    external val io_read: () -> int = "function() return io.read(1):byte() end"
    external val get: 'a -> int -> string = "function(t, i) return t[i] end"
    external val size: 'a -> int = "function(t) return table.getn(t) end"

module Tape =
    open import "data/array.ml"

    private type t = T of { data: array int, pos: int }
    let create () = T { data = make 1 0, pos = 0 }

    let current (T t) = 
        t.data.(t.pos)

    let put (T t) ch =
        t.data.[t.pos] <- ch

    let inc t delta = 
        (current t + delta) % 256 |> put t

    let move (T t) delta =
        let newPos = t.pos + delta
        let newData =  if size t.data > newPos then t.data else make newPos 0 |> append t.data
        T { data = newData, pos = t.pos + delta }

module BF =
    type codes = 
        | Inc of int
        | Move of int
        | Print
        | Input
        | Loop of list codes 
        | Ignore
        | Zero

    module String = include import "lua/string.ml"

    module Op =
        let inc = Inc 1
        let dec = Inc -1
        let right = Move 1
        let left = Move -1
        let print = Print
        let input = Input
        let ignore = Ignore
        let zero = Zero

    let parse s = 
        let rec parse_ s i res = 
            match String.substring s i i with
            | "" | "]" -> (reverse res, i)
            | "[" -> 
                let (codes, j) = parse_ s (i + 1) []
                parse_ s (j + 1) (Loop codes :: res)
            | ch -> 
                let cmd = match ch with
                | "+" -> Op.inc
                | "-" -> Op.dec
                | ">" -> Op.right
                | "<" -> Op.left
                | "." -> Op.print
                | "," -> Op.input
                (* | x -> error("Wrong code " ^ x) *)
                | _ -> Op.ignore
                parse_ s (i + 1) (cmd :: res)
        parse_ s 1 [] |> fst

    let run tape program = 
        let rec run_ tape = function
            | [] -> tape
            | Cons(op, rest) -> 
                match op with
                | Inc delta ->
                    Tape.inc tape delta
                    run_ tape rest
                | Move delta ->
                    run_ (Tape.move tape delta) rest
                | Print ->
                    Tape.current tape |> Lua.io_write
                    run_ tape rest
                | Loop code -> 
                    let rec loop tp =
                        if Tape.current tp > 0 then
                            run_ tp code |> loop
                        else
                            tp
                    run_ (loop tape) rest
                | Input ->
                    Lua.io_read() |> Tape.put tape
                    run_ tape rest
                | Zero ->
                    Tape.put tape 0
                    run_ tape rest
                | _ ->
                    run_ tape rest
        run_ tape program

    let start = 
        Tape.create() |> run


    let rec optimize program =
        let rec optimize_ res = function
        | [] -> reverse res
        | Cons(Inc a, Cons (Inc b, tail)) -> optimize_ (Inc (a + b) :: res) tail
        | Cons(Move a, Cons (Move b, tail)) -> optimize_ (Move (a + b) :: res) tail
        | Cons(Inc 0, tail)
        | Cons(Move 0, tail)
        | Cons(Loop [], tail) -> optimize_ res tail
        | Cons(Loop [Inc _], tail) -> optimize_ (Op.zero :: res) tail
        | Cons(Loop code, tail) ->
            let code_ = optimize code
            optimize_ (Loop code_ :: res) tail
        | Cons(x, tail) -> optimize_ (x :: res) tail
        let optimized = optimize_ [] program
        if (length optimized) == (length program) then optimized else optimize optimized


module Program =
    module IO = include import "lua/io.ml"
    let main arg =
        if Lua.size(arg) < 1 then
            print "bf <brainfuck.bf>"
        else
            let file_name = Lua.get arg 1
            file_name
                |> IO.open_for_reading
                |> IO.read_all
                |> function 
                | Some c -> 
                    let _ = c |> BF.parse |> BF.optimize |> BF.start
                    ()
                | _ ->
                    "Cannot read file '" ^ file_name ^ " '" |> print 

external val arg: 'a = "arg"
let _ = Program.main arg

