module Main

import StdEnv
// import Aux


:: Code = 
    Inc 
  | Dec 
  | Right 
  | Left 
  | Print
  // | Input 
  | Loop [Code]

///
parse :: [Char] -> [Code]
parse chars = codes
where
  parse1 :: [Char] [Code] -> ([Char], ![Code])
  parse1 [] acc = ([], reverse acc)
  parse1 [c:cs] acc =
      case c of
          '+'       -> parse1 cs [Inc:acc]
          '-'       -> parse1 cs [Dec:acc]
          '>'       -> parse1 cs [Right:acc]
          '<'       -> parse1 cs [Left:acc]
          '.'       -> parse1 cs [Print:acc ]
          '['       -> parse1 newCs [Loop loop:acc]
                      where (newCs, loop) = parse1 cs []
          ']'       -> (cs, reverse acc)
          _         -> parse1 cs acc
  (_, codes) = parse1 chars []
///

///
:: Tape = { pos :: !Int, data :: !.{#Int} }

:: St :== (!.File, !.Tape)
:: Op :== *St -> .St

parse2 :: [Char] -> Op
parse2 chars = fun
where
  move :: !*Tape Int -> .Tape
  move tape=:{ pos = pos } step = { tape & pos = pos + step }
  //
  add :: !*Tape Int -> .Tape
  add t=:{ pos = i, data = d } dx = { t & data.[i] = d.[i] + dx}
  ///
  right :: !*St -> .St
  right (file, tape) = (file, move tape 1)
  //
  left :: !*St -> .St
  left (file, tape) = (file, move tape -1)
  //
  inc :: !*St -> .St
  inc (file, tape) = (file, add tape 1)
  //
  dec :: !*St -> .St
  dec (file, tape) = (file, add tape -1)
  //
  print :: !*St -> .St
  print (file, tape=:{pos = i, data = d})
  # file = fwritec (toChar d.[i]) file
  # (_, file) = fflush file
  = (file, tape)
  //
  loop :: !Op !*St -> .St
  loop op st=:(file, tape=:{pos = i, data = d}) = R
  where
    R = if (d.[i] == 0) st (loop op (op st))

  parseR :: [Char] !Op -> ([Char], !Op)
  parseR [] op = ([], op)
  
  parseR ['[' : tail] f
  # (tail, f1) = parseR tail id
  = parseR tail ((loop f1) o f) 

  parseR [']' : tail] f = (tail, f)
  parseR ['.' : tail] f = parseR tail (print o f)
  parseR ['+' : tail] f = parseR tail (inc o f)
  parseR ['-' : tail] f = parseR tail (dec o f)
  parseR ['>' : tail] f = parseR tail (right o f)
  parseR ['<' : tail] f = parseR tail (left o f)
  parseR [  _ : tail] f = parseR tail f

  (_, fun) = parseR chars id

run2 :: !Op !*Tape !*File -> *(!*Tape, !*File)
run2 op tape file
  # (file, tape) = op (file, tape)
  = (tape, file)

newTape :: .Tape
newTape = { pos = 0, data = createArray 8192 0 }

move :: Int !*Tape -> .Tape
move step tape=:{ pos = pos } = { tape & pos = pos + step }

add :: !*Tape Int -> .Tape
add tape=:{ pos = pos, data = data } dx
  # (elem, data) = data![pos]
  # data & [pos] = (elem + dx + 256) rem 256
  // # data & [pos] = elem + dx
  = {tape & data = data}

current :: !*Tape -> (!Int, !*Tape)
current tape=:{ pos = pos, data = data } 
  # (elem, data) = data![pos]
  # tape & data = data
  = (elem, tape)

run :: ![Code] !*Tape !*File -> (!*Tape, !*File)
run [] tape io = (tape, io)
run [c : t] tape io = result
where
  runLoop :: ![Code] (!*Tape, !*File) -> (!*Tape, !*File)
  runLoop ops (tape=:{pos = pos, data = data}, io)
    # c = data.[pos]
    = if (c == 0) (tape, io) (runLoop ops (run ops tape io))

  print :: !*Tape !*File -> (!*Tape, !*File)
  print tape=:{pos = pos, data = data} io
    # c = data.[pos]
    # io = fwritec (toChar c) io
    # (_, io) = fflush io
    = (tape, io)

  runCode :: !Code !*Tape !*File -> (!*Tape, !*File)
  runCode (Loop ops) tape io = runLoop ops (tape, io)
  runCode Inc tape io = (add tape 1, io)
  runCode Dec tape io = (add tape -1, io)
  runCode Right tape io = (move 1 tape, io)
  runCode Left tape io = (move -1 tape, io)
  runCode Print tape io = print tape io
  // runCode op tape io = 
  //   case c of
  //     Inc   -> (add tape 1, io)
  //     Dec   -> (add tape -1, io)
  //     Right -> (move 1 tape, io)
  //     Left  -> (move -1 tape, io)
  //     Loop ops -> runLoop ops (tape, io)
  //     Print -> print tape io
  //     _ -> (tape, io)
  (t1, io1) = runCode c tape io
  result = run t t1 io1


Start :: *World -> .World
Start world
  # (io, world) = stdio world
  # (ok, f, world) = fopen "input.b" FReadText world
  # (input, f) = freads f 65535
  # (ok, world) = fclose f world
  # tape = newTape
  # inputl = [ elem \\ elem <-: input ]

  // # cod = parse inputl
  // # (tape, io) = run cod tape io

  # ops = parse2 inputl
  # (tape, io) = run2 ops tape io

  # (_, io) = fflush io

  # (c, tape) = current tape

  # { pos = pos, data = data } = tape
  # io = fwrites (toString pos) io
  # io = fwrites "\n" io
  # io = fwrites (toString c) io

  # (ok, world) = fclose io world
  = world
