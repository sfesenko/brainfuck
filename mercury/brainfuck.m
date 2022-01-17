:- module brainfuck
.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string, char, array, int, uint8.

:- type opcode
  ---> nop
    ; inc
    ; dec
    ; forth
    ; back
    ; read
    ; write
    ; loop(list(opcode))
    .

:- pred tr(char::in, opcode::out) is semidet.

tr('+', inc).
tr('-', dec).
tr('>', forth).
tr('<', back).
tr(',', read).
tr('.', write).

:- pred translate(list(char)::in, list(char)::out, list(opcode)::in, list(opcode)::out) is det.
translate([], [], Code, list.reverse(Code)).

translate([Char | TailSource], SourceRest, CodeIn, CodeOut) :-
  tr(Char, Op)
  ->
    translate(TailSource, SourceRest, [Op | CodeIn], CodeOut)
  ;
  Char = '['
  ->
    translate(TailSource, CodeAfterLoop, [], LoopCode),
    Loop = loop(LoopCode),
    translate(CodeAfterLoop, SourceRest, [Loop | CodeIn], CodeOut)
  ;
  Char = ']'
  ->
    SourceRest = TailSource,
    CodeOut = list.reverse(CodeIn)
  ;
    translate(TailSource, SourceRest, CodeIn, CodeOut)
.


:- type tape
  ---> tape(pos :: int, cells :: array(uint8)).


:- func current(tape) = uint8.
current(Tape) = array.lookup(Tape^cells, Tape^pos).

:- func new_tape = tape.
new_tape = tape(0, array.init(8192 * 1024, 0u8)).

:- pred run_code(opcode::in, tape::in, tape::out, io::di, io::uo) is det.

run_code(forth, Tape, (Tape^pos := Tape^pos + 1), !IO).
run_code(back, Tape, (Tape^pos := Tape^pos - 1), !IO).

run_code(inc, Tape, NewTape, !IO) :-
  V = current(Tape),
  NewTape = (Tape^cells := array.set(Tape^cells, Tape^pos, V + 1u8)).

run_code(dec, Tape, (Tape^cells := array.set(Tape^cells, Tape^pos, current(Tape) - 1u8)), !IO).

run_code(write, Tape, Tape, !IO) :-
    V = current(Tape),
    I = cast_to_int(V),
    ( if char.to_int(CH, I) then
       io.print(CH, !IO)
      else
       io.print('?', !IO)
    ),
    io.flush_output(!IO).

run_code(loop(CodeLoop), Tape, NewTape, !IO) :-
  run_loop(CodeLoop, Tape, NewTape, !IO).


run_code(nop, Tape, Tape, !IO).

run_code(read, Tape, TapeOut, !IO) :-
  io.read_char(ResIO, !IO),
  (
    ResIO = ok(Char),
    V = uint8.det_from_int(char.to_int(Char)),
    TapeOut = (Tape^cells := array.set(Tape^cells, Tape^pos, V))
    ;
    ResIO = eof,
    TapeOut = Tape
    ;
    ResIO = error(Error),
    io.print("Error: ", !IO),
    io.write_string(error_message(Error), !IO),
    io.nl(!IO),
    TapeOut = Tape
  ).

:- pred run_loop(list(opcode)::in, tape::in, tape::out, io::di, io::uo) is det.
run_loop(Codes, Tape, TapeOut, !IO) :-
  current(Tape) = 0u8 -> TapeOut = Tape
  ;
  run_program(Codes, Tape, NewTape, !IO),
  run_loop(Codes, NewTape, TapeOut, !IO).

:- pred run_program(list(opcode)::in, tape::in, tape::out, io::di, io::uo) is det.
run_program([], !Tape, !IO).

run_program([Op | Tail], !Tape, !IO) :-
  run_code(Op, !Tape, !IO),
  run_program(Tail, !Tape, !IO)
.

:- pred run_program(string::in, io::di, io::uo) is det.
run_program(Lines, !IO) :-
  Source = string.to_char_list(Lines),
  translate(Source, _, [], Program),
  run_program(Program, new_tape, _, !IO)
.

:- pred show_error(io.error::in, io::di, io::uo) is det.
show_error(Error, !IO) :-
  print("Error! ", !IO),
  io.write_string(error_message(Error), !IO),
  io.nl(!IO),
  io.set_exit_status(1, !IO).


main(!IO) :-
  io.command_line_arguments(Args, !IO),
  (
    Args = [],
    io.stderr_stream(StdErr, !IO),
    io.write_string(StdErr, "Usage: brainfuck <program filename> ...\n", !IO),
    io.set_exit_status(1, !IO)
    ;
    Args = [ File | _ ],
    io.open_input(File, Result, !IO),
    (
      Result = ok(Stream),
      io.read_file_as_string(Stream, PartialIO, !IO),
      (
        PartialIO = ok(Text),
        run_program(Text, !IO)
        ;
        PartialIO = error(_, Error),
        show_error(Error, !IO)
      )
      ;
      Result = error(Error),
      show_error(Error, !IO)
    )
  ).
