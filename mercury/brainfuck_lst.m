%%%
% Implementation that use 2 lists instead of array and pointer
%
%%%
:- module brainfuck_lst
.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list, string, char, array, int, uint8, pair.

:- type opcode
  --->
      inc
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


:- type tape1 == pair(list(uint8), list(uint8)).

:- pred putc(uint8::in, io::di, io::uo) is det.
putc(H, !IO) :-
    I = cast_to_int(H),
    ( if char.to_int(CH, I) then
       io.print(CH, !IO)
      else
       io.print('?', !IO)
    ),
    io.flush_output(!IO)
    .

:- func new_tape = tape1.
new_tape = pair([0u8], [0u8]).

:- pred run_code(opcode::in, tape1::in, tape1::out, io::di, io::uo) is det.

run_code(forth, []       - T2, [] - [0u8 | T2], !IO).
run_code(forth, [H | T1] - T2, T1 - [H   | T2], !IO).
%
run_code(back, T1 -       [], [0u8 | T1] - [], !IO).
run_code(back, T1 - [H | T2], [H | T1]   - T2, !IO).
%
run_code(inc, []       - T2, [1u8]         - T2, !IO).
run_code(inc, [H | T1] - T2, [H + 1u8| T1] - T2, !IO).

%
run_code(dec, []       - T2, [255u8]        - T2, !IO).
run_code(dec, [H | T1] - T2, [H - 1u8 | T1] - T2, !IO).
%
run_code(write, [] - T2, TapeOut, !IO) :-
  run_code(write,[0u8] - T2, TapeOut, !IO)
  .

run_code(write, TapeIn, TapeIn, !IO) :-
    TapeIn = [H | _] - _,
    putc(H, !IO)
%    (if H >= 31u8 then
%      putc(H, !IO)
%    else
%      io.format("=%d", [i(cast_to_int(H))], !IO)
%    )
    .

%
run_code(read, [] - T2, [] - T2, !IO).
run_code(read, [H0 | T1] - T2, [H1 | T1] - T2, !IO) :-
  io.read_char(ResIO, !IO),
  (
    ResIO = ok(Char),
    H1 = uint8.det_from_int(char.to_int(Char))
    ;
    ResIO = eof,
    H1 = H0
    ;
    ResIO = error(Error),
    io.format("Error: %s\n", [s(error_message(Error))], !IO),
    H1 = H0
  ).

run_code(loop(CodeLoop), Tape, NewTape, !IO) :-
  run_loop(CodeLoop, Tape, NewTape, !IO).

:- pred run_loop(list(opcode)::in, tape1::in, tape1::out, io::di, io::uo) is det.

run_loop(Codes, TapeIn, TapeOut, !IO) :-
  if (TapeIn = [0u8|_] - _; TapeIn = [] - _) then
      TapeOut = TapeIn
  else
    run_program(Codes, TapeIn, NewTape, !IO),
    run_loop(Codes, NewTape, TapeOut, !IO)
    .

:- pred run_program(list(opcode)::in, tape1::in, tape1::out, io::di, io::uo) is det.
run_program([], !Tape, !IO).

run_program([Op | Tail], !Tape, !IO) :-
  run_code(Op, !Tape, !IO),
  run_program(Tail, !Tape, !IO)
.

:- pred run_program(string::in, io::di, io::uo) is det.
run_program(Lines, !IO) :-
  Source = string.to_char_list(Lines),
  translate(Source, _, [], Program),
  io.format("Size: %d\n", [i(list.length(Program))], !IO),

  run_program(Program, new_tape, _, !IO)
.

:- pred show_error(io.error::in, io::di, io::uo) is det.
show_error(Error, !IO) :-
  io.format("Error: %s\n", [s(error_message(Error))], !IO),
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
