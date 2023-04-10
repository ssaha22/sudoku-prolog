:- use_module(library(clpfd)).

% Predicate that checks if a number can be placed at a given position in a sudoku board
is_valid_action(Board, Row, Col, Num) :-
    between(0, 8, Row),
    between(0, 8, Col),
    between(1, 9, Num),
    nth0(Row, Board, RowVals),
    \+ member(Num, RowVals),
    transpose(Board, Columns),
    nth0(Col, Columns, ColVals),
    \+ member(Num, ColVals),
    RowBlock is Row // 3 * 3,
    ColBlock is Col // 3 * 3,
    block_index(RowBlock, ColBlock, BlockIndex),
    blocks(Board, Blocks),
    nth0(BlockIndex, Blocks, BlockVals),
    \+ member(Num, BlockVals).

perform_action(Board, Row, Col, Num, NewBoard) :-
    nth0(Row, Board, OldRow),
    replace_element_in_list(OldRow, Col, Num, NewRow),
    replace_element_in_list(Board, Row, NewRow, NewBoard).
    
replace_element_in_list(List, Index, Element, Result) :-
    nth0(Index, List, _, Temp),
    nth0(Index, Result, Element, Temp).

apply_hint(Board, Solution, (NewBoard, ((Row, Col), Num))) :-
    find_empty_position(Board, (Row, Col)),
    nth0(Row, Solution, SolutionBoardRow),
    nth0(Col, SolutionBoardRow, Num),
    once(perform_action(Board, Row, Col, Num, NewBoard)).

find_empty_position(Board, (Row, Column)) :-
    nth0(Row, Board, RowList),   % Get the RowList at the Row index
    nth0(Column, RowList, 0).    % Check if the Column index in RowList is 0

block_index(0, 0, 0).
block_index(0, 3, 1).
block_index(0, 6, 2).
block_index(3, 0, 3).
block_index(3, 6, 4).
block_index(3, 6, 5).
block_index(6, 0, 6).
block_index(6, 3, 7).
block_index(6, 6, 8).

blocks([], []).
blocks([[],[],[]|Rows], BlocksTail) :-
  blocks(Rows, BlocksTail).
blocks([[A,B,C|Bs], [D,E,F|Es], [G,H,I|Fs]|Rows], [[A,B,C,D,E,F,G,H,I]|BlocksTail]) :-
  blocks([Bs,Es,Fs|Rows], BlocksTail).

solve(Board, UpdatedBoard) :-
    replace_zeros_in_board(Board, UpdatedBoard),
    flatten(UpdatedBoard, BoardList),
    BoardList ins 1..9,
    maplist(all_distinct, UpdatedBoard),
    transpose(UpdatedBoard, TransposedBoard),
    maplist(all_distinct, TransposedBoard),
    blocks(UpdatedBoard, Blocks),
    maplist(all_distinct, Blocks),
    % Label the variables to find a solution
    label(BoardList).

solvable(Board) :-
    % Find all possible solutions
    findall(Board, solve(Board, _), Solutions),
    length(Solutions, 1).

replace_zeros_in_board(Board, Result) :-
    once(maplist(replace_zeros_in_row, Board, Result)).

replace_zeros_in_row([], []).
replace_zeros_in_row([0|T], [_|T2]) :- replace_zeros_in_row(T, T2).
replace_zeros_in_row([H|T], [H|T2]) :- H #\= 0, replace_zeros_in_row(T, T2).

run_game((Board, Solution)) :-
  ( Board == Solution ->
    board_to_string(Board, BoardString),
    write(BoardString), nl,
    writeln("Congratulations! You completed the game!")
  ;
    writeln("Current board:"),
    board_to_string(Board, BoardString),
    write(BoardString), nl,
    writeln("Enter 'h' for a hint, 'q' to quit, or 'c' to continue playing: "),
    read_line_to_string(user_input, Input),
    ( Input == "h" ->
      apply_hint(Board, Solution, (UpdatedBoard, ((Row, Col), Num))),
      action_to_string(((Row, Col), Num), ActionString),
      writeln(ActionString),
      run_game((UpdatedBoard, Solution))
    ; Input == "q" ->
      writeln("Goodbye!"),
      !
    ; get_action(((Row, Col), Num)),
      ( is_valid_action(Board, Row, Col, Num) ->
        perform_action(Board, Row, Col, Num, UpdatedBoard),
        action_to_string(((Row, Col), Num), ActionString),
        writeln(ActionString),
        run_game((UpdatedBoard, Solution))
      ; otherwise ->
        writeln("Invalid move"),
        run_game((Board, Solution))
      )
    )
  ).

get_num_input(Msg, Input) :-
    write(Msg),
    read_line_to_string(user_input, InputStr),
    (   string_chars(InputStr, Chars),
        maplist(char_type, Chars, [digit|_])
    ->  number_string(Input, InputStr)
    ;   writeln('Invalid input'),
        get_num_input(Msg, Input)
    ).

get_action(((Row, Col), Num)) :-
    get_num_input("Enter the row where you would like to place a number (0-8): ", Row),
    get_num_input("Enter the column where you would like to place a number (0-8): ", Col),
    get_num_input("Enter the number you would like to place (1-9): ", Num).

get_difficulty(Difficulty) :-
    write('Enter the difficulty you would like to play (\'easy\', \'medium\', or \'hard\'): '),
    read_line_to_string(user_input, Input),
    string_lower(Input, LowerInput),
    (member(LowerInput, ["easy", "medium", "hard"]) ->
        Difficulty = LowerInput
    ;
        writeln('Invalid input'),
        get_difficulty(Difficulty)
    ).

board_to_string([], "-------------------------------------\n").
board_to_string([H|T], String) :-
    board_to_string(T, S1),
    row_to_string(H, S2),
    board_to_string([], S3),
    string_concat(S2, "\n", S4),
    string_concat(S3, S4, S5),
    string_concat(S5, S1, String).

row_to_string([], "| ").
row_to_string([H|T], String) :-
    row_to_string([], NewResult),
    string_concat(NewResult, H, TempResult),
    string_concat(TempResult, " ", NewTempResult),
    row_to_string(T, SubResult),
    string_concat(NewTempResult, SubResult, String).

action_to_string(((Row, Col), Num), String) :-
    string_concat("Placed ", Num, S1),
    string_concat(S1, " at position (", S2),
    string_concat(S2, Row, S3),
    string_concat(S3, ", ", S4),
    string_concat(S4, Col, S5),
    string_concat(S5, ")", String).