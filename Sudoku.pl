:- use_module(library(clpfd)).

% creates a base board with an easy mix
baseBoardEasy([[1, 2, 3, 4, 5, 6, 7, 8, 9],
               [4, 5, 6, 7, 8, 9, 1, 2, 3],
               [7, 8, 9, 1, 2, 3, 4, 5, 6],
               [2, 3, 4, 5, 6, 7, 8, 9, 1],
               [5, 6, 7, 8, 9, 1, 2, 3, 4],
               [8, 9, 1, 2, 3, 4, 5, 6, 7],
               [3, 4, 5, 6, 7, 8, 9, 1, 2],
               [6, 7, 8, 9, 1, 2, 3, 4, 5],
               [9, 1, 2, 3, 4, 5, 6, 7, 8]]).

% creates a base board with a medium mix
baseBoardMed([[5, 3, 4, 6, 7, 8, 9, 1, 2],
              [6, 7, 2, 1, 9, 5, 3, 4, 8],
              [1, 9, 8, 3, 4, 2, 5, 6, 7],
              [8, 5, 9, 7, 6, 1, 4, 2, 3],
              [4, 2, 6, 8, 5, 3, 7, 9, 1],
              [7, 1, 3, 9, 2, 4, 8, 5, 6],
              [9, 6, 1, 5, 3, 7, 2, 8, 4],
              [2, 8, 7, 4, 1, 9, 6, 3, 5],
              [3, 4, 5, 2, 8, 6, 1, 7, 9]]).

% creates a base board with a hard mix
baseBoardHard([[9, 5, 4, 7, 1, 2, 6, 8, 3],
               [1, 7, 3, 4, 8, 6, 9, 2, 5],
               [2, 6, 8, 9, 5, 3, 4, 7, 1],
               [3, 4, 1, 8, 7, 5, 2, 9, 6],
               [8, 2, 7, 6, 9, 1, 3, 5, 4],
               [5, 9, 6, 3, 2, 4, 8, 1, 7],
               [6, 8, 5, 2, 3, 7, 1, 4, 9],
               [4, 1, 9, 5, 6, 8, 7, 3, 2],
               [7, 3, 2, 1, 4, 9, 5, 6, 8]]).

% example of a valid board
exampleBoard([[0, 5, 8, 2, 4, 0, 9, 1, 0],
              [0, 0, 0, 0, 9, 0, 6, 8, 7],
              [0, 0, 0, 0, 6, 0, 2, 0, 0],
              [8, 0, 5, 0, 0, 0, 4, 0, 0],
              [0, 7, 0, 0, 5, 0, 1, 6, 2],
              [1, 2, 0, 0, 0, 4, 0, 3, 0],
              [0, 9, 6, 0, 8, 1, 3, 0, 5],
              [0, 8, 1, 0, 0, 0, 0, 2, 0],
              [7, 4, 3, 5, 0, 6, 0, 0, 0]]).
  
% exampleBoard almost fully filled in
exampleBoardAlmostFilled([[0, 5, 8, 2, 4, 0, 9, 1, 3],
                          [4, 3, 2, 1, 9, 5, 6, 8, 7],
                          [9, 1, 7, 8, 6, 3, 2, 5, 4],
                          [8, 6, 5, 3, 1, 2, 4, 7, 9],
                          [3, 7, 4, 9, 5, 8, 1, 6, 2],
                          [1, 2, 9, 6, 7, 4, 5, 3, 8],
                          [2, 9, 6, 7, 8, 1, 3, 4, 5],
                          [5, 8, 1, 4, 3, 9, 7, 2, 6],
                          [7, 4, 3, 5, 2, 6, 8, 9, 1]]).

% solution to exampleBoard
exampleBoardSolution([[6, 5, 8, 2, 4, 7, 9, 1, 3],
                      [4, 3, 2, 1, 9, 5, 6, 8, 7],
                      [9, 1, 7, 8, 6, 3, 2, 5, 4],
                      [8, 6, 5, 3, 1, 2, 4, 7, 9],
                      [3, 7, 4, 9, 5, 8, 1, 6, 2],
                      [1, 2, 9, 6, 7, 4, 5, 3, 8],
                      [2, 9, 6, 7, 8, 1, 3, 4, 5],
                      [5, 8, 1, 4, 3, 9, 7, 2, 6],
                      [7, 4, 3, 5, 2, 6, 8, 9, 1]]).

% a board with no solutions
unsolvableBoard([[5, 1, 6, 8, 4, 9, 7, 3, 2],
                 [3, 0, 7, 6, 0, 5, 0, 0, 0],
                 [8, 0, 9, 7, 0, 0, 0, 6, 5],
                 [1, 3, 5, 0, 6, 0, 9, 0, 7],
                 [4, 7, 2, 5, 9, 1, 0, 0, 6],
                 [9, 6, 8, 3, 7, 0, 0, 5, 0],
                 [2, 5, 3, 1, 8, 6, 0, 7, 4],
                 [6, 8, 4, 2, 0, 7, 5, 0, 0],
                 [7, 9, 1, 0, 5, 0, 6, 0, 8]]).
  
% a board with two possible solutions
boardWithTwoSolutions([[2, 9, 5, 7, 4, 3, 8, 6, 1],
                      [4, 3, 1, 8, 6, 5, 9, 0, 0],
                      [8, 7, 6, 1, 9, 2, 5, 4, 3],
                      [3, 8, 7, 4, 5, 9, 2, 1, 6],
                      [6, 1, 2, 3, 8, 7, 4, 9, 5],
                      [5, 4, 9, 2, 1, 6, 7, 3, 8],
                      [7, 6, 3, 5, 2, 4, 1, 8, 9],
                      [9, 2, 8, 6, 7, 1, 3, 5, 4],
                      [1, 5, 4, 9, 3, 8, 6, 0, 0]]).

% starts an interactive Sudoku game with a generated board
play :-
    writeln("Welcome to Haskell Sudoku!"),
    get_difficulty(Difficulty),
    writeln("Generating board."),
    generateBoard(Difficulty, Solution, Board),
    run_game((Board, Solution)).

% starts an interactive Sudoku game with the given board
% Examples:
% exampleBoard(Board), play_with_board(Board).
% exampleBoardAlmostFilled(Board), play_with_board(Board).
play_with_board(Board) :-
  ( solvable(Board) ->
    solve(Board, Solution),
    run_game((Board, Solution))
  ;   
    writeln("Invalid starting board.")
  ).

% runs a Sudoku game with a partially filled board and its solution
run_game((Board, Solution)) :-
  ( Board == Solution ->
    print_board(Board),
    write("Congratulations! You completed the game!")
  ;
    writeln("Current board:"),
    print_board(Board),
    writeln("Enter 'h' for a hint, 'q' to quit, or 'c' to continue playing: "),
    read_line_to_string(user_input, Input),
    ( Input == "h" ->
      apply_hint(Board, Solution, (UpdatedBoard, ((Row, Col), Num))),
      print_action(((Row, Col), Num)),
      run_game((UpdatedBoard, Solution))
    ; Input == "q" ->
      writeln("Goodbye!"),
      !
    ; get_action(((Row, Col), Num)),
      ( is_valid_action(Board, Row, Col, Num) ->
        perform_action(Board, Row, Col, Num, UpdatedBoard),
        print_action(((Row, Col), Num)),
        run_game((UpdatedBoard, Solution))
      ; otherwise ->
        writeln("Invalid move"),
        run_game((Board, Solution))
      )
    )
  ).

% produces true if UpdatedBoard is the solution to Board
% Examples:
% exampleBoard(Board), solve(Board, Solution).
% exampleBoardAlmostFilled(Board), solve(Board, Solution).
% unsolvableBoard(Board), solve(Board, Solution).
% boardWithTwoSolutions(Board), solve(Board, Solution).
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

% produces true if the given Board has exactly one solution
solvable(Board) :-
    % Find all possible solutions
    findall(Board, solve(Board, _), Solutions),
    length(Solutions, 1).

% produces true if Num placed at position (Row, Col) on the board is a valid move
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

% produces true if NewBoard is Board with Num placed at position (Row, Col)
perform_action(Board, Row, Col, Num, NewBoard) :-
    nth0(Row, Board, OldRow),
    replace_element_in_list(OldRow, Col, Num, NewRow),
    replace_element_in_list(Board, Row, NewRow, NewBoard).
    
% produces true if Result is List with Element placed at Index
replace_element_in_list(List, Index, Element, Result) :-
    nth0(Index, List, _, Temp),
    nth0(Index, Result, Element, Temp).

% applies a hint from the solution to produce NewBoard
apply_hint(Board, Solution, (NewBoard, ((Row, Col), Num))) :-
    once(find_empty_position(Board, (Row, Col))),
    nth0(Row, Solution, SolutionBoardRow),
    nth0(Col, SolutionBoardRow, Num),
    once(perform_action(Board, Row, Col, Num, NewBoard)).

% produces true if position (Row, Column) is empty in Board
find_empty_position(Board, (Row, Column)) :-
    nth0(Row, Board, RowList),   % Get the RowList at the Row index
    nth0(Column, RowList, 0).    % Check if the Column index in RowList is 0

% produces the index in the list produced by blocks of the 3x3 block
block_index(0, 0, 0).
block_index(0, 3, 1).
block_index(0, 6, 2).
block_index(3, 0, 3).
block_index(3, 6, 4).
block_index(3, 6, 5).
block_index(6, 0, 6).
block_index(6, 3, 7).
block_index(6, 6, 8).

% produces the board as a list of the 9 3x3 blocks
blocks([], []).
blocks([[],[],[]|Rows], BlocksTail) :-
  blocks(Rows, BlocksTail).
blocks([[A,B,C|Bs], [D,E,F|Es], [G,H,I|Fs]|Rows], [[A,B,C,D,E,F,G,H,I]|BlocksTail]) :-
  blocks([Bs,Es,Fs|Rows], BlocksTail).

% replaces zeros in a Sudoku board with free variables
replace_zeros_in_board(Board, Result) :-
    once(maplist(replace_zeros_in_row, Board, Result)).

% replaces zeros in a list with free variables
replace_zeros_in_row([], []).
replace_zeros_in_row([0|T], [_|T2]) :- replace_zeros_in_row(T, T2).
replace_zeros_in_row([H|T], [H|T2]) :- H #\= 0, replace_zeros_in_row(T, T2).

% prints Msg and gets a numerical input
get_num_input(Msg, Input) :-
    write(Msg),
    read_line_to_string(user_input, InputStr),
    (   string_chars(InputStr, Chars),
        maplist(char_type, Chars, [digit|_])
    ->  number_string(Input, InputStr)
    ;   writeln('Invalid input'),
        get_num_input(Msg, Input)
    ).

% gets a move for the Sudoku game
get_action(((Row, Col), Num)) :-
    get_num_input("Enter the row where you would like to place a number (0-8): ", Row),
    get_num_input("Enter the column where you would like to place a number (0-8): ", Col),
    get_num_input("Enter the number you would like to place (1-9): ", Num).

% gets a diffulty of easy, medium, or hard
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

% prints out the given board
print_board([]).
print_board([Row|Rows]) :-
    format('~w~n', [Row]),
    print_board(Rows).

% prints out the given action
print_action(((Row, Col), Num)) :-
    string_concat("Placed ", Num, S1),
    string_concat(S1, " at position (", S2),
    string_concat(S2, Row, S3),
    string_concat(S3, ", ", S4),
    string_concat(S4, Col, S5),
    string_concat(S5, ")", String),
    writeln(String).

%allButTwoMatching is true when L1 has the same values as L2 in every position except two of them
allButTwoMatching(L1, L2) :- nth(X1, L1, N1), nth(X1, L2, N1), nth(X2, L1, N2), nth(X2, L2, N2), nth(X3, L1, N3), nth(X3, L2, N3),
                            nth(X4, L1, N4), nth(X4, L2, N4), nth(X5, L1, N5), nth(X5, L2, N5), nth(X6, L1, N6), nth(X6, L2, N6),
                            nth(X7, L1, N7), nth(X7, L2, N7), 
                            X1 \= X2, X1 \= X3, X1 \= X4, X1 \= X5, X1 \= X6, X1 \= X7,
                            X2 \= X3, X2 \= X4, X2 \= X5, X2 \= X6, X2 \= X7,
                            X3 \= X4, X3 \= X5, X3 \= X6, X3 \= X7,
                            X4 \= X5, X4 \= X6, X4 \= X7,
                            X5 \= X6, X5 \= X7,
                            X6 \= X7, validList(L1), validList(L2).

%nth(N, L, V) is true if V is the Nth value in list L
nth(1, [A, B, C, D, E, F, G, H, I], A).
nth(2, [A, B, C, D, E, F, G, H, I], B).
nth(3, [A, B, C, D, E, F, G, H, I], C).
nth(4, [A, B, C, D, E, F, G, H, I], D).
nth(5, [A, B, C, D, E, F, G, H, I], E).
nth(6, [A, B, C, D, E, F, G, H, I], F).
nth(7, [A, B, C, D, E, F, G, H, I], G).
nth(8, [A, B, C, D, E, F, G, H, I], H).
nth(9, [A, B, C, D, E, F, G, H, I], I).

%findInBoard(X, Y, L, A) is true when A is the Xth value in the Yth list of board L
findInBoard(X, Y, L, A) :- nth(Y, L, B), nth(X, B, A).

%swapByValue(N1, N2, L1, L2) is true when L2 has N1 and N2 in swapped positions to where they were in L1, and all other
%values remain in the same spot
swapByValue(N1, N2, L1, L2) :- nth(X1, L1, N1), nth(X2, L1, N2), nth(X2, L2, N1), nth(X1, L2, N2), limit(1, allButTwoMatching(L1, L2)), N1 \= N2;
                               N1 = N2, L1 = L2.

%swapByPosition(N1, N2, L1, L2) is true when L2 has the value at N1 in L1 at N2, and the value at N2 in L1 at N1
swapByPosition(N1, N2, L1, L2) :- nth(N1, L1, V1), nth(N2, L1, V2), nth(N2, L2, V1), nth(N1, L2, V2), limit(1, allButTwoMatching(L1, L2)), N1 \= N2, L1 \= L2;
                                N1 = N2, L1 = L2.

%swapInRow(R, N1, N2, B1, B2) is true when B2 has values N1 and N2 swapped in row R from the positions they were in in B1 row R.
swapInRow(R, N1, N2, B1, B2) :- nth(R, B1, L1), nth(R, B2, L2), swapByValue(N1, N2, L1, L2).

%swapAllInBoard(N1, N2, B1, B2) is true when B2 has N1 and N2 in swapped positions row by row to where they were in L1, and 
%all the other values remain in the same places
swapAllInBoard(N1, N2, B1, B2) :- N1 \= N2, swapInRow(1, N1, N2, B1, B2), swapInRow(2, N1, N2, B1, B2), swapInRow(3, N1, N2, B1, B2),
                                swapInRow(4, N1, N2, B1, B2), swapInRow(5, N1, N2, B1, B2), swapInRow(6, N1, N2, B1, B2),
                                swapInRow(7, N1, N2, B1, B2), swapInRow(8, N1, N2, B1, B2), swapInRow(9, N1, N2, B1, B2);
                                N1 = N2, B1 = B2.

%mixBoard(B0, B9) is true when values 1-9 in B0 have been systematically swapped with randomly selected numbers
%to create their new order in B9 (ie a valid sudoku board (B0) is randomly scrambled into a new valid sudoku board (B9))
mixBoard(B0, B9) :- random(1, 9, X1), swapAllInBoard(1, X1, B0, B1), random(1, 9, X2), swapAllInBoard(2, X2, B1, B2),
                    random(1, 9, X3), swapAllInBoard(3, X3, B2, B3), random(1, 9, X4), swapAllInBoard(4, X4, B3, B4), 
                    random(1, 9, X5), swapAllInBoard(5, X5, B4, B5), random(1, 9, X6), swapAllInBoard(6, X6, B5, B6), 
                    random(1, 9, X7), swapAllInBoard(7, X7, B6, B7), random(1, 9, X8), swapAllInBoard(8, X8, B7, B8), 
                    random(1, 9, X9), swapAllInBoard(9, X9, B8, B9).

%swapsLists(N1, N2, L, B0, B9) is true when list number L is swapped with a randomly chosen list in range N1 to N2 from 
%B0 to make its new position in B9
swapLists(X, L, B0, B9) :- swapByPosition(L, X, B0, B9), L \= X, B0 \= B9;
                            X = L, B0 = B9.

%pickSwap(N1, N2, L, B0, B9) is true when B0 has the value at position L is swapped with a value at position X from where they were in B9, 
%randomly chosen between the range N1 and N2.
pickSwap(N1, N2, L, B0, B9) :- random(N1, N2, X), swapLists(X, L, B0, B9).

%jumbleRows(B0, B9) is true when rows in blocks 1 - 3, 4 - 6, and 7 - 9 are scrambled into new positions in B9
jumbleRows(B0, B9) :- pickSwap(1, 3, 1, B0, B1), pickSwap(1, 3, 2, B1, B2), pickSwap(1, 3, 3, B2, B3), pickSwap(4, 6, 4, B3, B4),
                    pickSwap(4, 6, 5, B4, B5), pickSwap(4, 6, 6, B5, B6), pickSwap(7, 9, 7, B6, B7), pickSwap(7, 9, 8, B7, B8),
                    pickSwap(7, 9, 9, B8, B9).

%jumbleColumns(B0, B9) is true when columns in blocks 1 - 3, 4 - 6, and 7 - 9 are scrambled into new positions in B9
jumbleColumns(B0, B3) :- transpose(B0, B1), jumbleRows(B1, B2), transpose(B2, B3).

%assignGroupNum(N1, N2) is true when N2 is the start value of the group N1 is in
assignGroupNum(N1, N2) :- N1 = 1, N2 = 1;
                        N1 = 2, N2 = 1;
                        N1 = 3, N2 = 1;
                        N1 = 4, N2 = 4;
                        N1 = 5, N2 = 4;
                        N1 = 6, N2 = 4;
                        N1 = 7, N2 = 7;
                        N1 = 8, N2 = 7;
                        N1 = 9, N2 = 7.

%jumbleRowGroups(B0, B9) is true when the rows in B0 are mixed up in groups from 1-3, 4-6, and 7-9 to make
%new placements in B9
jumbleRowGroups(B0, B9) :- random(1, 9, X1), assignGroupNum(X1, X2), swapByPosition(1, X2, B0, B1), (X3 is (X2 + 1)), swapByPosition(2, X3, B1, B2),
                            (X4 is (X3 + 1)), swapByPosition(3, X4, B2, B3), random(1, 9, X5), assignGroupNum(X5, X6), swapByPosition(4, X6, B3, B4),
                            (X7 is (X6 + 1)), swapByPosition(5, X7, B4, B5), (X8 is (X7 + 1)), swapByPosition(6, X8, B5, B6),
                            random(1, 9, X9), assignGroupNum(X9, X10), swapByPosition(7, X10, B6, B7), (X11 is (X10 + 1)), swapByPosition(8, X11, B7, B8),
                            (X12 is (X11 + 1)), swapByPosition(9, X12, B8, B9).

%jumbleColumnGroups(B0, B9) is true when the columns in B0 are mixed up in groups from 1-3, 4-6, and 7-9 to make
%new placements in B9
jumbleColumnGroups(B0, B3) :- transpose(B0, B1), limit(1, jumbleRowGroups(B1, B2)), transpose(B2, B3).

%jumbleBoard(B0, B5) is true when the values in B0 are mixed individually, by row, by column, by groups of rows, 
%by groups of columns to form a valid board B5
jumbleBoard(B0, B5) :- mixBoard(B0, B1), jumbleRows(B1, B2), jumbleColumns(B2, B3), jumbleRowGroups(B3, B4), jumbleColumnGroups(B4, B5), validBoard(B5).

%validList([A, B, C, D, E, F, G, H, I]) is true when A, B, C, D, E, F, G, H, and I are all different values
validList([A, B, C, D, E, F, G, H, I]) :- A \= B, A \= C, A \= D, A \= E, A \= F, A \= G, A \= H, A \= I,
                                          B \= C, B \= D, B \= E, B \= F, B \= G, B \= H, B \= I,
                                          C \= D, C \= E, C \= F, C \= G, C \= H, C \= I,
                                          D \= E, D \= F, D \= G, D \= H, D \= I,
                                          E \= F, E \= G, E \= H, E \= I,
                                          F \= G, F \= H, F \= I,
                                          G \= H, G \= I,
                                          H \= I.

%validRows(B) is true when each row in B is a valid list
validRows(B) :- nth(1, B, L1), validList(L1), nth(2, B, L2), validList(L2), nth(3, B, L3), validList(L3), nth(4, B, L4), validList(L4),
                nth(5, B, L5), validList(L5), nth(6, B, L6), validList(L6), nth(7, B, L7), validList(L7), nth(8, B, L8), validList(L8),
                nth(9, B, L9), validList(L9).

%validColumns(B) is true when each column in B is a valid list
validColumns(B) :- transpose(B, B1), nth(1, B1, L1), validList(L1), nth(2, B1, L2), validList(L2), nth(3, B1, L3), validList(L3),
                    nth(4, B1, L4), validList(L4), nth(5, B1, L5), validList(L5), nth(6, B1, L6), validList(L6), nth(7, B1, L7), validList(L7),
                    nth(8, B1, L8), validList(L8), nth(9, B1, L9), validList(L9).

%validSquares([[A, B, C,  A1, B1, C1,  A2, B2, C2], [D, E, F,  D1, E1, F1,  D2, E2, F2], [G, H, I,  G1, H1, I1,  G2, H2, I2],
%[A3, B3, C3,  A4, B4, C4,  A5, B5, C5], [D3, E3, F3,  D4, E4, F4,  D5, E5, F5], [G3, H3, I3,  G4, H4, I4,  G5, H5, I5],
%[A6, B6, C6,  A7, B7, C7,  A8, B8, C8], [D6, E6, F6,  D7, E7, F7,  D8, E8, F8], [G6, H6, I6,  G7, H7, I7,  G8, H8, I8]]) is
%true when each 9x9 square is a valid list
validSquares([[A, B, C,  A1, B1, C1,  A2, B2, C2],
              [D, E, F,  D1, E1, F1,  D2, E2, F2],
              [G, H, I,  G1, H1, I1,  G2, H2, I2],
              [A3, B3, C3,  A4, B4, C4,  A5, B5, C5],
              [D3, E3, F3,  D4, E4, F4,  D5, E5, F5],
              [G3, H3, I3,  G4, H4, I4,  G5, H5, I5],
              [A6, B6, C6,  A7, B7, C7,  A8, B8, C8],
              [D6, E6, F6,  D7, E7, F7,  D8, E8, F8],
              [G6, H6, I6,  G7, H7, I7,  G8, H8, I8]]) :- validList([A, B, C, D, E, F, G, H, I]),
                                                          validList([A1, B1, C1, D1, E1, F1, G1, H1, I1]),
                                                          validList([A2, B2, C2, D2, E2, F2, G2, H2, I2]),
                                                          validList([A3, B3, C3, D3, E3, F3, G3, H3, I3]),
                                                          validList([A4, B4, C4, D4, E4, F4, G4, H4, I4]),
                                                          validList([A5, B5, C5, D5, E5, F5, G5, H5, I5]),
                                                          validList([A6, B6, C6, D6, E6, F6, G6, H6, I6]),
                                                          validList([A7, B7, C7, D7, E7, F7, G7, H7, I7]),
                                                          validList([A8, B8, C8, D8, E8, F8, G8, H8, I8]).

%validBoard(B) is true when all the rows, columns, and squares in B are valid
validBoard(B) :- validRows(B), validColumns(B), validSquares(B).

%printBoard(B0, A, B, C, D, E, F, G, H, I) is true when A is the first list in B0, B is the second, C is the third, 
%D is the fourth, E is the fifth, F is the sixth, G is the seventh, H is the eighth, and I is the ninth
printBoard(B0, A, B, C, D, E, F, G, H, I) :- nth(1, B0, A), nth(2, B0, B), nth(3, B0, C), nth(4, B0, D), nth(5, B0, E),
                                            nth(6, B0, F), nth(7, B0, G), nth(8, B0, H), nth(9, B0, I).

%allButOne(B1, B2) is true when B1 has all the same values in the same positions as B2, except one
allButOne(B1, B2) :- limit(1, oneDifferentReturn(B1, B2, L1, L2)), limit(1, oneDifferent(L1, L2)).

%oneDifferentReturn(L1, L2, N91, N92) is true when L1 and L2 have all the same valies in the same positions
%except one, and N91 is the different value in L1, and N92 is the different value in L2.
oneDifferentReturn(L1, L2, N91, N92) :- nth(X1, L1, N1), nth(X1, L2, N1), nth(X2, L1, N2), nth(X2, L2, N2), nth(X3, L1, N3), nth(X3, L2, N3),
                                        nth(X4, L1, N4), nth(X4, L2, N4), nth(X5, L1, N5), nth(X5, L2, N5), nth(X6, L1, N6), nth(X6, L2, N6),
                                        nth(X7, L1, N7), nth(X7, L2, N7), nth(X8, L1, N8), nth(X8, L2, N8), nth(X9, L1, N91), nth(X9, L2, N92), N91 \= N92,
                                        X1 \= X2, X1 \= X3, X1 \= X4, X1 \= X5, X1 \= X6, X1 \= X7, X1 \= X8, X1 \= X9,
                                        X2 \= X3, X2 \= X4, X2 \= X5, X2 \= X6, X2 \= X7, X2 \= X8, X2 \= X9,
                                        X3 \= X4, X3 \= X5, X3 \= X6, X3 \= X7, X3 \= X8, X3 \= X9,
                                        X4 \= X5, X4 \= X6, X4 \= X7, X4 \= X8, X4 \= X9,
                                        X5 \= X6, X5 \= X7, X5 \= X8, X5 \= X9,
                                        X6 \= X7, X6 \= X8, X6 \= X9,
                                        X7 \= X8, X7 \= X9,
                                        X8 \= X9.

%oneDifferent(L1, L2) is true when list L1 has all the same values in the same positions as L2, except one
oneDifferent(L1, L2) :- nth(X1, L1, N1), nth(X1, L2, N1), nth(X2, L1, N2), nth(X2, L2, N2), nth(X3, L1, N3), nth(X3, L2, N3), nth(X4, L1, N4), nth(X4, L2, N4),
                        nth(X5, L1, N5), nth(X5, L2, N5), nth(X6, L1, N6), nth(X6, L2, N6), nth(X7, L1, N7), nth(X7, L2, N7),
                        nth(X8, L1, N8), nth(X8, L2, N8), nth(X9, L1, N91), nth(X9, L2, N92),
                        X1 \= X2, X1 \= X3, X1 \= X4, X1 \= X5, X1 \= X6, X1 \= X7, X1 \= X8, X1 \= X9,
                        X2 \= X3, X2 \= X4, X2 \= X5, X2 \= X6, X2 \= X7, X2 \= X8, X2 \= X9,
                        X3 \= X4, X3 \= X5, X3 \= X6, X3 \= X7, X3 \= X8, X3 \= X9,
                        X4 \= X5, X4 \= X6, X4 \= X7, X4 \= X8, X4 \= X9,
                        X5 \= X6, X5 \= X7, X5 \= X8, X5 \= X9,
                        X6 \= X7, X6 \= X8, X6 \= X9,
                        X7 \= X8, X7 \= X9,
                        X8 \= X9.

%removeValue(X, Y, B1, B2) is true when the value at coordinate (X, Y) in B2 is 0. If the value at coordinate (X, Y) in B1 isn't 0, all the
%values in B1 except the one at position (X, Y) are in the same positions in B2.
removeValue(X, Y, B1, B2) :- nth(Y, B2, L2), nth(X, L2, 0), nth(Y, B1, L1), nth(X, L1, V1), not(V1 = 0), limit(1, allButOne(B1, B2));
                             nth(Y, B1, L1), nth(X, L1, 0), B1 = B2.

%removal(B1, B2) is true when B2 has a randomly chosen value removed to create a still solvable board, or if the removal at the randomly chosen
%coordinates create an unsolvable board, B1 = B2.
removal(B1, B2):- random(1, 9, X), random(1, 9, Y), limit(1, removeValue(X, Y, B1, B2)).

%emptyBoard(B1, B67) is true when the function removal(B1, B2) acts on B1 67 times to remove a large number of the values in B1 to create B2
emptyBoard(B1, B50) :- limit(1, removal(B1, B2)), limit(1, removal(B2, B3)), limit(1, removal(B3, B4)), limit(1, removal(B4, B5)), limit(1, removal(B5, B6)), limit(1, removal(B6, B7)), limit(1, removal(B7, B8)), limit(1, removal(B8, B9)), limit(1, removal(B9, B10)), 
                       limit(1, removal(B10, B11)), limit(1, removal(B11, B12)), limit(1, removal(B12, B13)), limit(1, removal(B13, B14)), limit(1, removal(B14, B15)), limit(1, removal(B15, B16)), limit(1, removal(B16, B17)), limit(1, removal(B17, B18)), 
                       limit(1, removal(B18, B19)), limit(1, removal(B19, B20)), limit(1, removal(B20, B21)), limit(1, removal(B21, B22)), limit(1, removal(B22, B23)), limit(1, removal(B23, B24)), limit(1, removal(B24, B25)), limit(1, removal(B25, B26)), 
                       limit(1, removal(B26, B27)), limit(1, removal(B27, B28)), limit(1, removal(B28, B29)), limit(1, removal(B29, B30)), limit(1, removal(B30, B31)), limit(1, removal(B31, B32)), limit(1, removal(B32, B33)), limit(1, removal(B33, B34)), 
                       limit(1, removal(B34, B35)), limit(1, removal(B35, B36)), limit(1, removal(B36, B37)), limit(1, removal(B37, B38)), limit(1, removal(B38, B39)), limit(1, removal(B39, B40)), limit(1, removal(B40, B41)), limit(1, removal(B41, B42)), 
                       limit(1, removal(B42, B43)), limit(1, removal(B43, B44)), limit(1, removal(B44, B45)), limit(1, removal(B45, B46)), limit(1, removal(B46, B47)), limit(1, removal(B47, B48)), limit(1, removal(B48, B49)), limit(1, removal(B49, B50)).

%generateBoard(L, S, G) is true when L is a string either "Easy", "Medium", "Hard", and S is a solution board, and G is a game board
generateBoard(L, S, G) :- L = "easy", generateEasy(S, G);
                           L = "medium", generateMedium(S, G);
                           L = "hard", generateHard(S, G).

%generateEasy(S, G) is true when S is an easy solution board, and G is an easy game board (emptied easy solution board)
generateEasy(S, G) :- baseBoardEasy(B), jumbleBoard(B, S), emptyBoard(S, G).

%generateMedium(S, G) is true when S is an medium solution board, and G is an medium game board (emptied medium solution board)
generateMedium(S, G) :- baseBoardMed(B), jumbleBoard(B, S), emptyBoard(S, G).

%generateHard(S, G) is true when S is an hard solution board, and G is an hard game board (emptied hard solution board)
generateHard(S, G) :- baseBoardHard(B), jumbleBoard(B, S), emptyBoard(S, G).
