-module(minesweeperl).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([new_game/3, new_game/4]).

-include_lib("eunit/include/eunit.hrl").

-record(field,
        { is_mine    = false
        , is_covered = true
        , is_flagged = false
        , neighbours
        , neighbour_mines
        }).

-record(board,
        { dims
        , n_mines
        , fields
        }).

new_game(Rows, Cols, _Mines) when Rows < 2; Cols < 2 ->
    throw(badarg);
new_game(Rows, Cols, Mines) ->
    MineSeqList = generate_mines(Rows*Cols, Mines),
    new_game(Rows, Cols, Mines, MineSeqList).

%% partially or fully specified list of mines:
new_game(Rows, Cols, Mines, MineSeqList0) when length(MineSeqList0) < Mines ->
    MineSeqList = generate_mines(Rows*Cols, Mines, MineSeqList0),
    new_game(Rows, Cols, Mines, MineSeqList);
new_game(Rows, Cols, Mines, MineSeqList) ->
    Dims = {Rows, Cols},
    Fields0 = array:new([{size, Rows*Cols}, {default, #field{}}]),
    Fields1 = array:map(fun(Seq, Field) ->
                                case lists:member(Seq, MineSeqList) of
                                    true -> Field#field{is_mine = true};
                                    false -> Field
                                end
                        end, Fields0),
    Fields2 = array:map(fun(Seq, Field) ->
                                Pos = seq2pos(Seq, Dims),
                                Field#field{neighbours = neighbours(Pos, Dims)}
                        end, Fields1),
    Fields3 = array:map(fun(_Seq, Field) ->
                                NMines = neighbour_mines(Field, Fields2),
                                Field#field{neighbour_mines = NMines}
                        end, Fields2),
    #board{dims = Dims, n_mines = Mines, fields = Fields3}.


generate_mines(Fields, N) when N >= Fields -> throw(badarg);
generate_mines(Fields, N) -> generate_mines(Fields, N, []).

generate_mines(_Fields, 0, Mines) ->
    lists:sort(Mines);
generate_mines(Fields, N, Mines) ->
    Mine = random:uniform(Fields) - 1,
    AlreadyPresent = lists:member(Mine, Mines),
    if AlreadyPresent -> generate_mines(Fields, N, Mines);
        true          -> generate_mines(Fields, N-1, [Mine|Mines])
    end.


seq2pos(Seq, {_Rows, Cols}) ->
    X = Seq div Cols,
    Y = Seq rem Cols,
    {X, Y}.

pos2seq({Px, Py}, {_Rows, Cols}) -> Py + Cols * Px.

posl2seql(PosList, Dim) -> [pos2seq(Pos, Dim) || Pos <- PosList].


neighbours({Px, Py}, {Rows, Cols}) ->
    ns_1({Px, Py}, {Rows-1, Cols-1}, {Rows, Cols}).

ns_1({0, 0}, {_, _}, Dim) -> % top left
    posl2seql([{0, 1}, {1, 0}, {1, 1}], Dim);
ns_1({0, C}, {_, C}, Dim) -> % top right
    posl2seql([{0, C-1}, {1, C-1}, {1, C}], Dim);
ns_1({R, 0}, {R, _}, Dim) -> % bottom left
    posl2seql([{R-1, 0}, {R-1, 1}, {R, 1}], Dim);
ns_1({R, C}, {R, C}, Dim) -> % bottom right
    posl2seql([{R-1, C-1}, {R-1, C}, {R, C-1}], Dim);
ns_1({0, Py}, {_, _}, Dim) -> % top row
    posl2seql([{0, Py-1}, {0, Py+1}, {1, Py-1}, {1, Py}, {1, Py+1}], Dim);
ns_1({R, Py}, {R, _}, Dim) -> % bottom row
    posl2seql([{R-1, Py-1}, {R-1, Py}, {R-1, Py+1}, {R, Py-1}, {R, Py+1}], Dim);
ns_1({Px, 0}, {_, _}, Dim) -> % left column
    posl2seql([{Px-1, 0}, {Px-1, 1}, {Px, 1}, {Px+1, 0}, {Px+1, 1}], Dim);
ns_1({Px, C}, {_, C}, Dim) -> % right column
    posl2seql([{Px-1, C-1}, {Px-1, C}, {Px, C-1}, {Px+1, C-1}, {Px+1, C}], Dim);
ns_1({Px, Py}, {_, _}, Dim) -> % inner square
    posl2seql([{Px-1, Py-1}, {Px-1, Py}, {Px-1, Py+1}, {Px, Py-1}, {Px, Py+1},
               {Px+1, Py-1}, {Px+1, Py}, {Px+1, Py+1}], Dim).


neighbour_mines(#field{neighbours = Nbs}, Fields) ->
    NbFs = [array:get(Seq, Fields) || Seq <- Nbs],
    lists:foldl(fun(#field{is_mine = true}, Cnt) -> Cnt+1;
                   (#field{}, Cnt) -> Cnt
                end, 0, NbFs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newgame_test() ->
    %% Create a board with 3 rows and 4 cols with 5 pre-determined mines:
    %%   0, 1,*2,*3
    %%   4,*5, 6,*7
    %%   8,*9,10,11
    Board = new_game(3, 4, 5, [2,3,5,7,9]),
    #board{dims = {3, 4}, n_mines = 5, fields = Fields} = Board,
    F5 = array:get(5, Fields),
    F6 = array:get(6, Fields),
    ?assertEqual(true, F5#field.is_mine),
    ?assertEqual(false, F6#field.is_mine),
    ?assertEqual(2, neighbour_mines(F5, Fields)),
    ?assertEqual(5, neighbour_mines(F6, Fields)).


neighbours_test() ->
    %% Field seq numbers:
    %%   0, 1, 2, 3
    %%   4, 5, 6, 7
    %%   8, 9,10,11

    %% corners:
    ?assertEqual([1,4,5], neighbours({0,0}, {3,4})),
    ?assertEqual([2,6,7], neighbours({0,3}, {3,4})),
    ?assertEqual([4,5,9], neighbours({2,0}, {3,4})),
    ?assertEqual([6,7,10], neighbours({2,3}, {3,4})),
    %% sides:
    ?assertEqual([0,2,4,5,6], neighbours({0,1}, {3,4})),
    ?assertEqual([1,3,5,6,7], neighbours({0,2}, {3,4})),
    ?assertEqual([0,1,5,8,9], neighbours({1,0}, {3,4})),
    ?assertEqual([2,3,6,10,11], neighbours({1,3}, {3,4})),
    ?assertEqual([4,5,6,8,10], neighbours({2,1}, {3,4})),
    ?assertEqual([5,6,7,9,11], neighbours({2,2}, {3,4})),
    %% inner squares:
    ?assertEqual([0,1,2,4,6,8,9,10], neighbours({1,1}, {3,4})),
    ?assertEqual([1,2,3,5,7,9,10,11], neighbours({1,2}, {3,4})).
