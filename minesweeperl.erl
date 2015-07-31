-module(minesweeperl).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([new_game/3]).

-compile([export_all]). % DEBUG

-record(opts,
        { level
        , dims
        , n_mines
        }).

-record(field,
        { is_mine    = false
        , is_covered = true
        , is_flagged = false
        , n_borders
        , neighbour_mines
        }).

-record(state,
        { opts
        , board
        }).

new_game(Rows, Cols, Mines) ->
    Dims = {Rows, Cols},
    Board0 = array:new([{size, Rows*Cols}, {default, #field{}}]),
    MineSeqList = generate_mines(Rows*Cols, Mines),
    Board1 = array:map(fun(Seq, Field) ->
                               case lists:member(Seq, MineSeqList) of
                                   true -> Field#field{is_mine = true};
                                   false -> Field
                               end
                       end, Board0),
    Board2 = array:map(fun(Seq, Field) ->
                               Pos = seq_to_pos(Seq, Dims),
                               Field#field{n_borders = n_borders(Pos, Dims)}
                       end, Board1),
    Board3 = array:map(fun(Seq, Field) ->
                               Pos = seq_to_pos(Seq, Dims),
                               NMines = neighbour_mines(Pos, Dims, MineSeqList),
                               Field#field{neighbour_mines = NMines}
                       end, Board2),
    #state{opts = #opts{dims = Dims, n_mines = Mines},
           board = Board3}.


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


seq_to_pos(Seq, {_Rows, Cols}) ->
    X = Seq div Cols,
    Y = Seq rem Cols,
    {X, Y}.

pos_to_seq({Px, Py}, {_Rows, Cols}) ->
    Py + Cols * Px.


n_borders({Px, Py}, {Rows, Cols}) -> n_b1({Px, Py}, {Rows-1, Cols-1}).
n_b1({0, 0}, {_, _}) -> 3;
n_b1({0, C}, {_, C}) -> 3;
n_b1({R, 0}, {R, _}) -> 3;
n_b1({R, C}, {R, C}) -> 3;
n_b1({0, _}, {_, _}) -> 5;
n_b1({R, _}, {R, _}) -> 5;
n_b1({_, 0}, {_, _}) -> 5;
n_b1({_, C}, {_, C}) -> 5;
n_b1({_, _}, {_, _}) -> 8.

true1(true)  -> 1;
true1(false) -> 0.

has_mine({Px, _Py}, {_Rows, _Cols}, _MineSeqList) when Px < 0     -> false;
has_mine({Px, _Py}, {Rows, _Cols},  _MineSeqList) when Px >= Rows -> false;
has_mine({_Px, Py}, {_Rows, _Cols}, _MineSeqList) when Py < 0     -> false;
has_mine({_Px, Py}, {_Rows, Cols},  _MineSeqList) when Py >= Cols -> false;
has_mine(Pos, Dims, MineSeqList) ->
    lists:member(pos_to_seq(Pos, Dims), MineSeqList).

neighbour_mines({Px, Py}, Dims, MineSeqList) ->
    true1(has_mine({Px-1, Py-1}, Dims, MineSeqList)) +
    true1(has_mine({Px-1, Py},   Dims, MineSeqList)) +
    true1(has_mine({Px-1, Py+1}, Dims, MineSeqList)) +
    true1(has_mine({Px,   Py-1}, Dims, MineSeqList)) +
    true1(has_mine({Px,   Py+1}, Dims, MineSeqList)) +
    true1(has_mine({Px+1, Py-1}, Dims, MineSeqList)) +
    true1(has_mine({Px+1, Py},   Dims, MineSeqList)) +
    true1(has_mine({Px+1, Py+1}, Dims, MineSeqList)).
