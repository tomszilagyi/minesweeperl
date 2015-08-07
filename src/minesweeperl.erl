-module(minesweeperl).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([ new_game/3
        , new_game/4
        , step/2
        , flag/2
        , question/2]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/minesweeperl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    Fields = array:map(fun(Seq, Field) ->
                               Pos = seq2pos(Seq, Dims),
                               HasMine = lists:member(Seq, MineSeqList),
                               Field#field{has_mine = HasMine,
                                           neighbours = neighbours(Pos, Dims)}
                       end, Fields0),
    #board{dims = Dims, n_mines = Mines, n_hidden = Rows*Cols, fields = Fields}.

%% step on a field, and thus expose it.
%% - if it has a mine, you are dead and the game is over.
%% - if it is empty, the number of surrounding mines will appear.
%% - if there are no mines around you, this will recurse on the neighbours.
%%
%% return value: {Events, NewBoard} where
%% - Events is a list of game events triggered by your move;
%% - NewBoard is the updated board (in its eventual state).
%%
%% The idea is that the board display can be kept up-to-date based on
%% the event stream alone.
step(Pos, #board{dims = Dims} = Board) ->
    Seq = pos2seq(Pos, Dims),
    board_event(step, Seq, Board, []).

%% toggle the flag on a covered field
%% - if it is covered or questioned, it will become flagged.
%% - if it is flagged, it will become covered.
%% - if it is uncovered, nothing happens.
flag(Pos, #board{dims = Dims} = Board) ->
    Seq = pos2seq(Pos, Dims),
    board_event(flag, Seq, Board, []).

%% toggle the question mark on a covered field
%% - if it is covered or flagged, it will become questioned.
%% - if it is questioned, it will become covered.
%% - if it is uncovered, nothing happens.
question(Pos, #board{dims = Dims} = Board) ->
    Seq = pos2seq(Pos, Dims),
    board_event(question, Seq, Board, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

board_event(flag, Seq, #board{fields = Fields} = Board, Events) ->
    F = array:get(Seq, Fields),
    case F#field.status of
        flagged ->
            update_field_status(covered, Seq, Board, Events);
        St when St == covered; St == questioned ->
            update_field_status(flagged, Seq, Board, Events);
        _ -> {Events, Board}
    end;
board_event(question, Seq, #board{fields = Fields} = Board, Events) ->
    F = array:get(Seq, Fields),
    case F#field.status of
        questioned ->
            update_field_status(covered, Seq, Board, Events);
        St when St == covered; St == flagged ->
            update_field_status(questioned, Seq, Board, Events);
        _ -> {Events, Board}
    end;
%% stepped on a field -- leads to an uncover or an explode on the same field
board_event(step, Seq, #board{fields = Fields} = Board, Events) ->
    case array:get(Seq, Fields) of
        #field{has_mine = true, status = St} when St /= uncovered ->
            board_event(explode, Seq, Board, Events);
        #field{status = St} when St /= uncovered ->
            board_event(uncover, Seq, Board, Events)
    end;
%% stepped on an empty field -- handle uncovering it (possibly recursive)
board_event(uncover, Seq, #board{n_hidden = NH, fields = Fields} = Board,
            Events) ->
    F = array:get(Seq, Fields),
    case F#field.status of
        St when St == uncovered; St == exploded ->
            {Events, Board}; % break recursion
        _ ->
            NbrMines = nbr(mines, F, Fields),
            Fields1 = array:set(Seq, F#field{status=uncovered}, Fields),
            Events1 = [{uncovered, Seq, {empty, NbrMines}} | Events],
            Board1 = Board#board{n_hidden = NH-1, fields = Fields1},
            case NbrMines of
                0 -> % no mines nearby -- recursively uncover neighbours:
                    lists:foldl(fun(Nbr, {EvA,BoA}) ->
                                        board_event(uncover, Nbr, BoA, EvA)
                                end, {Events1, Board1}, F#field.neighbours);
                _ -> {Events1, Board1}
            end
    end;
%% stepped on a field with a mine -- handle explosion and game over
board_event(explode, Seq, Board, Events) ->
    {Events1, Board1} = update_field_status(exploded, Seq, Board, Events),
    board_event(game_over, Seq, Board1, Events1);
board_event(game_over, _Seq, #board{fields = Fields} = Board, Events) ->
    %% generate uncover event for all fields still covered
    %% {uncover, Seq, Type} where Type :: {empty, N} | mine
    UncoverEvents =
        array:foldl(fun(I, #field{status=St, has_mine=HasMine} = F, UEs)
                          when St == covered; St == flagged; St == questioned ->
                            Type = if HasMine -> mine;
                                      true -> {empty, nbr(mines, F, Fields)}
                                   end,
                            [{uncovered, I, Type} | UEs];
                       (_I, #field{}, UEs) -> UEs
                    end, Events, Fields),

    %% TODO update fields' status to uncovered (not really needed
    %% since display will be driven by event stream)

    %% mark end of game with game_over event
    Events1 = [game_over | UncoverEvents],
    {Events1, Board#board{n_hidden = 0}}.


update_field_status(NewStatus, Seq, #board{fields = Fields} = Board, Events) ->
    F = array:get(Seq, Fields),
    Fields1 = array:set(Seq, F#field{status=NewStatus}, Fields),
    Events1 = [{NewStatus, Seq} | Events],
    Board1 = Board#board{fields = Fields1},
    {Events1, Board1}.


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


nbr_fun(mines) -> fun(#field{has_mine = true}) -> true; (_) -> false end;
nbr_fun(covers) -> fun(#field{status = covered}) -> true; (_) -> false end;
nbr_fun(uncovers) -> fun(#field{status = uncovered}) -> true; (_) -> false end;
nbr_fun(flags) -> fun(#field{status = flagged}) -> true; (_) -> false end.

nbr(Attr, #field{neighbours = Nbs}, Fields) ->
    AttrFun = nbr_fun(Attr),
    NbFs = [array:get(Seq, Fields) || Seq <- Nbs],
    lists:foldl(fun(F, Cnt) ->
                        case AttrFun(F) of
                            true -> Cnt+1;
                            false -> Cnt
                        end
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
    ?assertEqual(true, F5#field.has_mine),
    ?assertEqual(false, F6#field.has_mine),
    ?assertEqual(2, nbr(mines, F5, Fields)),
    ?assertEqual(5, nbr(mines, F6, Fields)).

flag_covered_test() ->
    Board = new_game(3, 4, 5, [2,3,5,7,9]),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        flag({1,2}, Board),
    ?assertEqual(12, NHidden),
    ?assertMatch([{flagged, 6}], Events),
    ?assertMatch(#field{status=flagged}, array:get(6, NewFields)).

flag_questioned_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = question({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        flag({1,2}, Board1),
    ?assertEqual(12, NHidden),
    ?assertMatch([{flagged, 6}], Events),
    ?assertMatch(#field{status=flagged}, array:get(6, NewFields)).

flag_flagged_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = flag({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        flag({1,2}, Board1),
    ?assertEqual(12, NHidden),
    ?assertMatch([{covered, 6}], Events),
    ?assertMatch(#field{status=covered}, array:get(6, NewFields)).

flag_uncovered_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = step({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        flag({1,2}, Board1),
    ?assertEqual(11, NHidden),
    ?assertMatch([], Events),
    ?assertMatch(#field{status=uncovered}, array:get(6, NewFields)).

question_covered_test() ->
    Board = new_game(3, 4, 5, [2,3,5,7,9]),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        question({1,2}, Board),
    ?assertEqual(12, NHidden),
    ?assertMatch([{questioned, 6}], Events),
    ?assertMatch(#field{status=questioned}, array:get(6, NewFields)).

question_flagged_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = flag({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        question({1,2}, Board1),
    ?assertEqual(12, NHidden),
    ?assertMatch([{questioned, 6}], Events),
    ?assertMatch(#field{status=questioned}, array:get(6, NewFields)).

question_questioned_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = question({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        question({1,2}, Board1),
    ?assertEqual(12, NHidden),
    ?assertMatch([{covered, 6}], Events),
    ?assertMatch(#field{status=covered}, array:get(6, NewFields)).

question_uncovered_test() ->
    Board0 = new_game(3, 4, 5, [2,3,5,7,9]),
    {_, Board1} = step({1,2}, Board0),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} =
        question({1,2}, Board1),
    ?assertEqual(11, NHidden),
    ?assertMatch([], Events),
    ?assertMatch(#field{status=uncovered}, array:get(6, NewFields)).

step_on_empty_test() ->
    Board = new_game(3, 4, 5, [2,3,5,7,9]),
    {Events, #board{n_hidden = NHidden, fields=NewFields}} = step({1,2}, Board),
    ?assertEqual(11, NHidden),
    ?assertMatch([{uncovered, 6, {empty, 5}}], Events),
    ?assertMatch(#field{status=uncovered}, array:get(6, NewFields)).

step_on_mine_test() ->
    Board = new_game(3, 4, 5, [2,3,5,7,9]),
    {Events, #board{n_hidden = NHidden}} = step({0,2}, Board),
    ?assertEqual(0, NHidden),
    ?assertMatch([game_over,
                  {uncovered, 11, {empty, 1}},
                  {uncovered, 10, {empty, 3}},
                  {uncovered,  9, mine},
                  {uncovered,  8, {empty, 2}},
                  {uncovered,  7, mine},
                  {uncovered,  6, {empty, 5}},
                  {uncovered,  5, mine},
                  {uncovered,  4, {empty, 2}},
                  {uncovered,  3, mine},
                  {uncovered,  1, {empty, 2}},
                  {uncovered,  0, {empty, 1}},
                  {exploded,   2}], Events).

step_on_hole_test() ->
    %%  *0, 1,*2,*3
    %%   4, 5, 6, 7
    %%  *8, 9,10,11
    %%  12,13,14,15
    Board = new_game(4, 4, 4, [0,2,3,8]),
    {Events, #board{n_hidden = NHidden}} = step({2,2}, Board),
    ?assertEqual(7, NHidden),
    ?assertMatch([{uncovered, 15, {empty, 0}},
                  {uncovered, 13, {empty, 1}},
                  {uncovered, 14, {empty, 0}},
                  {uncovered, 11, {empty, 0}},
                  {uncovered,  9, {empty, 1}},
                  {uncovered,  7, {empty, 2}},
                  {uncovered,  6, {empty, 2}},
                  {uncovered,  5, {empty, 3}},
                  {uncovered, 10, {empty, 0}}], Events).

step_on_hole2_test() ->
    %%  *0, 1,*2,*3
    %%   4, 5, 6, 7
    %%  *8, 9,10,11
    %%  12,13,14,15
    %%  16,17,18,19
    Board = new_game(5, 4, 4, [0,2,3,8]),
    {Events, #board{n_hidden = NHidden}} = step({2,2}, Board),
    ?assertEqual(6, NHidden),
    ?assertMatch([{uncovered, 19, {empty, 0}},
                  {uncovered, 16, {empty, 0}},
                  {uncovered, 12, {empty, 1}},
                  {uncovered, 17, {empty, 0}},
                  {uncovered, 18, {empty, 0}},
                  {uncovered, 15, {empty, 0}},
                  {uncovered, 13, {empty, 1}},
                  {uncovered, 14, {empty, 0}},
                  {uncovered, 11, {empty, 0}},
                  {uncovered,  9, {empty, 1}},
                  {uncovered,  7, {empty, 2}},
                  {uncovered,  6, {empty, 2}},
                  {uncovered,  5, {empty, 3}},
                  {uncovered, 10, {empty, 0}}], Events).

generate_mines_test() ->
    ?assertMatch([_,_,_], generate_mines(2*2, 3)),
    ?assertMatch([_,_,_,_,_], generate_mines(3*4, 5)),
    ?assertMatch(badarg, catch generate_mines(3*4, 12)).

seq2pos_test() ->
    ?assertEqual({0,0}, seq2pos(0, {3,4})),
    ?assertEqual({0,1}, seq2pos(1, {3,4})),
    ?assertEqual({0,2}, seq2pos(2, {3,4})),
    ?assertEqual({0,3}, seq2pos(3, {3,4})),
    ?assertEqual({1,0}, seq2pos(4, {3,4})),
    ?assertEqual({1,1}, seq2pos(5, {3,4})),
    ?assertEqual({1,2}, seq2pos(6, {3,4})),
    ?assertEqual({1,3}, seq2pos(7, {3,4})),
    ?assertEqual({2,0}, seq2pos(8, {3,4})),
    ?assertEqual({2,1}, seq2pos(9, {3,4})),
    ?assertEqual({2,2}, seq2pos(10, {3,4})),
    ?assertEqual({2,3}, seq2pos(11, {3,4})).

pos2seq_test() ->
    ?assertEqual(0, pos2seq({0,0}, {3,4})),
    ?assertEqual(1, pos2seq({0,1}, {3,4})),
    ?assertEqual(2, pos2seq({0,2}, {3,4})),
    ?assertEqual(3, pos2seq({0,3}, {3,4})),
    ?assertEqual(4, pos2seq({1,0}, {3,4})),
    ?assertEqual(5, pos2seq({1,1}, {3,4})),
    ?assertEqual(6, pos2seq({1,2}, {3,4})),
    ?assertEqual(7, pos2seq({1,3}, {3,4})),
    ?assertEqual(8, pos2seq({2,0}, {3,4})),
    ?assertEqual(9, pos2seq({2,1}, {3,4})),
    ?assertEqual(10, pos2seq({2,2}, {3,4})),
    ?assertEqual(11, pos2seq({2,3}, {3,4})).

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
