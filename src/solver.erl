-module(solver).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([ step/2
        , flag_apparent_mines/1
        , uncover_safe_areas/1
        , uncover_unsafe/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/minesweeperl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Steps the solver by taking appropriate action on the board.
%% The current solver state input to this function can be flag | safe | unsafe.
%%
%% Returns {NewState, Events, NewBoard} where NewState can be one of
%% the above or 'newgame' in case the game has ended as a result of
%% the move done. In this case a new board must be set up, which can
%% be solved again, starting out with initial state 'flag'.
step(flag, Board) ->
    %% Can we flag some fields?
    case flag_apparent_mines(Board) of
        {[], Board} -> % nope
            step(unsafe, Board);
        {Events, NewBoard} ->
            {safe, Events, NewBoard}
    end;
step(safe, Board) ->
    case uncover_safe_areas(Board) of
        {[], Board} -> % we need to go unsafe
            step(unsafe, Board);
        {Events, NewBoard} ->
            maybe_newgame({flag, Events, NewBoard})
    end;
step(unsafe, Board) ->
    {Events, NewBoard} = uncover_unsafe(Board),
    maybe_newgame({flag, Events, NewBoard}).

%% Mines in an uncovered state, for which the number of covered
%% borders (flagged or not) is equal to the number of neighbour
%% mines will have all their covered neighbours flagged as mines.
flag_apparent_mines(Board) ->
    CondFun =
        fun(NCovers, NFlags, NMines) ->
                if NCovers + NFlags == NMines, NCovers > 0 -> true;
                   true -> false
                end
        end,
    Seqs = solver_prepare(CondFun, Board),
    solver_action(flag, Seqs, Board).

%% If the number of flagged neighbours is equal to the number
%% of surrounding mines, and there are other covered neighbours
%% that are not flagged, it is safe to uncover those.
%%
%% This step should be done repeatedly until no more fields can be
%% uncovered.
uncover_safe_areas(Board) ->
    uncover_safe_areas([], Board).

uncover_safe_areas(Events, Board) ->
    CondFun =
        fun(NCovers, NFlags, NMines) ->
                if NFlags == NMines, NMines > 0, NCovers > 0 -> true;
                   true -> false
                end
        end,
    Seqs = solver_prepare(CondFun, Board),
    case solver_action(step, Seqs, Board) of
        {[], Board} ->
            {Events, Board};
        {NewEvents, NewBoard} ->
            uncover_safe_areas(NewEvents ++ Events, NewBoard)
    end.

%% Dangerous step that potentially leads to losing the game.
%% Should be done only when the above two methods do not yield moves.
uncover_unsafe(Board) ->
    Seq = random_uncovered(Board),
    solver_action(step, [Seq], Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_newgame({_State, [{game_over, _} | _] = Events, Board}) ->
    {newgame, Events, Board};
maybe_newgame(ReturnTuple) -> ReturnTuple.

%% Choose a random covered field to uncover. Desperate and dangerous.
random_uncovered(#board{dims=Dims, fields=Fields} = Board) ->
    {Rows, Cols} = Dims,
    NFields = Rows * Cols,
    Seq = random:uniform(NFields) - 1,
    #field{status=St} = barr:get(Seq, Fields),
    if St =:= covered orelse St =:= questioned -> Seq;
       true -> random_uncovered(Board)
    end.

%% Solver function to iterate over all the uncovered fields and collect
%% all their neighbours fulfilling certain conditions to perform action on.
solver_prepare(CondFun, #board{fields=Fields}) ->
    Seqs0 =
        barr:foldl(fun(_Seq, #field{status=uncovered,
                                    neighbours=Nbrs}=F, L) ->
                           NCovers = board:nbr_covers(F, Fields),
                           NFlags = board:nbr_flags(F, Fields),
                           NMines = board:nbr_mines(F, Fields),
                           case CondFun(NCovers, NFlags, NMines) of
                               true  -> [solver_filter(Nbrs, Fields) | L];
                               false -> L
                           end;
                      (_Seq, #field{}, L) -> L
                   end, [], Fields),
    lists:usort(lists:flatten(Seqs0)).

%% Perform a certain board action on pre-selected fields.
solver_action(Action, Seqs, Board) ->
    lists:foldl(fun(Seq, {EvA, BoA}) ->
                        board:Action(Seq, BoA, EvA)
                end, {[], Board}, Seqs).

%% Filter only those fields the solver should act upon.
%% These are fields that are covered but not flagged.
solver_filter(Seqs, Fields) ->
    Fi = fun(Seq) ->
                 #field{status=St} = barr:get(Seq, Fields),
                 St =:= covered
         end,
    lists:filter(Fi, Seqs).
