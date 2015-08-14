-module(solver).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([ flag_apparent_mines/1
        , uncover_safe_areas/1
        , uncover_unsafe/1
        , random_uncovered/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/minesweeperl.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    CondFun =
        fun(NCovers, NFlags, NMines) ->
                if NFlags == NMines, NMines > 0, NCovers > 0 -> true;
                   true -> false
                end
        end,
    Seqs = solver_prepare(CondFun, Board),
    solver_action(step, Seqs, Board).

%% Dangerous step that potentially leads to losing the game.
%% Should be done only when the above two methods do not yield moves.
uncover_unsafe(Board) ->
    Seq = random_uncovered(Board),
    solver_action(step, [Seq], Board).

random_uncovered(#board{dims=Dims, fields=Fields} = Board) ->
    {Rows, Cols} = Dims,
    NFields = Rows * Cols,
    Seq = random:uniform(NFields),
    #field{status=St} = array:get(Seq, Fields),
    if St =:= covered orelse St =:= questioned -> Seq;
       true -> random_uncovered(Board)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Solver function to iterate over all the uncovered fields and collect
%% all their neighbours fulfilling certain conditions to perform action on.
solver_prepare(CondFun, #board{fields=Fields}) ->
    Seqs0 =
        array:foldl(fun(_Seq, #field{status=uncovered,
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
                 #field{status=St} = array:get(Seq, Fields),
                 St =:= covered
         end,
    lists:filter(Fi, Seqs).
