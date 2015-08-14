-record(field,
        { has_mine = false  :: true | false
        , status = covered
                  :: covered    % field is in its initial, hidden state
                  | flagged     % player or solver flagged field
                  | questioned  % player marked field with '?'
                  | uncovered   % stepped on or uncovered indirectly
                  | exploded    % this is a mine, and player has stepped on it
        , neighbours :: [pos_integer()] % list of field seq numbers
        }).

-record(board,
        { dims       :: {pos_integer(), pos_integer()}
        , n_mines    :: pos_integer()
        , n_hidden   :: pos_integer()         % countdown to n_mines: game ends
        , fields     :: array:array(#field{}) % access fields via seq number
        }).
