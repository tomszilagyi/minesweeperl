-module(barr).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([ new/2
        , get/2
        , set/3
        , map/2
        , foldl/3
        ]).

new(Size, Default) ->
    array:new([{size, Size}, {default, Default}]).

get(Idx, Array) ->
    array:get(Idx, Array).

set(Idx, Value, Array) ->
    array:set(Idx, Value, Array).

map(Fun, Array) ->
    array:map(Fun, Array).

foldl(Fun, Acc0, Array) ->
    array:foldl(Fun, Acc0, Array).
