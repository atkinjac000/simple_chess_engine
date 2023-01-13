-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(ATX, ATY)) -> list({ATX, ATY}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({AUH, AUI})) -> map_(AUH, AUI).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(AUR, any()), AUR) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(AVH, AVI), AVH) -> {ok, AVI} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(AVT, AVU), AVT, AVU) -> map_(AVT, AVU).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(AWF, AWG), fun((AWF, AWG) -> AWJ)) -> map_(AWF, AWJ).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(AWT, any())) -> list(AWT).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), AXE)) -> list(AXE).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AXN, AXO), fun((AXN, AXO) -> boolean())) -> map_(AXN, AXO).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AXZ, AYA), list(AXZ)) -> map_(AXZ, AYA).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AYN, AYO), map_(AYN, AYO)) -> map_(AYN, AYO).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AZD, AZE), AZD) -> map_(AZD, AZE).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(AZP, AZQ), list(AZP)) -> map_(AZP, AZQ).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(AZW, AZX), AZW, fun((gleam@option:option(AZX)) -> AZX)) -> map_(AZW, AZX).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({BAD, BAE}), BAG, fun((BAG, BAD, BAE) -> BAG)) -> BAG.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(BAH, BAI), BAL, fun((BAL, BAH, BAI) -> BAL)) -> BAL.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
