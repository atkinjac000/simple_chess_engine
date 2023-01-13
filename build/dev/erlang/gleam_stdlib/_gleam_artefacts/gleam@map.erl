-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(ARD, ARE)) -> list({ARD, ARE}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({ARN, ARO})) -> map_(ARN, ARO).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(ARX, any()), ARX) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(ASN, ASO), ASN) -> {ok, ASO} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(ASZ, ATA), ASZ, ATA) -> map_(ASZ, ATA).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(ATL, ATM), fun((ATL, ATM) -> ATP)) -> map_(ATL, ATP).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(ATZ, any())) -> list(ATZ).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), AUK)) -> list(AUK).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AUT, AUU), fun((AUT, AUU) -> boolean())) -> map_(AUT, AUU).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AVF, AVG), list(AVF)) -> map_(AVF, AVG).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AVT, AVU), map_(AVT, AVU)) -> map_(AVT, AVU).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AWJ, AWK), AWJ) -> map_(AWJ, AWK).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(AWV, AWW), list(AWV)) -> map_(AWV, AWW).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(AXC, AXD), AXC, fun((gleam@option:option(AXD)) -> AXD)) -> map_(AXC, AXD).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({AXJ, AXK}), AXM, fun((AXM, AXJ, AXK) -> AXM)) -> AXM.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(AXN, AXO), AXR, fun((AXR, AXN, AXO) -> AXR)) -> AXR.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
