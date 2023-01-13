-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, interleave/1, transpose/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(EZ) :: {continue, EZ} | {stop, EZ}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(FE)) -> list(FE).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(FM), FM) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | _@1] when Head =:= Elem ->
            true;

        [_@2 | Tail] ->
            contains(Tail, Elem)
    end.

-spec first(list(FO)) -> {ok, FO} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _@1] ->
            {ok, X}
    end.

-spec rest(list(FS)) -> {ok, list(FS)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_@1 | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(FX), fun((FX) -> boolean()), list(FX)) -> list(FX).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(GB), fun((GB) -> boolean())) -> list(GB).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(GE), fun((GE) -> {ok, GG} | {error, any()}), list(GG)) -> list(GG).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _@1} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(GM), fun((GM) -> {ok, GO} | {error, any()})) -> list(GO).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(GT), fun((GT) -> GV), list(GV)) -> list(GV).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(GY), fun((GY) -> HA)) -> list(HA).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec map_fold(list(HC), HE, fun((HE, HC) -> {HE, HF})) -> {HE, list(HF)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec do_index_map(list(HH), fun((integer(), HH) -> HJ), integer(), list(HJ)) -> list(HJ).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(HM), fun((integer(), HM) -> HO)) -> list(HO).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(HQ), fun((HQ) -> {ok, HS} | {error, HT}), list(HS)) -> {ok,
        list(HS)} |
    {error, HT}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(IA), fun((IA) -> {ok, IC} | {error, ID})) -> {ok, list(IC)} |
    {error, ID}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(IJ), integer()) -> list(IJ).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_@1 | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(IM), integer(), list(IM)) -> list(IM).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(IQ), integer()) -> list(IQ).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(IV), list(IV)) -> list(IV).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(JD), JD) -> list(JD).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(JG), list(JG)) -> list(JG).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [Head | Tail] ->
            reverse_and_prepend(Tail, [Head | Suffix])
    end.

-spec do_flatten(list(list(JK)), list(JK)) -> list(JK).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_flatten(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec flatten(list(list(JP))) -> list(JP).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(JT), fun((JT) -> list(JV))) -> list(JV).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(JY), KA, fun((KA, JY) -> KA)) -> KA.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec fold_right(list(KB), KD, fun((KD, KB) -> KD)) -> KD.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(KE), KG, fun((KG, KE, integer()) -> KG), integer()) -> KG.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(KH), KJ, fun((KJ, KH, integer()) -> KJ)) -> KJ.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(KK), KM, fun((KM, KK) -> {ok, KM} | {error, KN})) -> {ok,
        KM} |
    {error, KN}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {error, _try} -> {error, _try};
                {ok, Accumulator@1} ->
                    try_fold(Rest, Accumulator@1, Fun)
            end
    end.

-spec fold_until(list(KS), KU, fun((KU, KS) -> continue_or_stop(KU))) -> KU.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(KW), fun((KW) -> boolean())) -> {ok, KW} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _@1 ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(LA), fun((LA) -> {ok, LC} | {error, any()})) -> {ok, LC} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _@1 ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(LI), fun((LI) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    all(Tail, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(LK), fun((LK) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    true;

                false ->
                    any(Tail, Predicate)
            end
    end.

-spec do_zip(list(LM), list(LO), list({LM, LO})) -> list({LM, LO}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_@1, _@2} ->
            reverse(Acc)
    end.

-spec zip(list(LS), list(LU)) -> list({LS, LU}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(LX), list(LZ)) -> {ok, list({LX, LZ})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({MI, MJ}), list(MI), list(MJ)) -> {list(MI), list(MJ)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({MI, MJ})) -> {list(MI), list(MJ)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(MN), MN, list(MN)) -> list(MN).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(MR), MR) -> list(MR).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_@1] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(MU), integer()) -> {ok, MU} | {error, nil}.
at(List, Index) ->
    _pipe = List,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-spec unique(list(MY)) -> list(MY).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(NB),
    list(NB),
    list(NB),
    fun((NB, NB) -> gleam@order:order())
) -> list(NB).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _@1, _@2} ->
            Acc;

        {_@3, 0, [Ax | Ar], _@4} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _@5, _@6, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_@7, _@8, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _@9 ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end
    end.

-spec merge_down(
    integer(),
    integer(),
    list(NG),
    list(NG),
    list(NG),
    fun((NG, NG) -> gleam@order:order())
) -> list(NG).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _@1, _@2} ->
            Acc;

        {_@3, 0, [Ax | Ar], _@4} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _@5, _@6, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_@7, _@8, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _@9 ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end
    end.

-spec merge_sort(
    list(NL),
    integer(),
    fun((NL, NL) -> gleam@order:order()),
    boolean()
) -> list(NL).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln
                        - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln
                        - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(NO), fun((NO, NO) -> gleam@order:order())) -> list(NO).
sort(List, Compare) ->
    merge_sort(List, length(List), Compare, true).

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            reverse([Stop | Acc]);

        gt ->
            tail_recursive_range(Start - 1, Stop, [Start | Acc]);

        lt ->
            tail_recursive_range(Start + 1, Stop, [Start | Acc])
    end.

-spec do_repeat(NU, integer(), list(NU)) -> list(NU).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(NX, integer()) -> list(NX).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(NZ), integer(), list(NZ)) -> {list(NZ), list(NZ)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(OE), integer()) -> {list(OE), list(OE)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(OI), fun((OI) -> boolean()), list(OI)) -> {list(OI),
    list(OI)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _@1 ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(ON), fun((ON) -> boolean())) -> {list(ON), list(ON)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({OR, OS}), OR) -> {ok, OS} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(PA), fun((PA) -> boolean()), list(PA)) -> {ok, {PA, list(PA)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(PA), fun((PA) -> boolean())) -> {ok, {PA, list(PA)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(PJ), fun((PJ) -> {ok, PL} | {error, any()}), list(PJ)) -> {ok,
        {PL, list(PJ)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _@1} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(PJ), fun((PJ) -> {ok, PL} | {error, any()})) -> {ok,
        {PL, list(PJ)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({PS, PT}), PS) -> {ok, {PT, list({PS, PT})}} | {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _@1 ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({PY, PZ}), PY, PZ) -> list({PY, PZ}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _@1} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(QC), fun((QC) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec do_partition(list(QK), fun((QK) -> boolean()), list(QK), list(QK)) -> {list(QK),
    list(QK)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(QK), fun((QK) -> boolean())) -> {list(QK), list(QK)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(QO)) -> list(list(QO)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _@1 ->
            _pipe = L,
            _pipe@5 = index_map(
                _pipe,
                fun(I_idx, I) ->
                    _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@5)
    end.

-spec do_window(list(list(QS)), list(QS), integer()) -> list(list(QS)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(QY), integer()) -> list(list(QY)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(RC)) -> list({RC, RC}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(RF), fun((RF) -> boolean())) -> list(RF).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(RI), fun((RI) -> boolean()), list(RI)) -> list(RI).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    do_take_while(Tail, Predicate, [Head | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(RM), fun((RM) -> boolean())) -> list(RM).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(RP), fun((RP) -> RR), RR, list(RP), list(list(RP))) -> list(list(RP)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [Head | Tail] ->
            Key = F(Head),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Tail, F, Key, [Head], New_acc);

                _@1 ->
                    do_chunk(Tail, F, Key, [Head | Current_chunk], Acc)
            end;

        _@2 ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(RX), fun((RX) -> any())) -> list(list(RX)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(list(SC), integer(), integer(), list(SC), list(list(SC))) -> list(list(SC)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [Head | Tail] ->
            Chunk = [Head | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Tail,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Tail, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(SJ), integer()) -> list(list(SJ)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(SN), fun((SN, SN) -> SN)) -> {ok, SN} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec do_scan(list(SR), ST, list(ST), fun((ST, SR) -> ST)) -> list(ST).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(SW), SY, fun((SY, SW) -> SY)) -> list(SY).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(TA)) -> {ok, TA} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(TE), integer()) -> list(list(TE)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _@1 ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(TI)) -> list(list({TI, TI})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(TM)) -> list({TM, TM}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec interleave(list(list(TP))) -> list(TP).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec transpose(list(list(TT))) -> list(list(TT)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _@1] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec do_shuffle_pair_unwrap(list({float(), TY}), list(TY)) -> list(TY).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        _@1 ->
            [Elem_pair | Enumerable] = List,
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), UC})) -> list({float(), UC}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(UF)) -> list(UF).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
