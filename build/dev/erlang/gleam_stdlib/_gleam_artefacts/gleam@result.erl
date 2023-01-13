-module(gleam@result).
-compile(no_auto_import).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _@1} ->
            false;

        {ok, _@2} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _@1} ->
            false;

        {error, _@2} ->
            true
    end.

-spec map({ok, BZG} | {error, BZH}, fun((BZG) -> BZK)) -> {ok, BZK} |
    {error, BZH}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BZN} | {error, BZO}, fun((BZO) -> BZR)) -> {ok, BZN} |
    {error, BZR}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BZU} | {error, BZV}} | {error, BZV}) -> {ok, BZU} |
    {error, BZV}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, CAC} | {error, CAD}, fun((CAC) -> {ok, CAG} | {error, CAD})) -> {ok,
        CAG} |
    {error, CAD}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, CAL} | {error, any()}, CAL) -> CAL.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, CAP} | {error, any()}, fun(() -> CAP)) -> CAP.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, CAU}, CAU) -> CAU.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, CAX} | {error, CAX}) -> CAX.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, CBA} | {error, any()}) -> {ok, CBA} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, CBG} | {error, CBH}, {ok, CBG} | {error, CBH}) -> {ok, CBG} |
    {error, CBH}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, CBO} | {error, CBP}, fun(() -> {ok, CBO} | {error, CBP})) -> {ok,
        CBO} |
    {error, CBP}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, CBW} | {error, CBX})) -> {ok, list(CBW)} | {error, CBX}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, CCF}, CCI) -> {ok, CCI} | {error, CCF}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CCL} | {error, any()}, CCP) -> {ok, CCL} | {error, CCP}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, CCS} | {error, any()})) -> list(CCS).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
