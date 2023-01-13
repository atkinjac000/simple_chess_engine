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

-spec map({ok, CCA} | {error, CCB}, fun((CCA) -> CCE)) -> {ok, CCE} |
    {error, CCB}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, CCH} | {error, CCI}, fun((CCI) -> CCL)) -> {ok, CCH} |
    {error, CCL}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, CCO} | {error, CCP}} | {error, CCP}) -> {ok, CCO} |
    {error, CCP}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, CCW} | {error, CCX}, fun((CCW) -> {ok, CDA} | {error, CCX})) -> {ok,
        CDA} |
    {error, CCX}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, CDF} | {error, any()}, CDF) -> CDF.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, CDJ} | {error, any()}, fun(() -> CDJ)) -> CDJ.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, CDO}, CDO) -> CDO.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, CDR} | {error, CDR}) -> CDR.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, CDU} | {error, any()}) -> {ok, CDU} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, CEA} | {error, CEB}, {ok, CEA} | {error, CEB}) -> {ok, CEA} |
    {error, CEB}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, CEI} | {error, CEJ}, fun(() -> {ok, CEI} | {error, CEJ})) -> {ok,
        CEI} |
    {error, CEJ}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, CEQ} | {error, CER})) -> {ok, list(CEQ)} | {error, CER}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, CEZ}, CFC) -> {ok, CFC} | {error, CEZ}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CFF} | {error, any()}, CFJ) -> {ok, CFF} | {error, CFJ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, CFM} | {error, any()})) -> list(CFM).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
