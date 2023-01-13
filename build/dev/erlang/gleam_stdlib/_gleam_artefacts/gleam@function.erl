-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EDJ) -> EDK), fun((EDK) -> EDL)) -> fun((EDJ) -> EDL).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EDM, EDN) -> EDO)) -> fun((EDM) -> fun((EDN) -> EDO)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EDQ, EDR, EDS) -> EDT)) -> fun((EDQ) -> fun((EDR) -> fun((EDS) -> EDT))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EDV, EDW, EDX, EDY) -> EDZ)) -> fun((EDV) -> fun((EDW) -> fun((EDX) -> fun((EDY) -> EDZ)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EEB, EEC, EED, EEE, EEF) -> EEG)) -> fun((EEB) -> fun((EEC) -> fun((EED) -> fun((EEE) -> fun((EEF) -> EEG))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EEI, EEJ, EEK, EEL, EEM, EEN) -> EEO)) -> fun((EEI) -> fun((EEJ) -> fun((EEK) -> fun((EEL) -> fun((EEM) -> fun((EEN) -> EEO)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EEQ, EER) -> EES)) -> fun((EER, EEQ) -> EES).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EET) -> EET.
identity(X) ->
    X.

-spec constant(EEU) -> fun((any()) -> EEU).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EEW, fun((EEW) -> any())) -> EEW.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EEY) -> EEZ), EEY) -> EEZ.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EFA, EFB) -> EFC), EFA, EFB) -> EFC.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EFD, EFE, EFF) -> EFG), EFD, EFE, EFF) -> EFG.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
