-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((AQQ) -> AQR), fun((AQR) -> AQS)) -> fun((AQQ) -> AQS).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((AQT, AQU) -> AQV)) -> fun((AQT) -> fun((AQU) -> AQV)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((AQX, AQY, AQZ) -> ARA)) -> fun((AQX) -> fun((AQY) -> fun((AQZ) -> ARA))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((ARC, ARD, ARE, ARF) -> ARG)) -> fun((ARC) -> fun((ARD) -> fun((ARE) -> fun((ARF) -> ARG)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((ARI, ARJ, ARK, ARL, ARM) -> ARN)) -> fun((ARI) -> fun((ARJ) -> fun((ARK) -> fun((ARL) -> fun((ARM) -> ARN))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((ARP, ARQ, ARR, ARS, ART, ARU) -> ARV)) -> fun((ARP) -> fun((ARQ) -> fun((ARR) -> fun((ARS) -> fun((ART) -> fun((ARU) -> ARV)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((ARX, ARY) -> ARZ)) -> fun((ARY, ARX) -> ARZ).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(ASA) -> ASA.
identity(X) ->
    X.

-spec constant(ASB) -> fun((any()) -> ASB).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(ASD, fun((ASD) -> any())) -> ASD.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((ASF) -> ASG), ASF) -> ASG.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((ASH, ASI) -> ASJ), ASH, ASI) -> ASJ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((ASK, ASL, ASM) -> ASN), ASK, ASL, ASM) -> ASN.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
