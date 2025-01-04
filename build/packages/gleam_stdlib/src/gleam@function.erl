-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 2).
-spec compose(fun((EGG) -> EGH), fun((EGH) -> EGI)) -> fun((EGG) -> EGI).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 7).
-spec curry2(fun((EGJ, EGK) -> EGL)) -> fun((EGJ) -> fun((EGK) -> EGL)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 12).
-spec curry3(fun((EGN, EGO, EGP) -> EGQ)) -> fun((EGN) -> fun((EGO) -> fun((EGP) -> EGQ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 17).
-spec curry4(fun((EGS, EGT, EGU, EGV) -> EGW)) -> fun((EGS) -> fun((EGT) -> fun((EGU) -> fun((EGV) -> EGW)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 22).
-spec curry5(fun((EGY, EGZ, EHA, EHB, EHC) -> EHD)) -> fun((EGY) -> fun((EGZ) -> fun((EHA) -> fun((EHB) -> fun((EHC) -> EHD))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 27).
-spec curry6(fun((EHF, EHG, EHH, EHI, EHJ, EHK) -> EHL)) -> fun((EHF) -> fun((EHG) -> fun((EHH) -> fun((EHI) -> fun((EHJ) -> fun((EHK) -> EHL)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 36).
-spec flip(fun((EHN, EHO) -> EHP)) -> fun((EHO, EHN) -> EHP).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 42).
-spec identity(EHQ) -> EHQ.
identity(X) ->
    X.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 47).
-spec constant(EHR) -> fun((any()) -> EHR).
constant(Value) ->
    fun(_) -> Value end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 56).
-spec tap(EHT, fun((EHT) -> any())) -> EHT.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 62).
-spec apply1(fun((EHV) -> EHW), EHV) -> EHW.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 67).
-spec apply2(fun((EHX, EHY) -> EHZ), EHX, EHY) -> EHZ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-file("/Users/louis/src/gleam/stdlib/src/gleam/function.gleam", 72).
-spec apply3(fun((EIA, EIB, EIC) -> EID), EIA, EIB, EIC) -> EID.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
