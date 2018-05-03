
open Util
open StringSetMap

type term =
        |Var of string
        |Lam of string * term
        |App of term * term

