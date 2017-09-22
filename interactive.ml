#require "tjr_minifs";;

open Tjr_minifs;;

#use "test.ml";;

open Mini_unix

open S
open T

let ops = mk_ops()

let run = run_imperative

let dh = run @@ ops.opendir "."

let _ = run @@ ops.readdir dh

module Extra_ops_ = Extra_ops(struct let ops=ops end)

open Extra_ops_

let _ = readdir "."

