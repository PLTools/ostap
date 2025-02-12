open Ostap

@type t =
| Arg of GT.int | Cond of GT.int | Map of GT.int * GT.int * t GT.list
| Region of GT.int | Param of GT.string | QParam of GT.string
with show

let _: t -> string  = GT.show t

let parse s =
  let strip s = String.sub s 1 (String.length s - 1) in
  ostap (
    n:RIDENT     {Arg  n}
  | "?" n:RIDENT {Cond n}
  | "forall" n:RIDENT "in" a:RIDENT exp:(  r:EXPREGION {Region r}
                                         | p:PARAM     {Param  (strip p)}
                                         | "`" p:PARAM {QParam (strip p)}
                                        )* {Map (n, a, exp)}
  ) s