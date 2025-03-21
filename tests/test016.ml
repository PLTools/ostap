open Ostap
open Types
open Matcher

module H = Hashtbl.Make(struct
                         type t = int * int
                         let equal (f0, p0) (f1, p1) = f0 == f1 && p0 = p1
                         let hash = Hashtbl.hash
                       end)


class lexer (str :  string) =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  object (self : 'self)

    inherit Matcher.t str

    method! skip p c = skip str p c
  end

ostap (
  l:
    a:p -"." -"x" {`Lp a}
   | -"x" {`Lx} ;

  p:
     a:p -"(" -"n" -")" {`Pn a}
   | a:l {`Pl a} ;

  main: l -EOF
)

[@@@ocaml.warning "-20"]

let _ =
  let rec print r =
    match r with
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
  in
  match Combinators.Mem.mapply (Combinators.Mem.memoize main) (new lexer "x(n)(n).x(n).x") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed:\n"
  | Empty -> failwith "Not implemented"
