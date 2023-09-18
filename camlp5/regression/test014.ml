open Ostap
open Types_
open Matcher

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Re.Str.regexp "[0-9]+" in
  object (self)

    inherit Matcher.t s

    method! skip p c = skip s p c
    method getCONST : 'b . (Token.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result = self#get "constant" const

  end

ostap (
  n:
     c:CONST {`N c} ;

  t:
     -"(" a:e -")" {`TBr a}
   | a:n {`TN a} ;

  e:
     a:e -"+" b:t {`EAdd (a, b)}
   | a:e -"-" b:t {`ESub (a, b)}
   | a:t {`ET a} ;

  main: e -EOF
)

let _ =
  let rec print r =
    match r with
    | `N _ -> "n"
    | `TN a -> "T[" ^ (print a) ^ "]"
    | `TBr a -> "T[" ^ "(" ^ (print a) ^ ")" ^ "]"
    | `ET a -> "E[" ^ (print a) ^ "]"
    | `EAdd (a, b) -> "E[" ^ (print a) ^ "+" ^ (print b) ^ "]"
    | `ESub (a, b) -> "E[" ^ (print a) ^ "-" ^ (print b) ^ "]"
  in
  match main (new lexer "1-(2+3)+4") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed:\n"
