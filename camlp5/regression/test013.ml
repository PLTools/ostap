open Ostap
open Types
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
  primary:
    c:CONST {`N c} ;

  exp:
     e:exp -"+" p:primary {`E2 (e, p)}
   | p:primary {`E1 p} ;

  main: exp -EOF
)

let _ =
  let rec print r =
    match r with
    | `N p -> "n"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  match main (new lexer "1+2+3") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
  | Failed _ -> Printf.printf "Not parsed.\n"
