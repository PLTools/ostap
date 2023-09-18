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
  primary:
     c:CONST {`N c} ;

  inner:
     i:inner -"-" p:primary {`I2 (i, p)}
   | p:primary {`I1 p} ;

  exp:
     i:inner -"+" e:exp {`E2 (i, e)}
   | i:inner {`E1 i} ;

  main: exp -EOF
)

let _ =
  let rec print r =
    match r with
    | `N p -> "n"
    | `I1 i -> "M[" ^ (print i) ^ "]"
    | `I2 (i, p) -> "M[" ^ (print i) ^ "-" ^ (print p) ^ "]"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
  in
  let run input =
    match main (new lexer input) (fun res s -> Parsed ((res, s), None)) with
    | Parsed ((b, _), _) -> Printf.printf "Parsed: %s\n" (print b)
    | Failed _ -> Printf.printf "Not parsed:\n"
  in
  let input0 = "1+2+3" in
  let input1 = "1-2-3" in
  run input0;
  run input1
