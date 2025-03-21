open Ostap
open Types
open Combinators
open Matcher

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let const = Re.Str.regexp "[0-9]+" in
  object (self)
    inherit Matcher.t s

    method! skip p c = skip s p c
    method getCONST : 'b . (Token.t -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      self#get "constant"   const

  end

module Test0 = struct
  let e p s = fix (fun e -> ostap(x:e "+" y:e {`Add (x, y)} | p {`Var})) s

  ostap (
    main: e[ostap("a")] -EOF
  )

  let rec print r =
    match r with
    | `Add (x, y) -> (print x) ^ " + " ^ (print y)
    | `Var -> "a"
    | `I _ -> "I"
end

module Test1 = struct
  let n = ostap (a:CONST {`N a})

  let rec t eta = ostap ("(" a:e ")" {`TBr a} | a:n {`TN a}) eta
  and e s = fix (fun e -> ostap ( a:e "+" b:t {`EAdd (a, b)}
                                | a:e "-" b:t {`ESub (a, b)}
                                | a:t {`ET a}
                                )) s

  let main = ostap (e -EOF)

  let rec print r =
    match r with
    | `N _ -> "n"
    | `TN a -> "T[" ^ (print a) ^ "]"
    | `TBr a -> "T[" ^ " ( " ^ (print a) ^ " ) " ^ "]"
    | `ET a -> "E[" ^ (print a) ^ "]"
    | `EAdd (a, b) -> "E[" ^ (print a) ^ "+" ^ (print b) ^ "]"
    | `ESub (a, b) -> "E[" ^ (print a) ^ "-" ^ (print b) ^ "]"
end

module Test2 = struct
  let rec exp s = fix (fun exp -> ostap ( i:inner "+" e:exp {`Add (`Inner i, `Exp e)}
                                    | i:inner {`Inner i}
                                    )) s
  and inner s = fix (fun inner -> ostap ( i:inner "-" p:primary {`Sub (`Inner i, `Primary p)}
                                        | p:primary {`Primary p}
                                        )) s
  and primary = ostap (c:CONST {`Const c})

  let main = ostap (exp -EOF)
  let rec print r =
    match r with
    | `Primary p -> print p
    | `Const c -> Ostap.Matcher.Token.toString c
    | `Sub (`Inner i, `Primary p) -> (print i) ^ " - " ^ (print p)
    | `Add (`Inner i, `Exp e) -> (print i) ^ " + " ^ (print e)
    | `Exp e -> print e
    | `Inner i -> print i
end

module Test3 = struct
  let rec primary = ostap (c:CONST {`N c})
  and inner s = fix (fun inner -> ostap ( i:inner "-" p:primary {`I2 (i, p)}
                                        | p:primary {`I1 p}
                                        )) s
  and exp s = fix (fun exp -> ostap ( i:inner "+" e:exp {`E2 (i, e)}
                                    | i:inner {`E1 i}
                                    )) s
  let main = ostap(memo[exp] -EOF)

  let rec print r =
    match r with
    | `N p -> "n"
    | `I1 i -> "M[" ^ (print i) ^ "]"
    | `I2 (i, p) -> "M[" ^ (print i) ^ "-" ^ (print p) ^ "]"
    | `E1 e -> "E[" ^ (print e) ^ "]"
    | `E2 (i, e) -> "E[" ^ (print i) ^ "+" ^ (print e) ^ "]"
end

module Test4 = struct
  let rec l eta = ostap (a:p "." "x" {`Lp a} | "x" {`Lx}) eta
  and p s = fix (fun p -> ostap (a:p "(" "n" ")" {`Pn a} | a:p "." "x" {`Pl (`Lp a)} | "x" {`Pl `Lx})) s
  let main = ostap (p -EOF)

  let rec print r =
    match r with
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
end

module Test5 = struct
  ostap (
    l:
       a:memo[p] -"." -"x" {`Lp a}
     | -"x" {`Lx} ;

    p:
       a:memo[p] -"(" -"n" -")" {`Pn a}
     | a:memo[p] -"." -"x" {`Pl (`Lp a)}
     | -"x" {`Pl `Lx} ;
     (*| a:memo[l] {`Pl a} ; *)

    main: memo[l] -EOF
  )

  let rec print r =
    match r with
    | `Lp a -> "L[" ^ (print a) ^ ".x]"
    | `Lx -> "L[x]"
    | `Pn a -> "P[" ^ (print a) ^ "(n)]"
    | `Pl a -> "P[" ^ (print a) ^ "]"
end

module Test6 = struct
  ostap (
    l: a:memo[p] -"b" {`Lp a} ;

    p:
       a:memo[p] -"b" {`Pn a}
     | -"a" {`Pl } ;

    main: memo[l] -EOF
  )

  let rec print r =
    match r with
    | `Lp a -> (print a) ^ "b"
    | `Pn a -> (print a) ^ "b"
    | `Pl -> "a"
end

module Test7 = struct
  ostap (
    l: (-"b")* -"b" {`Lp } ;

    main: memo[l] -EOF
  )

  let print r =
    match r with
    | `Lp -> "aha!"
end

let _ =
  let input =
    if Array.length Sys.argv < 2
    then "a+a"
    else Sys.argv.(1)
  in
  match Test0.main (new lexer input) (fun res s -> match res with
                                              | `Add (_,_)
	                                            | `I _ -> Parsed ((res, s), None)
                                              | _ -> failwith "Not implemented") with
  | Parsed ((b, _), _) -> Printf.printf "Parsed. %s\n" (Test0.print b)
  | Failed m -> Printf.printf "Not parsed:\n%s\n" (Reason.toString `All `Acc m)
  | Empty -> failwith "not implemented"
