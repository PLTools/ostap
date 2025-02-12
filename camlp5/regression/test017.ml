(*
 * test017: regression test.
 * Copyright (C) 2006-2008
 * Dmitri Boulytchev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Re.Str
open Ostap
open Types
open Printf

class lexer (s : string) =
  object (self : 'self)

    val ws    = regexp "[' ''\n''\t']+"
    val ident = regexp "[a-zA-Z]\([a-zA-Z0-9]\)*"
    val const = Re.Str.regexp "[0-9]+"
    val p = 0
    val s = s

    method pos = p
    method str = s
    method equal : 'self -> bool =
      fun s' -> (s = s' # str) && (p = s' # pos)

    method getIDENT : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if string_match ident s p
        then
          let m = matched_string s in
          k m {< p = p + String.length m >}
        else
          emptyResult

    method getCONST : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if string_match const s p
        then
          let m = matched_string s in
          k m {< p = p + String.length m >}
        else
          emptyResult

    method look : 'b . string -> (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun cs k ->
        try
          let l = String.length cs in
          let m = String.sub s p l in
          let p = p + l in
          if cs = m
          then k m {< p = p >}
          else emptyResult
        with Invalid_argument _ -> emptyResult

    method getEOF : 'b . (string -> 'self -> ('self, 'b, Reason.t) result) -> ('self, 'b, Reason.t) result =
      fun k ->
        let p =
          if string_match ws s p
          then p + (String.length (matched_string s))
          else p
        in
        if p = String.length s
        then k "EOF" self
        else emptyResult
  end

let left  f c x y = f (c x) y
let right f c x y = c (f x y)

ostap (
    id[x]: x
  )

type t = Left | None | Right

(*
let expr ops opnd =
  let ops =
    Array.map
      (fun (assoc, list) ->
         let g = match assoc with Lefta | Nona -> left | Righta -> right in
         assoc = Nona, Ostap.Combinators.altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id   x = x                in
  let ostap (
      inner[l][c]:
          {n = l                } => x:opnd {c x}
        | {n > l && not (nona l)} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
             match b with None -> c x | Some x -> x
          }
        | {n > l && nona l} => x:inner[l+1][id] b:(op[l] inner[l+1][id])? {
            c (match b with None -> x | Some (o, y) -> o id x y)
          }
    )
  in
  ostap (inner[0][id])
 *)
(*
let mixfixExpr ops opnd =
  let lastPart l = List.hd (List.rev l) in
  let fstParts l = List.rev @@ List.tl @@ List.rev l in
  let midParts l = List.tl (fstParts l) in
  let constrOpart opart opnd = List.fold_right2 (fun opart' opnd' acc' -> ostap (o:opart' x:opnd' a:acc' {x::a}))
                                                (fstParts opart)
                                                 opnd
                                                (ostap (!(lastPart @@ opart) {[]}))
  in
  let applyOp oparts opnds =
    List.fold_right2 (fun opart opnd acc ->
                        match opart with
                        | `Simpl  opart             -> ostap (o:opart x:(!(lastPart opnd)) a:acc {x::a})
                        | `Compl (opart, folder, _) -> ostap (o:(constrOpart[opart][fstParts opnd])* x:(!(lastPart opnd)) a:acc {(folder o) :: x :: a})
                     )
                     (fstParts oparts)
                     (fstParts opnds)
                     (ostap (!(match lastPart oparts with
                               | `Simpl  opart             -> ostap (o:opart {[]})
                               | `Compl (opart, folder, _) -> ostap (o:(constrOpart[opart][lastPart opnds])* {[folder o]})
                        )))
  in
  let n       = Array.length ops in
  let assoc l = fst ops.(l) in
  let id x    = x in
  let ostap (
      inner[l][c] : !(
    if n = l then
      ostap (x:opnd {c x})
    else
      Ostap.Combinators.altl
        (List.fold_left
           (fun acc elem -> (
              let oparts = fst elem in
              let opsema = fst (snd elem) in
              let oplev  = snd (snd elem) in
              let generateOpnds oparts oplev =
                (List.map2
                  (fun opart l ->
                     match opart with
                     | `Simpl _              -> [inner l id]
                     | `Compl (opart, _, ls) -> (List.map (fun l' -> inner l' id) ls) @ [inner l id]
                  )
                  (fstParts oparts)
                   oplev
                ) @
                [match lastPart oparts with
                 | `Simpl _              -> []
                 | `Compl (opart, _, ls) -> (List.map (fun l' -> inner l' id) ls)
                ]
              in
              if (assoc l = Lefta) then
                let midPart = if ((List.length oparts) = 2) then ostap ("" {[]})
                              else applyOp (midParts oparts) (generateOpnds (midParts oparts) (midParts oplev))
                in
                let fstPart =
                  let lvl = List.hd oplev in
                  match List.hd oparts with
                  | `Simpl _ ->
                    ostap (x:inner[lvl][id] {[c x]})
                  | `Compl (opart, folder, ls) ->
                    let opnds  = (List.map (fun l' -> inner l' id) ls) in
                    let opnds' = (ostap (x:inner[List.hd ls][id] {c x})) :: (List.tl opnds) in
                    ostap (x:inner[lvl][id] {[c x]}
                          | pre:constrOpart[opart][opnds'] o:(constrOpart[opart][opnds])* x:inner[lvl][id] {[folder (List.map (fun elem -> pre @ elem) o); x]})
                in
                let res = ostap (f:fstPart m:midPart {f @ m}) in
                let lvl = lastPart oplev in
                match lastPart oparts with
                | `Simpl opart ->
                  ostap (-r:res x:inner[lvl][if lvl = l then (fun y -> opsema (r @ [y])) else id] -opart {x})
                | `Compl (opart, folder, ls) ->
                  let opnds  = (List.map (fun l' -> inner l' id) ls) in
                  let lvl' = lastPart ls in
                  ostap (-r:res inner[lvl][if lvl = l then (fun y -> opsema (r @ [y])) else id]
                        | r:res x:inner[lvl][id] o:(constrOpart[opart][opnds])* post:constrOpart[fstParts opart][fstParts opnds] y:inner[lvl'][if lvl' = l then (fun y -> opsema (r @ [x; folder (List.map (fun elem -> elem @ post @ [y]) o)])) else id] lastPart[opart] {y})
              else
              if (assoc l = Righta) then
                let res = applyOp (fstParts oparts) (generateOpnds (fstParts oparts) (fstParts oplev)) in
                let lvl = lastPart oplev in
                match lastPart oparts with
                | `Simpl opart ->
                  ostap (-r:res x:inner[lvl][if lvl = l then (fun y -> opsema (r @ [y])) else id] -opart {x})
                | `Compl (opart, folder, ls) ->
                  let opnds  = (List.map (fun l' -> inner l' id) ls) in
                  let lvl' = lastPart ls in
                  ostap (-r:res inner[lvl][if lvl = l then (fun y -> opsema (r @ [y])) else id]
                        | r:res x:inner[lvl][id] o:(constrOpart[opart][opnds])* post:constrOpart[fstParts opart][fstParts opnds] y:inner[lvl'][if lvl' = l then (fun y -> c (opsema (r @ [x; folder (List.map (fun elem -> elem @ post @ [y]) o)]))) else id] lastPart[opart] {y})
              else
                let opnds = generateOpnds oparts oplev in
                let res   = applyOp oparts opnds in
                ostap (x:res {c (opsema x)})) :: acc)
            [ostap (x:inner[l+1][id] {c x})]
            (snd ops.(l)))))
  in
  ostap (inner[0][id])
let ariphmetics = [| (Lefta, [([`Simpl (ostap ("")); `Simpl (ostap ("+")); `Simpl (ostap (""))], ((fun [x; y] -> x + y), [0; 0]));
                              ([`Simpl (ostap ("")); `Simpl (ostap ("-")); `Simpl (ostap (""))], ((fun [x; y] -> x - y), [0; 0]));
                              ([`Simpl (ostap ("match")); `Simpl (ostap ("with")); `Compl ([ostap ("l"); ostap ("o"); ostap ("l")], (fun l -> List.hd (List.hd l)), [0; 0])], ((fun [x; y; z] -> x - y), [0; 0]))])
                  |] *)
(*
let mixfixExpr f ops opnd =
  let fstParts l = List.rev @@ List.tl @@ List.rev l in
  let lastPart l = List.hd (List.rev l) in
  let midParts l = List.tl (fstParts l) in
  let isInterm l = fst (fst ops.(l)) in
  let assoc l = snd (fst ops.(l)) in
  let n = Array.length ops in
  let id x = x in
  let ostap (
      inner[l][c] : f [
    if n <= l then
      ostap (x:opnd {c x})
    else
      Ostap.Combinators.altl
        (List.fold_left
           (fun acc elem -> (
              let passSema lvl = lvl = l || isInterm lvl in
              let oparts = List.map (fun opart -> ostap(- $(opart))) (fst elem) in
              let opsema = fst (snd elem) in
              let levels = snd (snd elem) in
              let backbone oparts levels =
                (List.fold_right2
                  (fun opart level acc ->
                     match level with
                     | `Simple l         -> ostap (-opart x:inner[l][id]   a:acc {x :: a})
                     | `Many (l, folder) -> ostap (-opart x:(inner[l][id])* a:acc {(folder x) :: a})
                     | `Opt (l, default) -> ostap (-opart x:(inner[l][id])? a:acc {(match x with Some x -> x | None -> default) :: a})
                  )
                  (fstParts oparts)
                   levels
                  (ostap (lastPart[oparts] {[]}))
                )
              in
              if (assoc l = Left) then
                let midPart = if ((List.length oparts) = 2) then ostap ("" {[]})
                              else backbone (midParts oparts) (midParts levels)
                in
                let fstPart =
                  let opart = List.hd oparts in
                  match List.hd levels with
                  | `Simple lvl ->
                    ostap (opart o:inner[lvl][id] {c o})
                  | `Many (lvl, folder) ->
                    ostap (opart pre:inner[lvl][c] o:(inner[lvl][id])+ {folder (pre :: o)}
                          | opart {folder []})
                  | `Opt (lvl, default) ->
                    ostap (opart o:(inner[lvl][c])? {match o with Some x -> x | None -> default})
                in
                let res = ostap (f:fstPart m:midPart {f :: m}) in
                let opart = lastPart oparts in
                match lastPart levels with
                | `Simple lvl ->
                  ostap (r:res o:inner[lvl][if passSema lvl then (fun y -> opsema (r @ [y])) else id] opart {o})
                | `Many (lvl, folder) ->
                  ostap (r:res o:(inner[lvl][id])* post:inner[lvl][if passSema lvl then (fun y -> opsema (r @ [folder (o @ [y])])) else id] opart {post}
                        | r:res -opart {opsema (r @ [folder []])})
                | `Opt (lvl, default) ->
                  ostap (r:res o:(inner[lvl][if passSema lvl then (fun y -> opsema (r @ [y])) else id])? opart {match o with Some x -> x | None -> opsema (r @ [default])})
              else
              if (assoc l = Right) then
                let res = backbone (fstParts oparts) (fstParts levels) in
                let opart = lastPart oparts in
                match lastPart levels with
                | `Simple lvl ->
                  ostap (r:res o:inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [y]))) else id] opart {o})
                | `Many (lvl, folder) ->
                  ostap (r:res o:(inner[lvl][id])* post:inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [folder (o @ [y])]))) else id] opart {post}
                        | r:res -opart {c (opsema (r @ [folder []]))})
                | `Opt (lvl, default) ->
                  ostap (r:res o:(inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [y]))) else id])? opart {match o with Some x -> x | None -> c (opsema (r @ [default]))})
              else
                let res = backbone oparts levels in
                ostap (x:res {c (opsema x)})) :: acc)
           (if isInterm l then []
            else
              let i = ref l in
              while ((!i+1 < n) && (isInterm (!i+1))) do i := !i+1 done;
              [ostap (x:inner[!i+1][id] {c x})]
           )
           (snd ops.(l))
        )]) in
  ostap (inner[0][id])

let isInterm = true

let lamdba = [| ((not isInterm, Right), [(["^"; "."; ""], ((fun [x; y] -> `Lamdba (x, y)),  [`Simple 4; `Simple 0]))]);
                ((not isInterm, Left),  [([""; " "; ""],  ((fun [x; y] -> `Compose (x, y)), [`Simple 2; `Simple 1]))]);
                ((not isInterm, None),  [(["("; ")"],     ((fun [x] -> `Parenth x),         [`Simple 0]))]);
                ((isInterm, None),      [([""; ""; ""],   ((fun _ -> `Var ""),              [`Opt (3, `Var ""); `Many (4, fun _ -> `Var "")]))])
             |]
*)

let mixfixExpr f ops opnd =
  let fstParts l = List.rev @@ List.tl @@ List.rev l in
  let lastPart l = List.hd (List.rev l) in
  let midParts l = List.tl (fstParts l) in
  let assoc    l =fst ops.(l) in
  let n = Array.length ops in
  let id x = x in
  let ostap (
      inner[l][c] : f [
    if n <= l then
      ostap (x:opnd {c x})
    else
      Ostap.Combinators.altl
        (List.fold_left
           (fun acc elem -> (
              let passSema lvl = lvl = l in
              let ostapExpr = fst elem in
              let opsema    = fst (snd elem) in
              let levels    = snd (snd elem) in
              let backbone oparts levels =
                (List.fold_right
                  (fun opart level acc ->
                     match level with
                     | `Simple l         -> ostap (-opart x:inner[l][id]   a:acc {x :: a})
                     | `Many (l, folder) -> ostap (-opart x:(inner[l][id])* a:acc {(folder x) :: a})
                     | `Opt (l, default) -> ostap (-opart x:(inner[l][id])? a:acc {(match x with Some x -> x | None -> default) :: a})
                  )
                   levels
                  (ostap (lastPart[oparts] {[]}))
                )
              in
              (* if (assoc l = Left) then
                let midPart = if ((List.length oparts) = 2) then ostap ("" {[]})
                              else backbone (midParts oparts) (midParts levels)
                in
                let fstPart =
                  let opart = List.hd oparts in
                  match List.hd levels with
                  | `Simple lvl ->
                    ostap (opart o:inner[lvl][id] {c o})
                  | `Many (lvl, folder) ->
                    ostap (opart pre:inner[lvl][c] o:(inner[lvl][id])+ {folder (pre :: o)}
                          | opart {folder []})
                  | `Opt (lvl, default) ->
                    ostap (opart o:(inner[lvl][c])? {match o with Some x -> x | None -> default})
                in
                let res = ostap (f:fstPart m:midPart {f :: m}) in
                let opart = lastPart oparts in
                match lastPart levels with
                | `Simple lvl ->
                  ostap (r:res o:inner[lvl][if passSema lvl then (fun y -> opsema (r @ [y])) else id] opart {o})
                | `Many (lvl, folder) ->
                  ostap (r:res o:(inner[lvl][id])* post:inner[lvl][if passSema lvl then (fun y -> opsema (r @ [folder (o @ [y])])) else id] opart {post}
                        | r:res -opart {opsema (r @ [folder []])})
                | `Opt (lvl, default) ->
                  ostap (r:res o:(inner[lvl][if passSema lvl then (fun y -> opsema (r @ [y])) else id])? opart {match o with Some x -> x | None -> opsema (r @ [default])})
              else
              if (assoc l = Right) then
                let res = backbone (fstParts oparts) (fstParts levels) in
                let opart = lastPart oparts in
                match lastPart levels with
                | `Simple lvl ->
                  ostap (r:res o:inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [y]))) else id] opart {o})
                | `Many (lvl, folder) ->
                  ostap (r:res o:(inner[lvl][id])* post:inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [folder (o @ [y])]))) else id] opart {post}
                        | r:res -opart {c (opsema (r @ [folder []]))})
                | `Opt (lvl, default) ->
                  ostap (r:res o:(inner[lvl][if passSema lvl then (fun y -> c (opsema (r @ [y]))) else id])? opart {match o with Some x -> x | None -> c (opsema (r @ [default]))})
              else *)
                let res = backbone oparts levels in
                ostap (x:res {c (opsema x)})) :: acc)
           (if isInterm l then []
            else
              let i = ref l in
              while ((!i+1 < n) && (isInterm (!i+1))) do i := !i+1 done;
              [ostap (x:inner[!i+1][id] {c x})]
           )
           (snd ops.(l))
        )]) in
  ostap (inner[0][id])

let lamdba = [| (Right, [(fun x y -> ostap ("^" x "." y), ((fun [x; y] -> `Lamdba (x, y)),  [4; 0]))]);
                (Left,  [(fun x y -> ostap (x y),         ((fun [x; y] -> `Compose (x, y)), [2; 1]))]);
                (None,  [(fun x   -> ostap ("(" x ")"),   ((fun [x] -> `Parenth x),         [0]))])
             |]
(*
let lamdba = [| ((not isInterm, Right), [([ostap ("^"); ostap ("."); ostap ("")], ((fun [x; y] -> `Lamdba (x, y)),  [`Simple 4; `Simple 0]))]);
                ((not isInterm, Left),  [([ostap (""); ostap (" "); ostap ("")],  ((fun [x; y] -> `Compose (x, y)), [`Simple 2; `Simple 1]))]);
                ((not isInterm, None),  [([ostap ("("); ostap (")")],             ((fun [x] -> `Parenth x), [`Simple 0]))]);
                ((isInterm, None),      [([ostap (""); ostap (""); ostap ("")],   ((fun _ -> `Var ""), [`Opt (3, `Var ""); `Many (4, fun _ -> `Var "")]))])
             |]
 *)
(*
let arithmetics = [| ((not isInterm, None), [([ostap ("if "); ostap (" then "); ostap (""); ostap (" else "); ostap ("")], ((fun [b; vthen; elifs; velse] -> if b = 1 then vthen else (if elifs != -1 then elifs else velse)), [`Simple 2; `Simple 2; `Many (1, fun l -> List.fold_right (fun v a -> if v != -1 then v else a) l (-1)); `Simple 2]))]);
                     ((isInterm, None),     [([ostap (" elif "); ostap (" then "); ostap ("")], ((fun [b; x] -> if b = 1 then x else -1), [`Simple 2; `Simple 2]))]);
                     ((not isInterm, Left), [([ostap (""); ostap ("+"); ostap ("")], ((fun [x; y] -> x + y), [`Simple 3; `Simple 2]));
                                             ([ostap (""); ostap ("-"); ostap ("")], ((fun [x; y] -> x - y), [`Simple 3; `Simple 2]))]);
                     ((not isInterm, Left), [([ostap (""); ostap ("*"); ostap ("")], ((fun [x; y] -> x * y), [`Simple 4; `Simple 3]));
                                             ([ostap (""); ostap ("/"); ostap ("")], ((fun [x; y] -> x / y), [`Simple 4; `Simple 3]))]);
                     ((not isInterm, None), [([ostap ("("); ostap (")")], ((fun [x] -> x), [`Simple 0]))]);
                     ((isInterm, None),     [([ostap ("("); ostap (")")], ((fun [x] -> x), [`Opt (0, 1)]))])
                  |] *)

(*
 ostap (
  opnd: IDENT {3};
  intExpr: mixfixExpr[arithmetics][opnd];
  arithm: intExpr -EOF
  ) *)
let id = fun x -> x

ostap (
    var: x:IDENT {`Var x};
    lambdaExpr: mixfixExpr[id][lamdba][var];
    lambda: lambdaExpr -EOF
  )
(*
let _ =
  match arithm (new lexer "a+a") (fun res s -> (*match res with
	                                                   | `I _ ->*) Parsed ((res, s), None)) with
  | Parsed ((res, _), _) -> Printf.printf "%d\n" res
  | Failed _ -> Printf.printf "Failed.\n"*)

let _ =
  let rec print res =
    match res with
    | `Lamdba (x, y) -> "{Lambda " ^ (print x) ^ " -> " ^ (print y) ^ "}"
    | `Compose (x, y) -> "{" ^ (print x) ^ "  " ^ (print y) ^ "}"
    | `Parenth x -> "{(" ^ (print x) ^ ")}"
    | `Var x     -> x
  in
  match Combinators.Mem.mapply lambda (new lexer "^x.^y.y x") (fun res s -> Parsed ((res, s), None)) with
  | Parsed ((res, _), _) -> Printf.printf "Parsed. %s\n" (print res)
  | Failed _ -> Printf.printf "Failed.\n"
