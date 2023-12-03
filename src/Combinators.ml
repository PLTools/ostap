open Lazy
open Types_

module HashCons :
  sig

    type t

    val lookup_obj : 'a -> 'a
    val dump       : 'a -> string
    val clear      : unit -> unit

  end =
  struct
    open Obj

    exception Leave of int

    let dump_inner x =
      let buffer = Buffer.create 1024 in
      let append = Buffer.add_string buffer in
      let rec inner offset x =
        append offset;
        let offset = "  " ^ offset in
        if is_int x then (append "int: "; append (string_of_int @@ magic x); append "\n")
        else
          let t = tag x in
          if t <= last_non_constant_constructor_tag || t = closure_tag
          then (
            if t = closure_tag
            then append (Printf.sprintf "closure @ %x <\n" @@ magic x)
            else append (Printf.sprintf "tag %d @ %x <\n" t (magic x));
            for i = 0 to size x - 1 do
              append (Printf.sprintf "%dth field: " i);
              inner offset (field x i)
            done;
            append ">\n"
          )
          else if t = out_of_heap_tag then append (Printf.sprintf "out_of_heap: %x\n" @@ magic x)
          else append (Printf.sprintf "unsupported tag %d @ %x\n" t (magic x))
      in
      inner "" (repr x);
      Buffer.contents buffer

    let dump x = dump_inner (repr x)

    let rec compare_inner x y ancx ancy =
      if x == y || List.exists (fun (x', y') -> x == x' && y == y') (List.combine ancx ancy)
      then 0
      else (
        if is_int x
        then
          if is_int y
          then (Stdlib.Obj.magic x) - (Stdlib.Obj.magic y)
          else -1
        else
          if is_int y
          then 1
          else (
            let lx, ly = size x, size y in
            let tx, ty = tag x, tag y in
            if lx = ly
            then
              if tx = ty
              then (
                if tx = string_tag || tx = double_tag || tx = double_array_tag
                then compare x y
                else
                  if tx = object_tag
                  then compare (magic x) (magic y)
                  else
                    if tx <= last_non_constant_constructor_tag || tx = closure_tag
                    then
                      try
                        for i = 0 to lx - 1 do
                          let fx, fy = field x i, field y i in
                          (* let fxs, fys = List.init (size x) (field x), List.init (size y) (field y) in *)
                          let c =
                            if tx = closure_tag
                            then
                              if i = lx - 1
                              then (magic fx) - (magic fy)
                              else if tag fx = out_of_heap_tag
                                   then 0
                                   else compare_inner fx fy (x::ancx) (y::ancy)
                            else compare_inner fx fy (x::ancx) (y::ancy)
                          in
                          if c <> 0 then raise (Leave c)
                        done;
                        0
                      with Leave c -> c
                    else
                      if tx = out_of_heap_tag || tx = infix_tag || tx = forward_tag
                      then (Stdlib.Obj.magic x) - (Stdlib.Obj.magic y)
                      else
                        if tx = lazy_tag
                        then compare_inner (field x 0) (field y 0) (x::ancx) (y::ancy)
                        else
                          invalid_arg (Printf.sprintf "compare_obj: invalid tag %d\n" tx)
              )
              else tx - ty
            else lx - ly
          )
      )

    let compare_obj x y = compare_inner x y [] []

    module M = Map.Make (struct type t = Obj.t let compare = compare_obj end)

    type t = Obj.t M.t ref

    let m = ref M.empty

    let clear () = m := M.empty

    let rec lookup_inner v =
      (*
        Printf.printf "Looking up: %s\n" (dump v);
        Printf.printf "Binginds:\n";
        List.iter (fun (x, y) -> Printf.printf "  key: %s\n  value: %s\n" (dump x) (dump y)) (M.bindings !m);
        Printf.printf "\n";
      *)
      try M.find v !m with
        Not_found -> (
          (* Printf.printf "Not_found :\n %s\n===============\n\n" (dump v); *)
          let vr = repr v in
          if is_int vr
          then (m := M.add v v !m; v)
          else (
            let t = tag vr  in
            if t = string_tag || t = double_tag || t = double_array_tag
            then (m := M.add v v !m; v)
            else if t <= last_non_constant_constructor_tag || t = closure_tag
            then (
              let l   = size vr in
              let vr' = dup vr in
              for i = 0 to l - 1 do
                if i <> 0 || t <> closure_tag
                then set_field vr' i (lookup_inner (field vr' i))
              done;
              m := M.add v vr' !m;
              vr'
            )
            else
              if t = out_of_heap_tag || t = object_tag || t = infix_tag || t = lazy_tag  || t = forward_tag || t = lazy_tag
              then (m := M.add v v !m; v)
              else
                if t = lazy_tag
                then lookup_inner (field v 0)
                else
                  invalid_arg (Printf.sprintf "lookup_obj: invalid tag %d\n" t)
      ))

    let lookup_obj v = magic @@ lookup_inner (repr v)

  end

module Mem : sig
  type marrow
  val mapply : marrow -> 'a -> 'b
  val memoize : ('a -> 'b) -> marrow
  (* val wrap : 'a -> marrow
  val unwrap: marrow -> 'a *)
  end =
  struct
    type marrow = Obj.t
    let mapply : marrow -> 'a -> 'b = fun m a -> (Obj.magic m: _ -> _) a

    let memoize : ('a -> 'b) -> marrow =
      fun f -> Obj.magic (
        let m : ('a * 'b) list option ref = ref None in
        fun a ->
          let m' = match !m with None -> (* Printf.printf "Memotable created at %d\n" ((Obj.magic f) mod 10000);*)
                                         [] | Some x -> x in
          let b = List.find_opt (fun (a', _) -> (a == a')) m' in
          match b with
          | Some (_, b) -> (*Printf.printf "HIT Old entry at %d\n" ((Obj.magic f) mod 10000);*) b
          | None -> let b = f a in
                    (* Printf.printf "MISS New entry at %d\n" ((Obj.magic f) mod 10000); *)
                    m := Some ((a, b) :: m');
                    b)
    (* let wrap : 'a -> marrow =
      fun param -> Obj.magic param

    let unwrap: marrow -> 'a =
      fun m -> Obj.magic m *)
  end

let join = function
| None   -> fun y -> y
| Some x -> function None -> Some x | Some y -> Some (x#add y)

let (<@>) : ('stream, 'b, 'c) result -> ('stream, 'b, 'c) result -> ('stream, 'b, 'c) result =
  fun res1 res2 ->
    match res1, res2 with
    | Parsed ((res, x), opt1), Failed opt2             -> Parsed ((res, x), join opt1 opt2)
    | Failed opt1,             Parsed ((res, x), opt2) -> Parsed ((res, x), join opt1 opt2)
    | Parsed ((res, x), opt1), Parsed ((_, _), opt2)   -> Parsed ((res, x), join opt1 opt2)
    | Failed None,             Failed opt2             -> Failed (opt2)
    | Failed opt1,             Failed opt2             -> Failed (join opt1 opt2)
    | Empty, _ -> res2
    | _, Empty -> res1

let memo_k =
  fun k ->
    let table : ('a * 'stream, ('stream, 'b, 'c) result) Hashtbl.t = Hashtbl.create 16 in
    fun a s ->
      match (Hashtbl.fold
              (fun (a', s') r' acc ->
                 match acc with
                 | Some _ -> acc
                 | None when ((Stdlib.compare : int -> int -> int) (Obj.magic a) (Obj.magic a') == 0) && (s # equal s') -> Some r'
                 | _ -> None)
              table None) with
      | None ->
        let r = k a s in
        (match r with
        | Empty -> r
        | _ -> Hashtbl.add table (a, s) r; r)
      | Some r -> r

let comment str = function
| None   -> None
| Some m -> Some (m#comment str)

let return =
  fun x s k -> k x s

let cast =
  function Failed x -> Failed x | _ -> invalid_arg "Ostap.cast"

let map =
  fun f p s k ->
    p s (memo_k (fun a s -> k (f a) s))

let (-->) p f = map f p

let empty =
  fun s k -> return () s k

let fail =
 fun r _s _k -> Failed r

let lift =
  fun s k -> k s s

let sink =
  fun p s k ->
    match p s k with
    | Parsed ((s, _), f) -> Parsed ((s, s), f)
    | Failed x           -> Failed x
    | Empty -> Empty

let memoresult =
  fun p ->
    let ss : ('stream * 'a) list ref = ref      [] in
    let ks :                K.ks ref = ref K.empty in
    fun k ->
      if K.length !ks = 0
      then (
        ks := K.singleton k;
        p (memo_k (fun a s ->
            match List.find_all (fun (s', a') -> ((Stdlib.compare : int -> int -> int) (Obj.magic a) (Obj.magic a') == 0) && (s # equal s')) !ss with
             | [] -> (ss := (s, a) :: !ss;
                      K.fold (fun k acc -> acc <@> (k a s)) !ks (Empty))
             |  _ -> Empty
          )))
      else
        (ks := K.add k !ks;
         List.fold_left (fun acc x -> match acc, x with
                                      | Parsed (r, opt1), Failed opt2 -> Parsed (r, join opt1 opt2)
                                      | Failed opt1, Parsed (r, opt2) -> Parsed (r, join opt1 opt2)
                                      | Failed opt1, Failed opt2 -> Failed (join opt1 opt2)
                                      | Parsed _, _ -> acc
                                      | Empty, _              -> x
                                      | _, Empty              -> acc)
                        Empty
                        (List.map (fun (s, a) -> (k a s)) !ss))

let memo =
  fun f ->
    let table : ('stream, ('a, 'stream, 'b, 'c) parser') Hashtbl.t = Hashtbl.create 16 in
    fun s k ->
      match (Hashtbl.fold (fun s' p' acc -> match acc with
                                            | Some _                   -> acc
                                            | None when (s # equal s') -> Some p'
                                            | _                        -> None
                          ) table None) with
      | None -> let r = memoresult @@ (f s) in
                  Hashtbl.add table s r; r k
      | Some x -> x k

let alt =
  fun x y -> memo (fun s k -> let res1 = (x s k) in
                    match res1 with
                    | Parsed _ -> res1
                    | _ -> res1 <@> (y s k))
  (* fun x y -> memo (fun s k -> (x s k) <@> (y s k)) *)

let (<|>) = alt

let seq =
  fun x y s k -> x s (memo_k (fun a s' -> y a s' k))

let (|>) = seq

let opt =
  fun p ->
    memo (fun s k -> let s' = Oo.copy s in
                     let k' = memo_k (fun a s -> k (Some a) s) in
                     (p s k') <@> (k None s'))

let (<?>) = opt

let rec manyFold =
  fun f init p s k -> (empty |> fun _ -> return init) s k <@>
                      (p                 |> (fun xp  ->
                       manyFold f init p |> (fun xps ->
                       return (f xp xps)))) s k
(*
let rec many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p s k ->
    let result : ('stream, 'b, 'c) result ref = ref (k [] s) in
    let rec loop alist stream =
      p stream (memo_k (fun a stream' ->
                  let alist' = List.rev (a :: (List.rev alist)) in
                  let curResult = k (alist') stream' in
                  result := curResult <@> !result;
                  let tmp = loop (alist') stream' in
                  curResult))
    in
    let tmp = loop [] s in
    !result *)

let many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> memo (fun s k ->
    let rec loop alist stream result =
      result <@>
      p stream (memo_k (fun a stream' ->
                  let alist' = alist @ [a] in
                  let curResult = k (alist') stream' in
                  loop (alist') stream' (curResult <@> result)))
  in
  loop [] s (k [] (Oo.copy s)))
(*
let many : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p s k -> manyFold (fun xp xps -> xp :: xps) [] p s k
 *)
let (<*>) = many

let someFold =
  fun f init p -> p                 |> (fun xp  ->
                  manyFold f init p |> (fun xps ->
                  return (f xp xps)))

(* let some : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> p        |> (fun x  ->
          (many p)  |> (fun xs ->
          return (x :: xs)))
*)
let some : ('a, 'stream, 'b, 'c) parser -> ('a list, 'stream, 'b, 'c) parser =
  fun p -> memo (fun s k ->
    let rec loop alist stream =
      p stream (memo_k (fun a stream' ->
                  let alist' = alist @ [a] in
                  let curResult = k (alist') stream' in
                  curResult <@> loop (alist') stream'))
  in
  loop [] s)

let (<+>) = some

let guard =
  fun p f r s k ->
    p s (memo_k (fun a s -> if f a
                            then k a s
                            else Failed (match r with
                                         | None   -> None
                                         | Some r -> Some (r a))))

let unwrap r f g =
  match r with
  | Parsed ((x, _), _) -> f x
  | Failed x           -> g x

let altl =
  fun l -> List.fold_left (<|>) (fail None) l

let comment p str s k =
  match p s k with
  | (Parsed _ as x) -> x
  | Empty -> Empty
  | Failed m -> Failed (comment str m)

let fix =
  fun f -> let rec p = lazy (f (fun t -> force p t)) in force p

let fixPoly =
  fun l -> fix (fun self l -> Array.map (fun li x -> li (self l) x) l) l
