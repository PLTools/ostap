#use "topfind";;
#require "unix";;
let count = 1000

let get_all_tests () =
  let dir = Unix.opendir "." in
  let is_good_test s  =
    String.starts_with ~prefix:"test" s && String.ends_with ~suffix:".ml" s in
  let rec loop acc =
    match Unix.readdir dir with
    | exception End_of_file -> List.sort String.compare acc
    | "." | ".." -> loop acc
    | s when is_good_test s ->
      loop (String.sub s 0 (String.length s -3) :: acc)
    | _ -> loop acc
  in
  loop []

let is_skipped_test = function
  | "test016"
  | "test017"
  | "test020" -> true
  | _ -> false

let () =
  let files = get_all_tests () in
  let files  = List.filter (fun name ->
    let ans = is_skipped_test name in
    if ans then Printf.eprintf "Test %s is skipped.\n" name;
    not ans) files
  in
  List.iteri (Printf.printf "%d: %s\n") files;

  Out_channel.with_open_text "dune.tests" (fun dunech ->
      let dprintfn fmt = Format.kasprintf (Printf.fprintf dunech "%s\n") fmt in
      dprintfn "; This file was autogenerated via (cd camlp5/regression && ocaml gen.ml)\n";

      ListLabels.iter files ~f:(fun testname ->
        let cram_buf = Buffer.create 100 in
        let cram_printfn fmt =
          Format.kasprintf (Printf.bprintf cram_buf "%s\n") fmt
        in
        let (idx, suffix) = Scanf.sscanf testname "test%d%s" (fun a b -> (a,b)) in
        let cram_file = ref (Printf.sprintf "test%03d.t" idx) in
        (* let ocaml_file = ref (Printf.sprintf "test%03d%s.ml" idx suffix) in *)
        let exe_file = Printf.sprintf "test%03d%s.exe" idx suffix in

        cram_printfn "  $ ./test%03d%s.exe" idx suffix;
        dprintfn "(cram";
        dprintfn "  (applies_to test%03d)" idx;
        dprintfn "  (deps %s))%!" exe_file;
        Out_channel.with_open_text !cram_file (fun ch ->
          output_string ch (Buffer.contents cram_buf));
        dprintfn "
(executable
  (name %s)
  (modules %s)
  (libraries ostap)
  (preprocessor_deps %s)
  (preprocess
  (action
    (run %s %%{input-file}))))\n"
                    (Printf.sprintf "test%03d%s" idx suffix)
                    (Printf.sprintf "test%03d%s" idx suffix)
                    "%{project_root}/camlp5/pp5+GT+ostap.exe"
                    "%{project_root}/camlp5/pp5+GT+ostap.exe"
      );
  )

