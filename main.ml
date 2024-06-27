open Printf

let rec string_of_formula (f : Lfs.formula) =
  match f with
  | Single prop -> Lfs.string_of_prop prop
  | And (f1, f2) ->
    sprintf "(%s & %S)" (string_of_formula f1) (string_of_formula f2)
  | Or (f1, f2) ->
    sprintf "(%s | %S)" (string_of_formula f1) (string_of_formula f2)
  | Not f1 -> sprintf "!%s" (string_of_formula f1)

let process () =
  print_string "\n> ";
  flush stdout;
  let cmd = input_line stdin in
  match cmd |> String.split_on_char ' ' with
  | "mkdir" :: dirs -> List.iter Lfs_path.mkdir_ dirs
  | "touch" :: files -> List.iter (fun name -> Lfs_path.mkfile_ name "") files
  | "rm" :: files -> List.iter (fun name -> Lfs.rm (Left name) |> ignore) files
  | "rmdir" :: dirs -> List.iter Lfs.rmdir dirs
  | [ "cd"; arg ] -> Lfs_path.cd_ arg
  | [ "ls" ] -> Lfs.ls_bis () |> List.iter print_endline
  | [ "mv"; file; dir ] -> Lfs_path.mv_ file dir
  | [ "pwd" ] -> Lfs.pwd () |> string_of_formula |> print_endline
  | [ "mvdir"; dir1; dir2 ] -> Lfs_path.mvdir_ dir1 dir2
  | [ "" ] -> ()
  | _ -> prerr_endline ("invalid command: " ^ cmd)

let () =
  while true do
    try process () with
    | End_of_file ->
      print_endline "Bye for now!";
      exit 0
    | exn -> printf "error: %s\n%!" (Printexc.to_string exn)
  done
