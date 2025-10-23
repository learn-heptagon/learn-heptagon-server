open Lwt
open Cohttp_lwt_unix

let get_req_assoc (json: Yojson.Safe.t) =
  match json with
  | `Assoc ass -> ass
  | _ -> invalid_arg "get_req_assoc"

let get_string key json =
  match List.assoc_opt key (get_req_assoc json) with
  | Some (`String s) -> s
  | _ -> invalid_arg (Printf.sprintf "get_string %s" key)

let get_prog = get_string "prog"
let get_filename = get_string "file_name"
let get_nodename = get_string "node_name"

let get_ins_outs key json =
  let msg = Printf.sprintf "get_ins_outs %s" key in
  match List.assoc_opt key (get_req_assoc json) with
  | Some (`List s) -> List.map (function `String s -> s | _ -> invalid_arg msg) s
  | _ -> invalid_arg msg

let get_ins = get_ins_outs "ins"
let get_outs = get_ins_outs "outs"

let read_all inch =
  let rec aux acc =
    try
     aux (input_line inch::acc)
    with _ -> List.rev acc
  in
  let res = String.concat "\n" (aux []) in
  res

let print_equiv_node fmt ndname ins outs =
  let open Format in
  let ins = List.mapi (fun x ty -> Format.sprintf "i_%d" x, ty) ins
  and outs1 = List.mapi (fun x ty -> Format.sprintf "o1_%d" x, ty) outs
  and outs2 = List.mapi (fun x ty -> Format.sprintf "o2_%d" x, ty) outs in
  let pp_sep fmt () = fprintf fmt ";" in
  let pp_vd fmt (x, ty) = fprintf fmt "%s: %s" x ty in
  fprintf fmt "node check_equiv(%a) returns (ok: bool)\n"
    (pp_print_list ~pp_sep pp_vd) ins;
  fprintf fmt "var %a;\n"
    (pp_print_list ~pp_sep pp_vd) (outs1@outs2);
  fprintf fmt "let\n";
  let pp_sep fmt () = fprintf fmt "," in
  let pp_var fmt (x, _) = fprintf fmt "%s" x in
  fprintf fmt "(%a) = %s(%a);\n"
    (pp_print_list ~pp_sep pp_var) outs1
    ndname
    (pp_print_list ~pp_sep pp_var) ins;
  fprintf fmt "(%a) = %s_corr(%a);\n"
    (pp_print_list ~pp_sep pp_var) outs2
    ndname
    (pp_print_list ~pp_sep pp_var) ins;
  let pp_sep fmt () = fprintf fmt " and " in
  let pp_eq fmt ((x, _), (y, _)) = fprintf fmt "(%s = %s)" x y in
  fprintf fmt "ok = %a;\n"
    (pp_print_list ~pp_sep pp_eq) (List.combine outs1 outs2);
  fprintf fmt "--%%MAIN;\n";
  fprintf fmt "--%%PROPERTY ok;\n";
  fprintf fmt "tel\n"

let client_folder = "../learn-heptagon/"

let json_headers = Http.Header.of_list [("Content-Type", "application/json")]

let users_folder = "./users/"

let () =
  if not (Sys.file_exists users_folder) then Unix.mkdir users_folder 0o755

let random_token () =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let n = String.length chars in
  String.init 16 (fun _ -> chars.[Random.int n])

let user_file token =
  Printf.sprintf "%s/%s/user.json" users_folder token

exception User_already_exists

let rec create_unique_token username =
  (* Check if username does not already exist *)
  let files = Sys.readdir users_folder in
  if Array.exists (fun token ->
         let f = user_file token in
         if Sys.file_exists f then
           let ic = open_in f in
           let json = Yojson.Safe.from_string (read_all ic) in
           close_in ic;
           match json with
           | `Assoc ass -> (match List.assoc_opt "username" ass with
                             | Some (`String s) -> s = username
                             | _ -> false)
           | _ -> false
         else false
       ) files
  then raise User_already_exists;
  (* Create token and folder *)
  let token = random_token () in
  let user_dir = Filename.concat users_folder token in
  try
    Unix.mkdir user_dir 0o755;
    let resp = `Assoc [("token", `String token); ("username", `String username)] |> Yojson.Safe.to_string in
    let oc = open_out (user_file token) in
    output_string oc resp;
    close_out oc;
    resp
  with Unix.Unix_error (Unix.EEXIST, _, _) -> create_unique_token username

let rec drop_leading_slashes (s: char Seq.t) =
  match s () with
  | Cons ('/', s) -> drop_leading_slashes s
  | _ -> s

let server =
  let callback _conn req body =
    try
      let uri = req |> Request.uri in
      let path = uri |> Uri.path |> String.to_seq |> drop_leading_slashes |> String.of_seq in
      match path with
      | "verify" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let body = Yojson.Safe.from_string body in
           try
             let p = get_prog body in
             let (inch, outch) = Unix.open_process "kind2 --modular true -json" in
             output_string outch p;
             close_out outch;
             let response = read_all inch in
             close_in inch;
             Server.respond_string
               ~status:`OK
               ~headers:json_headers
               ~body:response ()
           with Invalid_argument _ ->
             Server.respond_error ~status:`Unsupported_media_type ~body:"" ()
        )

      | "autocorrect" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let body = Yojson.Safe.from_string body in
           try
             let filename = get_filename body
             and prog = get_prog body
             and ndname = get_nodename body
             and ins = get_ins body and outs = get_outs body in
             (* Get correction from file *)
             let ic = open_in (Printf.sprintf "corrections/%s.lus" filename) in
             let cprog = read_all ic in
             close_in ic;
             (* Build the checked program *)
             print_equiv_node Format.str_formatter ndname ins outs;
             let p = Printf.sprintf "%s\n%s\n%s"
                 prog cprog
                 (Format.flush_str_formatter ())
             in
             (* Call Kind2 *)
             let (inch, outch) = Unix.open_process "kind2 -json" in
             output_string outch p;
             close_out outch;
             let response = read_all inch in
             close_in inch;
             (* Send response *)
             Server.respond_string
               ~status:`OK
               ~headers:json_headers
               ~body:response ()
           with
           | Invalid_argument _ -> Server.respond_error ~status:`Unsupported_media_type ~body:"" ()
           | Sys_error msg -> Server.respond_error ~status:`Not_found ~body:msg ()
        )

      | "create-user" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let json = Yojson.Safe.from_string body in
           try
             let username =
               match List.assoc_opt "username" (get_req_assoc json) with
               | Some (`String t) -> t
               | _ -> invalid_arg "create-user: missing username"
             in
             let resp = create_unique_token username in
             Server.respond_string ~status:`OK ~headers:json_headers ~body:resp ()
           with
           | User_already_exists -> Server.respond_error ~status:`Conflict ~body:"" ()
           | Invalid_argument msg -> Server.respond_error ~status:`Unsupported_media_type ~body:msg ())

      | "get-user" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let json = Yojson.Safe.from_string body in
           let token =
             match List.assoc_opt "token" (get_req_assoc json) with
             | Some (`String t) -> t
             | _ -> invalid_arg "get-user: missing token"
           in
           let user_dir = Filename.concat users_folder token in
           if Sys.file_exists user_dir && Sys.is_directory user_dir then
             let ic = open_in (user_file token) in
             let resp = read_all ic in
             close_in ic;
             Server.respond_string ~status:`OK ~headers:json_headers ~body:resp ()
           else
             Server.respond_error ~status:`Not_found ~body:"User not found." ()
        )

      | "save-notebook" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let json = Yojson.Safe.from_string body in
           let token =
             match List.assoc_opt "token" (get_req_assoc json) with
             | Some (`String t) -> t
             | _ -> invalid_arg "save-notebook: missing token"
           in
           let user_dir = Filename.concat users_folder token in
           if Sys.file_exists user_dir && Sys.is_directory user_dir then
             let notebook_json =
               match List.assoc_opt "notebook" (get_req_assoc json) with
               | Some n -> n
               | _ -> invalid_arg "save-notebook: missing notebook data"
             in
             let notebook_filename =
               match List.assoc_opt "filename" (get_req_assoc notebook_json) with
               | Some (`String t) -> t
               | _ -> "unfilenamed"
             in
             let filename = Filename.concat user_dir (notebook_filename ^ ".json") in
             let oc = open_out filename in
             Yojson.Safe.to_channel oc notebook_json;
             close_out oc;
             let resp = `Assoc [("status", `String "ok")] |> Yojson.Safe.to_string in
             Server.respond_string ~status:`OK ~headers:json_headers ~body:resp ()
           else
             Server.respond_error ~status:`Not_found ~body:"User not found." ()
        )

      | "get-notebook" ->
        Cohttp_lwt.Body.to_string body >>=
        (fun body ->
           let json = Yojson.Safe.from_string body in
           match List.assoc_opt "token" (get_req_assoc json),
                 List.assoc_opt "filename" (get_req_assoc json)
           with
           | Some (`String token), Some (`String filename) ->
             let user_dir = Filename.concat users_folder token in
             if Sys.file_exists user_dir && Sys.is_directory user_dir then
               let fname = Filename.concat user_dir (filename ^ ".json") in
               if Sys.file_exists fname then
                 let ic = open_in fname in
                 let notebook_json =
                   try Yojson.Safe.from_channel ic
                   with _ -> `Null
                 in
                 close_in ic;
                 Server.respond_string ~status:`OK ~headers:json_headers ~body:(Yojson.Safe.to_string notebook_json) ()
               else
                 Server.respond_error ~status:`No_content ~body:"Notebook not found." ()
             else
               Server.respond_error ~status:`Not_found ~body:"User not found." ()
           | _ -> Server.respond_error ~status:`Bad_request ~body:"Missing fields." ()
        )

      | "" -> Server.respond_file ~fname:(client_folder^"index.html") ()

      | s -> Server.respond_file ~fname:(client_folder^s) ()
    with e -> Server.respond_error ~status:`Internal_server_error ~body:(Printexc.to_string e) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
