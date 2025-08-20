open Lwt
open Cohttp_lwt_unix

let get_req_assoc (json: Yojson.Safe.t) =
  match json with
  | `Assoc ass -> ass
  | _ -> invalid_arg "get_req_assoc"

let get_prog json =
  match List.assoc_opt "prog" (get_req_assoc json) with
  | Some (`String s) -> s
  | _ -> invalid_arg "get_prog"

(* let get_job_id json = *)
(*   match List.assoc_opt "id" (get_req_assoc json) with *)
(*   | Some (`String s) -> s *)
(*   | _ -> invalid_arg "get_job_id" *)

let read_result inch =
  let rec read_all acc =
    try
     read_all (input_line inch::acc)
    with _ -> List.rev acc
  in
  let res = String.concat "\n" (read_all []) in
  res

let client_folder = "../learn-heptagon/"

let json_headers = Http.Header.of_list [("Content-Type", "application/json")]

let users_folder = "./users/"

let () =
  if not (Sys.file_exists users_folder) then Unix.mkdir users_folder 0o755

let random_token () =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let n = String.length chars in
  String.init 16 (fun _ -> chars.[Random.int n])

let server =
  let callback _conn req body =
    let uri = req |> Request.uri in
    match Uri.path uri with
      | "/verify" ->
        Cohttp_lwt.Body.to_string body >>=
          (fun body ->
            let body = Yojson.Safe.from_string body in
            try
              let p = get_prog body in
              let (inch, outch) = Unix.open_process "kind2 -json" in
              output_string outch p;
              close_out outch;
              Server.respond_string
                ~status:`OK
                ~headers:json_headers
                ~body:(read_result inch) ()
            with Invalid_argument _ ->
              Server.respond_error ~status:`Unsupported_media_type ~body:"" ()
          )

      | "/create-user" ->
        let token = random_token () in
        let user_dir = Filename.concat users_folder token in
        Unix.mkdir user_dir 0o755;
        let resp = `Assoc [("token", `String token)] |> Yojson.Safe.to_string in
        Server.respond_string ~status:`OK ~body:resp ()

      | "/get-user" ->
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
              let resp = `Assoc [("token", `String token)] |> Yojson.Safe.to_string in
              Server.respond_string ~status:`OK ~body:resp ()
            else
              Server.respond_error ~status:`Not_found ~body:"User not found" ()
          )

      | "/save-notebook" ->
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
              let notebook_title =
                match List.assoc_opt "title" (get_req_assoc notebook_json) with
                  | Some (`String t) -> t
                  | _ -> "untitled"
              in
              let filename = Filename.concat user_dir (notebook_title ^ ".json") in
              let oc = open_out filename in
              Yojson.Safe.to_channel oc notebook_json;
              close_out oc;
              let resp = `Assoc [("status", `String "ok")] |> Yojson.Safe.to_string in
              Server.respond_string ~status:`OK ~body:resp ()
            else
              Server.respond_error ~status:`Not_found ~body:"User not found" ()
          )

      | "/get-notebook" ->
        Cohttp_lwt.Body.to_string body >>=
          (fun body ->
            let json = Yojson.Safe.from_string body in
            match List.assoc_opt "token" (get_req_assoc json),
                  List.assoc_opt "title" (get_req_assoc json)
            with
              | Some (`String token), Some (`String title) ->
                let user_dir = Filename.concat users_folder token in
                if Sys.file_exists user_dir && Sys.is_directory user_dir then
                  let fname = Filename.concat user_dir (title ^ ".json") in
                  if Sys.file_exists fname then
                    let ic = open_in fname in
                    let notebook_json =
                      try Yojson.Safe.from_channel ic
                      with _ -> `Null
                    in
                    close_in ic;
                    Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string notebook_json) ()
                  else
                    Server.respond_error ~status:`Not_found ~body:"Notebook not found" ()
                else
                  Server.respond_error ~status:`Not_found ~body:"User not found" ()
              | _ -> Server.respond_error ~status:`Bad_request ~body:"Missing fields" ()
          )

      | "/" -> Server.respond_file ~fname:(client_folder^"index.html") ()

      | s -> Server.respond_file ~fname:(client_folder^s) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
