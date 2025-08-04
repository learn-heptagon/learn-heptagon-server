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
             Server.respond_string ~status:`OK ~body:(read_result inch) ()
           with Invalid_argument _ ->
             Server.respond_error ~status:`Unsupported_media_type ~body:"" ()
         )
    | "/" -> Server.respond_file ~fname:"../learn-heptagon/index.html" ()
    | s -> Server.respond_file ~fname:("../learn-heptagon/"^s) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
