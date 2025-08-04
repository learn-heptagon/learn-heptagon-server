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

(* (\** Table of jobs in progress *\) *)
(* let jobs = Hashtbl.create 100 *)

(* (\** Add a new job *\) *)
(* let register_job (conn: Conduit_lwt_unix.flow) inch = *)
(*   let sflow = Sexplib0.Sexp.to_string (Conduit_lwt_unix.sexp_of_flow conn) in *)
(*   let stime = string_of_float (Unix.time ()) in *)
(*   let _id = Digest.string (Printf.sprintf "%s-%s" sflow stime) in *)
(*   let id = "salut" in *)
(*   Hashtbl.add jobs id inch; *)
(*   id *)

(* let get_job_result (id: string) = *)
(*   let inch = Hashtbl.find jobs id in *)
(*   read_result inch *)

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
             (* let jobid = register_job conn inch in *)
             (* let res = `Assoc [("id", `String jobid)] in *)
             Server.respond_string ~status:`OK ~body:(read_result inch) ()
           with Invalid_argument _ ->
             Server.respond_error ~status:`Unsupported_media_type ~body:"" ()
         )
    (* | "/results" -> *)
    (*    Cohttp_lwt.Body.to_string body >>= *)
    (*      (fun body -> *)
    (*        let body = Yojson.Safe.from_string body in *)
    (*        try *)
    (*          let id = get_job_id body in *)
    (*          let res = get_job_result id in *)
    (*          Server.respond_string ~status:`OK ~body:res () *)
    (*        with *)
    (*        | Invalid_argument _ -> Server.respond_error ~status:`Unsupported_media_type ~body:"" () *)
    (*        | Not_found -> Server.respond_error ~status:`Not_found ~body:"" () *)
    (*        | Sys_error s -> Server.respond_error ~status:`Service_unavailable ~body:s () *)
    (*      ) *)
    | "/" -> Server.respond_file ~fname:"tests/index.html" ()
    | s -> Server.respond_file ~fname:("tests"^s) ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
