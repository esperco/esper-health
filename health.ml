open Lwt
open Log

(* something like "proc.ip-10-136-83-8.scgi41001" *)
let instance_id = ref "unknown"

let health_check () =
  let pid = Unix.getpid () in
  let prefix = "wolverine.api." ^ !instance_id in
  (match Util_linux.get_resident_memory_size pid with
   | None -> return ()
   | Some x ->
       Cloudwatch.send (prefix ^ ".mem.rss") x
  ) >>= fun () ->
  Util_linux.get_fd_count pid >>= function
  | None -> return ()
  | Some fd_count ->
      Cloudwatch.send (prefix ^ ".fd_count") (float fd_count)

let repeat_every_minute f =
  let rec loop () =
    ignore (Lwt_unix.sleep 60. >>= loop);
    catch f
      (fun e ->
         let msg = string_of_exn e in
         logf `Error "Exception raised during health check: %s" msg;
         return ()
      )
  in
  ignore (loop ())

(*
   Produce health reports every minute.

   The instance ID is the prefix used for the Cloudwatch metrics and
   should follow its syntax constraints. Something like
     "wolverine.api.proc.ip-10-165-33-20.41001"
   is valid.
*)
let monitor () =
  Cloudwatch.send_event ("wolverine.api." ^ !instance_id ^ ".start")
  >>= fun () ->
  repeat_every_minute health_check;
  return ()
