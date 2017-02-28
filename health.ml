open Lwt
open Log

let ( >>=! ) = Lwt.bind

let health_check prefix =
  let pid = Unix.getpid () in
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
    ignore (Lwt_unix.sleep 60. >>=! loop);
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

   cloudwatch_prefix is the prefix used for the Cloudwatch metrics and
   should follow its syntax constraints. Something like
     "wolverine.api.proc.ip-10-165-33-20.1"
   is valid.
*)
let monitor cloudwatch_prefix =
  Cloudwatch.send_event (cloudwatch_prefix ^ ".start")
  >>= fun () ->
  repeat_every_minute (fun () -> health_check cloudwatch_prefix);
  return ()
