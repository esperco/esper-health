open Lwt
open Log

let try_finally f g =
  catch
    (fun () ->
       f () >>= fun x ->
       g () >>= fun () ->
       return x
    )
    (fun e ->
       g () >>= fun () ->
       raise e
    )

(* Must run the same process because the contents of /proc/self
   depend on the calling process. *)
let ls dirname =
  Lwt_unix.opendir dirname >>= fun dir ->
  let rec read acc =
    let maxlen = 1025 in
    Lwt_unix.readdir_n dir maxlen >>= fun a ->
    let acc = List.rev_append (Array.to_list a) acc in
    if Array.length a < maxlen then
      return (List.rev acc)
    else
      read acc
  in
  try_finally
    (fun () -> read [])
    (fun () -> Lwt_unix.closedir dir)

let get_fd_count () =
  let dirname = "/proc/self/fd" in
  if Sys.file_exists dirname (* Linux only *) then
    ls dirname >>= fun l ->
    (* Exclude . and ..; assume all other files are decimal numbers
       identifying file descriptors *)
    return (Some (List.length l - 2))
  else
    return None

let health_check instance_id =
  get_fd_count () >>= function
  | None -> return ()
  | Some fd_count ->
      Cloudwatch.send
        (instance_id ^ ".fd_count")
        (float fd_count)

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
let monitor instance_id =
  Cloudwatch.send_event (instance_id ^ ".start") >>= fun () ->
  repeat_every_minute (fun () -> health_check instance_id);
  return ()
