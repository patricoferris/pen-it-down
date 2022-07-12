open Lwt.Syntax
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Server = Irmin_server.Make (Store)

let info () = Store.Info.v ~author:"Alice" ~message:"Hello" (Unix.gettimeofday () |> Int64.of_float)

let main =
  let uri = Uri.of_string "ws://localhost:9090/ws" in
  let config = Irmin_git.config "penit" in
  let* store = Store.Repo.v config in
  let* main = Store.main store in
  let* () = Store.set_exn ~info main [ "world" ] "# A markdown note!!" in
  let* server = Server.v ~uri config in
  let () = Format.printf "Listening on %a@." Uri.pp uri in
  Server.serve server

let () = Lwt_main.run main
