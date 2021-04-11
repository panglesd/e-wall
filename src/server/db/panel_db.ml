open Irmin.Type
open Lwt.Infix
open !Lwt.Syntax

module Content = struct
  (* type t = {
   *     id : int ;
   *     name : string ;
   *     filename: string;
   *   } [@@deriving irmin] *)
  type t = Model.Panel.t

  (* let t = derived by the irmin ppx *) 
  (* let t = Model.Panel.t *)
  let t = record "t" (fun id name filename -> ({ id ; name ; filename} : t))
          |+ field "id" string (fun (t:t) -> t.id)
          |+ field "name" string (fun (t:t) -> t.name)
          |+ field "filename" string (fun (t:t) -> t.filename)
          |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end
            
module Store = Irmin_unix.Git.FS.KV (Content)

let config = Irmin_git.config ~bare:true ".ewall/db/panel_db"
                                
let get_master () =
  (* Open the repo *)
  Store.Repo.v config
  >>= (* Load the master branch *)
  Store.master

let info message = Irmin_unix.info ~author:"Example" "%s" message
                 
let add ~(panel:(Content.t)) =
  get_master () >>= fun t ->
  Store.set_exn t ~info:(info "My first commit") [panel.id] panel

let get ~id =
  get_master () >>= fun t ->
  Store.get t [id]

let get_all () =
  get_master () >>= fun t ->
  let+ tree = Store.list t [] in
  List.filter_map (fun (_key, tree) ->
      match Store.Tree.destruct tree with
        `Contents (contents, _) -> Some contents
      | `Node _ -> None
    ) tree

