open Irmin.Type
open Lwt.Infix
(* open Lwt.Syntax *)

module Content = struct
  (* type t = {
   *     id : int ;
   *     name : string ;
   *     filename: string;
   *   } [@@deriving irmin] *)
  type t = Model.Hold.t
         
  let t = record "t" (fun id panel position size name -> ({ id ; panel ; position ; size ; name}:t))
          |+ field "id" string (fun (t:t) -> t.id)
          |+ field "panel" Panel_db.Content.t (fun (t:t) -> t.panel)
          |+ field "position" (pair int int) (fun (t:t) -> t.position)
          |+ field "size" int (fun (t:t) -> t.size)
          |+ field "name" string (fun (t:t) -> t.name)
          |> sealr
  (* let t = derived by the irmin ppx *) 
  (* let t = Model.Hold.t *)

  let merge = Irmin.Merge.(option (idempotent t))
end

module ContentList = struct
  (* type t = {
   *     id : int ;
   *     name : string ;
   *     filename: string;
   *   } [@@deriving irmin] *)
  type t = Model.Hold.t list
         
  let t = list Content.t 
  (* let t = derived by the irmin ppx *) 
  (* let t = Model.Hold.t *)

  let merge = Irmin.Merge.(option (idempotent t))
end
               
module Store = Irmin_unix.Git.FS.KV (Content)
module StoreList = Irmin_unix.Git.FS.KV (ContentList)

let config = Irmin_git.config ~bare:true ".ewall/db/holds"
let config_list = Irmin_git.config ~bare:true ".ewall/db/holds_list"

let get_master () =
  (* Open the repo *)
  Store.Repo.v config
  >>= (* Load the master branch *)
    Store.master
  
let get_master_list () =
  (* Open the repo *)
  StoreList.Repo.v config_list
  >>= (* Load the master branch *)
    StoreList.master
  
let info message = Irmin_unix.info ~author:"Example" "%s" message
                 
let set ~(hold:(Content.t)) =
  get_master () >>= fun t ->
  Store.set_exn t ~info:(info "My first commit") [hold.id] hold

let get ~id =
  get_master () >>= fun t ->
  Store.get t [id]

let set_all l =
  (* Todo : vérifier que tout a bien marché pour committer et faire la suite *)
  List.iter (fun hold -> ignore @@ set ~hold) l;
  get_master_list () >>= fun t ->
  StoreList.set_exn t ~info:(info "My first commit") ["a"] l
  
  
  
let get_all () =
  get_master_list () >>= fun t ->
  StoreList.get t ["a"]
  
(* let get_all () =
 *   get_master () >>= fun t ->
 *   let+ tree = Store.list t [] in
 *   List.filter_map (fun (_key, tree) ->
 *       match Store.Tree.destruct tree with
 *         `Contents (contents, _) -> Some contents
 *       | `Node _ -> None
 *     ) tree *)

