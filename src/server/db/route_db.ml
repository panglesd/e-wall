open Irmin.Type
open Lwt.Infix
open !Lwt.Syntax

module Cotation_Content = struct
  open Model.Cotation
  let t_letter = enum "cotation_letter" [("A",A);("B",B);("C",C)]
  let t_signe = enum "cotation_sign" [("Plus", Plus)]
  let t_bloc = triple int t_letter (option t_signe)
  let t_voie = triple int t_letter (option t_signe)
  let t_arkose = enum "cotation_arkose" [("Jaune",(Jaune:cotation_arkose));("Vert",(Vert:cotation_arkose));("Bleu",(Bleu:cotation_arkose));("Rouge",(Rouge:cotation_arkose));("Noir",(Noir:cotation_arkose));("Violet",(Violet:cotation_arkose));]
  let t_mroc = enum "cotation_mroc" [("Jaune",(Jaune:cotation_mroc));("Vert",(Vert:cotation_mroc));("Bleu",(Bleu:cotation_mroc));("Rouge",(Rouge:cotation_mroc));("Noir",(Noir:cotation_mroc));("Violet",(Violet:cotation_mroc));]

  let t = variant "t" (fun bloc voie arkose mroc -> function Bloc c -> bloc c | Voie c -> voie c | Arkose c -> arkose c | Mroc c -> mroc c)
  |~ case1 "Bloc" t_bloc (fun x -> Bloc x)
  |~ case1 "Voie" t_voie (fun x -> Voie x)
  |~ case1 "Arkose" t_arkose (fun x -> Arkose x)
  |~ case1 "Mroc" t_mroc (fun x -> Mroc x)
  |> sealv


end
   
module Content = struct
  (* type t = {
   *     id : int ;
   *     name : string ;
   *     filename: string;
   *   } [@@deriving irmin] *)
  type t = Model.Route.t

  (* let t = derived by the irmin ppx *) 
  (* let t = Model.Route.t *)

  let feet_t = enum "t" [("All", Model.Route.Feet.All) ; ("Only", Model.Route.Feet.Only)]

  let t = record "t" (fun id name holds feet final_hold initial_hold cotation -> ({id ; name ; holds ; feet ; final_hold ; initial_hold ; cotation}:t))
          |+ field "id" string (fun (t:t) -> t.id)
          |+ field "name" string (fun (t:t) -> t.name)
          |+ field "holds" (list Hold_db.Content.t) (fun (t:t) -> t.holds)
          |+ field "feet" feet_t (fun (t:t) -> t.feet)
          |+ field "final_hold" (option Hold_db.Content.t) (fun (t:t) -> t.final_hold)
          |+ field "initial_hold" (option (list Hold_db.Content.t)) (fun (t:t) -> t.initial_hold)
          |+ field "cotation" (option Cotation_Content.t) (fun (t:t) -> t.cotation)
          |> sealr

  let merge = Irmin.Merge.(option (idempotent t))
end
            
module Store = Irmin_unix.Git.FS.KV (Content)

let config = Irmin_git.config ~bare:true ".ewall/db/route_db"
                                
let get_master () =
  (* Open the repo *)
  Store.Repo.v config
  >>= (* Load the master branch *)
  Store.master

let info message = Irmin_unix.info ~author:"Example" "%s" message
                 
let add ~(route:(Content.t)) =
  get_master () >>= fun t ->
  Store.set_exn t ~info:(info "My first commit") [route.id] route

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

let delete route =
  let open Model.Route in
  get_master () >>= fun t ->
  Store.remove ~info:(info "My first commit") t [route.id]
