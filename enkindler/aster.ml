module L = Name_study
module Ty = Lib_builder.Ty

let list merge map =
  List.fold_left (fun acc x -> merge acc @@ map x) []

let listr merge map l last =
  List.fold_right (fun x acc -> merge (map x) acc) l last

let nloc = Location.mknoloc

let typename n = let open Longident in
  Lident (Fmt.strf "%a" L.pp_type n)


let varname n = let open Longident in
  Lident (Fmt.strf "%a" L.pp_var n)

let typ n = [%type: [%t typename n]]

let pty n = Ast_helper.Pat.var @@ nloc @@ Fmt.strf "%a" L.pp_type n

let typexp n = [%expr [%e typename n]]
let tyvar n = Ast_helper.Exp.ident @@ nloc @@ typename n
let var n = Ast_helper.Exp.ident @@ nloc @@ varname n

let int d = Ast_helper.(Exp.constant @@ Const.int d)

module Result = struct

  let expr (ok,errors) =
    tyvar @@ Subresult.composite_nominal ok errors
end

module Typexp = struct
    let rec make degraded x =
      let make = make degraded in
      match x with
      | Ty.Const t -> make t
      | Name n -> tyvar n
      | Ptr Name n | Ptr Const Name n ->
        [%expr ptr [%e tyvar n]]
      | Ptr ty -> [%expr ptr [%e make ty] ]
      | Option Name n -> [%expr ptr [%e tyvar L.(n//"opt")]]
      | Option (Ptr typ) -> [%expr ptr_opt [%e make typ] ]
      | Option Array (_,t) -> [%expr ptr_opt [%e make t]]
      | Option String -> [%expr string_opt]
      | Option t -> Fmt.epr "Not implemented: option %a@." Ty.pp t; exit 2
      | String -> [%expr string]
      | Array (Some Const n ,typ) -> [%expr array [%e tyvar n] [%e make typ]]
      | Array (Some (Lit n) ,typ) when not degraded ->
        [%expr array [%e int n] [%e make typ]]
      | Array (_,typ) -> make (Ty.Ptr typ)
      | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
      | Handle _  ->
        failwith "Anonymous type"
      | Result {ok;bad} ->
        Result.expr (ok,bad)
      | Record_extensions _ -> [%expr ptr void]
      (* ^FIXME^?: better typing? *)
      | FunPtr _ ->
        failwith "Not_implemented: funptr"


end


module Funptr = struct

  let mkty (fn:Ty.fn) =
    let ret  = Typexp.make true fn.return in
    listr (fun l r -> [%expr[%e l] @-> [%e r] ]) (Typexp.make true)
      (List.map snd @@ Ty.flatten_fn_fields fn.args)
      [%expr returning [%e ret]]

  let make (tyname, (fn:Ty.fn)) =
    let ty = pty tyname and tyo = pty L.(tyname//"opt")in
    match List.map snd @@ Ty.flatten_fn_fields fn.args with
    | [] -> [%stri let [%p ty] = ptr void ]
    | _ ->
      [%stri let [%p ty], [%p tyo] =
               let ty = [%e mkty fn] in
               Foreign.funptr ty, Foreign.funptr_opt ty
      ]

end
