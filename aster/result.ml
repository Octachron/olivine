
module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module T = B.T
  module I = Inspect
end
open Aliases
open Item
open Utils


let modname _ctx = L.simple ["Vk__Subresult"]

let expr ctx (ok,errors) =
  tyvar ~par:[modname ctx] @@ Info.Subresult.composite_nominal ok errors

module M = B.Result.Map

let find name {B.results=m; _ } =
  try M.find name m with
  | Not_found ->
    Fmt.(pf stderr) "Either.find: not found %a\n%!"
      L.pp_var name;
    List.iter (fun (name,id) -> Fmt.(pf stderr) "%a:%d\n%!"
                  L.pp_var name id)
    @@ M.bindings m; Fmt.epr "@."; exit 2

let def name lbls = decltype name
    ~manifest:(polyvariant_type ~order:Eq @@ List.map nloc @@ List.map mkconstr lbls)


let side_type ~order x =
  polyvariant_type ~order @@ List.map nloc @@ List.map mkconstr x

let typ ok error =
  [%type:
    ([%t side_type ~order:Eq ok], [%t side_type ~order:Eq error])
      Stdlib.result
      Ctypes.typ
  ]

  let view m name lbls =
    let constrs =
      List.map (fun name -> name, T.Abs (find name m)) lbls in
    module' name @@ structure @@
    Enum.( of_int (side_type ~order:Greater lbls) (Poly,name) constrs
           ^:: to_int (side_type ~order:Lesser lbls) (Poly,name)
             constrs
           ^:: nil
         )

  let make m (name,ok,error) = match ok, error with
    | [], x | x, [] -> (view m name x) ^:: nil
    | _ ->
      let v, ok_name, error_name = let open Info.Subresult in
        (composite_nominal ok error), side_name ok, side_name error in
      let conv name = open' name [%expr of_int, to_int] in
      item
        [%stri let [%p pat var v] =
                 Vk__result.view ~ok:[%e conv ok_name]
                   ~error:[%e conv error_name]
        ]
        (val' v @@ typ ok error)
      ^:: nil
