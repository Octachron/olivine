
module Aliases= struct
  module L = Name_study
  module B = Lib_builder
  module T = Lib_builder.T
end
open Aliases
open Ast__item
open Ast__utils

  let expr (ok,errors) =
    tyvar @@ Subresult.composite_nominal ok errors

  module M = B.Result.Map

  let find name m =
    try M.find name m with
    | Not_found ->
      Fmt.(pf stderr) "Either.find: not found %a\n%!"
        L.pp_var name;
      List.iter (fun (name,id) -> Fmt.(pf stderr) "%a:%d\n%!"
                    L.pp_var name id)
      @@ M.bindings m;
      raise Not_found

  let view m name constrs =
    let constrs =
      List.map (fun name -> name, T.Abs (find name m)) constrs in
    module' name @@
    Ast__enum.( of_int  (Poly,name) constrs ^::
           to_int [%type: t] (Poly,name) constrs ^:: nil
         )

  let make m (name,ok,error) = match ok, error with
    | [], x | x, [] -> (view m name x) ^:: nil
    | _ ->
      let v, ok_name, error_name = let open Subresult in
        (composite_nominal ok error), side_name ok, side_name error in
      let ty x = polyvariant_type ~closed:true @@ List.map typestr x in
      let conv name = open' name [%expr of_int, to_int] in
      item [%stri let [%p pat var v] =
               Vk__result.view ~ok:[%e conv ok_name] ~error:[%e conv error_name]
      ]
        (val' v
            [%type: ([%t ty ok], [%t ty error]) result ]
        ) ^:: nil
