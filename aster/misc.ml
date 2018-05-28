module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module H = Ast_helper
  module Exp = H.Exp
  module P = Parsetree
  module C = Common
end
open Aliases
open Item
open Utils


let packed m = Exp.pack H.Mod.(ident @@ nlid @@ modname m)

let builtin = "Builtin_types"
let builtin' = L.(~:builtin)

let view = L.(~:"view")
let alias {B.builtins;_} (name,origin) =
  if not @@ B.Name_set.mem name builtins then begin
    B.Name_set.iter (fun n ->
        Format.eprintf "Builtin type: %a@." L.full_pp n)
      builtins;
    Format.eprintf "Alias %a@." L.full_pp name;
    let sign =
      let constraint' =
        H.Type.mk ~manifest:(typ ~par:[~:builtin; origin] ~:"t")
          (nloc "x") in
      H.Sig.include_ @@ H.Incl.mk @@
      H.Mty.(with_ (ident  @@ nloc @@
                    Longident.( Ldot(Lident builtin,
                                     "alias") ) )
               [P.Pwith_typesubst constraint'] ) in
    let _t = typ ~par:[name] ~:"t" in
    let str = include' @@
      H.Mod.(apply
               (ident @@ nloc @@ qualify [builtin'] "Alias")
               (ident @@ nloc @@ modpath ~par:L.[~:builtin] origin)
            ) in
    item [str] [sign]
(*
        sign
    )
    ^:: C.extern_type name
    ^:: item
      [%stri let view = [%e ident @@ qn name "ctype"] ]
      (val' view [%type: [%t t] Ctypes.typ])
    ^:: item
      [%stri let view_opt =
               Builtin_types.integer_opt [%e packed name] ]
      (val' L.(view//"opt") [%type: [%t t] option Ctypes.typ])
    ^:: nil *)
  end
  else
    nil

let float_const f = Exp.constant (H.Const.float @@ string_of_float f)

module Const = struct
  let make (name,const) =
    let rec expr =
      function
      | B.Arith.Float f -> float_const f
      | Int n ->  int.e n
      | UInt64 n ->
        [%expr Unsigned.ULLong.of_string [%e string @@ Unsigned.ULLong.to_string n]]
      | UInt n ->
        [%expr Unsigned.UInt.of_string [%e string @@ Unsigned.UInt.to_string n] ]
      | Complement num_expr -> [%expr lnot [%e expr num_expr] ]
      | Minus (a,b) -> [%expr [%e expr a] - [%e expr b] ] in
    let rec ty = function
      | B.Arith.Float _ -> [%type: float]
      | Int _ -> [%type: int ]
      | UInt64 _ -> [%type: Unsigned.ullong ]
      | UInt _ -> [%type: Unsigned.uint ]
      | Complement n -> ty n
      | Minus(a,_) -> ty a in
    item
      [[%stri let [%p pat var name] = [%e expr const] ]]
      [val' name @@ ty const]
end
