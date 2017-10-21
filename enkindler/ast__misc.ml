module Aliases= struct
  module L = Name_study
  module B = Lib_builder
  module Ty = Lib_builder.Ty
  module H = Ast_helper
  module Exp = H.Exp
  module P = Parsetree
  module C = Ast__common
end
open Aliases
open Ast__item
open Ast__utils


let packed m = Exp.pack H.Mod.(ident @@ nlid @@ modname m)
let alias builtins (name,origin) =
  if not @@ B.Name_set.mem name builtins then
    let sign =
      let constraint' = H.Type.mk ~manifest:(typ ~par:origin ~:"t") (nloc "x") in
      H.Mty.(with_ (ident @@ nlid "alias") [P.Pwith_typesubst constraint'] ) in
    let t = typ ~par:name ~:"t" in
    ( module_gen name @@
      item
        H.Mod.(apply (ident (nlid "Alias"))
                 (ident @@ nlid @@ modname origin))
        sign
    )
    ^:: C.extern_type name
    ^:: item
      [%stri let [%p pat var name] = [%e ident @@ qn name "ctype"] ]
      (val' name [%type: [%t t] Ctypes.typ])
    ^:: item
      [%stri let [%p pat var L.(name//"opt")] = integer_opt [%e packed name] ]
      (val' L.(name//"opt") [%type: [%t t] option Ctypes.typ])
    ^:: nil
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
    [%stri let [%p pat var name] = [%e expr const] ]
end
