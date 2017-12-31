module Aliases = struct
  module T = Lib.T
  module H = Ast_helper
end open Aliases
open Utils
module M = Info.Common.StringMap
(*let (%) = M.find*)
  let rec expr: type a. (a, Info.Linguistic.name) T.math -> _ = function
    | T.Int n -> H.Exp.constant @@ H.Const.int n
    | T.Var v -> ex var v
    | T.Ceil T.Div(x,y) ->
      [%expr let x, y = [%e expr x], [%e expr y] in if x mod y = 0 then x / y else 1 + x/y ]
    | T.Floor x -> expr x
    | T.Div (x,y) -> [%expr [%e expr x] / [%e expr y] ]
