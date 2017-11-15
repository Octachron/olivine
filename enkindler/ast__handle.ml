open Ast__item
open Ast__utils
open Ast__common

let make ~dispatchable name =
    if dispatchable then
      make_genf name "Handle" ^:: extern_type name ^:: views name
    else
      make_genf ~suffix:"_non_dispatchable" name "Handle"
      ^:: extern_type name
      ^:: views name
