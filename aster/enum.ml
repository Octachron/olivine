module Aliases= struct
  module L = Info.Linguistic
  module B = Lib
  module Ty = B.Ty
  module T = B.T


  module H = Ast_helper
  module Exp = H.Exp
  module Pat = H.Pat
  module P = Parsetree
  module C = Common
end
open Aliases
open Item
open Utils

type implementation = Std | Poly

let constr (_,name_0) (c, _) =
  let name = L.remove_context name_0 c in
  let name = if name = L.mu then name_0 else name in
  Fmt.strf "%a" L.pp_constr name

let def_std name constrs =
  let constr c = let n = constr (Std,name) c in
    H.Type.constructor (nloc n) in
  variant "t" @@ List.map constr constrs

let def_poly name constrs =
  let constr c = constr (Poly,name) c in
  polyvariant (nloc "t") @@ List.map (fun x -> nloc (constr x)) constrs

let def (impl,name) = match impl with
  | Std -> def_std name
  | Poly -> def_poly name

let case pair ty (_, p as c) =
  match p with
  | T.Abs n ->  pair (constr ty c) n
  | _ -> assert false

let construct (impl,_) = match impl with
  | Std -> { p = (fun s -> Pat.construct (nlid s) None)
           ; e = (fun s -> Exp.construct (nlid s) None) }
  | Poly -> { p = (fun s -> Pat.variant s None);
              e = (fun s -> Exp.variant s None) }


let conv ?(cases=[]) f params constrs =
  Exp.function_ (List.map (case f params) constrs @ cases)

let to_int t ty constrs =
  let c = construct ty in
  let f =
    conv (fun n d -> Exp.case (c.p n) (int.e d)) ty constrs in
  item [%stri let to_int = [%e f] ]
    (val' ~:"to_int" [%type: [%t t] -> int])

let of_int t ty constrs =
  let c = construct ty in
  let app n d = H.Exp.case (int.p d) (c.e n) in
  let cases = [Exp.case (Pat.any ()) [%expr assert false]] in
  let f = conv ~cases app ty constrs in
  let sigi = val' ~:"of_int" [%type: int -> [%t t] ] in
  let stri = [%stri let of_int = [%e f] ] in
  item stri sigi

let unique, reset = C.id_maker ()
let pp opn pre ty constrs =
  let cstr = construct ty in
  let pp = Pat.var @@ nloc @@ pre ^ "pp" in
  let x = unique "x" in
  let m =
    let case c = let s = constr ty c in
      Exp.case (cstr.p s) (string s) in
    Exp.match_ x.e (List.map case constrs) in
  item
    [%stri let [%p pp] = fun ppf [%p x.p] -> Printer.pp_print_string ppf [%e m]]
    (val' ~:(pre ^ "pp") [%type: Printer.formatter -> [%t opn] -> unit])

let view =
  item [%stri let view = Ctypes.view ~write:to_int ~read:of_int int]
    (val' ~:"view" [%type: t Ctypes.typ])

let view_result =
  item
    [%stri let view = Vk__result.view  ~ok:(of_int,to_int) ~error:(of_int,to_int)]
    (val' ~:"view" [%type: (t,t) result Ctypes.typ])

let pp_result =
  item [%stri let pp = Vk__result.pp raw_pp]
    (val' ~:"pp" [%type: Printer.formatter -> (t,t) result -> unit])

let view_opt =
  item
    [%stri let view_opt =
             let read x = if x = max_int then
                 Option.None
               else Option.Some(of_int x) in
             let write = function Option.None -> max_int
                                | Option.Some x -> to_int x in
             Ctypes.view ~read ~write int
    ]
    (val' ~:"view_opt" [%type: t option Ctypes.typ])

let view_result_opt =
  item
    [%stri let view_opt =
             let read x = if x = max_int then Option.None
               else if x < 0 then Option.Some(Error(of_int x))
               else Option.Some(Ok(of_int x))
             in
             let write = function
               | Option.None -> max_int
               | Option.Some (Error x| Ok x) -> to_int x in
             Ctypes.view ~read ~write int
    ]
    (val' ~:"view_opt" [%type: (t,t) result option Ctypes.typ])


let extern_type name =
  if Inspect.is_result_name name then
    item [%str type nonrec result = (Result.t,Result.t) result ]
      [%sig: type nonrec result = (Result.t,Result.t) result ]
  else
    C.extern_type name ^:: nil

let make impl (name,constrs) =
  let is_result = Inspect.is_result_name name in
  let f = if is_result then (fun t -> [%type: ([%t t],[%t t]) result])
    else (fun t -> t) in
  let pre = if is_result then "raw_" else "" in
  let ty = impl, name in
  let opn = if is_result then [%type: [< t]] else [%type: t] in
  let cl = if is_result then [%type: [> t]] else [%type:t] in
  let str =
    def ty constrs
    ^:: to_int opn ty constrs
    ^:: of_int cl ty constrs
    ^:: pp opn pre ty constrs
    ^:: (if is_result then pp_result ^:: view_result ^:: view_result_opt ^:: nil
         else view ^:: view_opt ^:: nil ) in
  let m = module' name @@ structure str in
  m ^:: C.views ~f name @* extern_type name
