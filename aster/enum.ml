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

let def_std tn name constrs =
  let constr c = let n = constr (Std,name) c in
    H.Type.constructor (nloc n) in
  variant tn @@ List.map constr constrs

let def_poly tn name constrs =
  let constr c = constr (Poly,name) c in
  polyvariant (nloc tn) @@ List.map (fun x -> nloc(constr x)) constrs

let def tn (impl,name) = match impl with
  | Std -> def_std tn name
  | Poly -> def_poly tn name

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
    [%stri let [%p pp] = fun ppf [%p x.p] -> Format.pp_print_string ppf [%e m]]
    (val' ~:(pre ^ "pp") [%type: Format.formatter -> [%t opn] -> unit])

let vn = "ctype"

let view =
  item [%stri let ctype =
                Ctypes.view ~write:to_int ~read:of_int Ctypes.int]
    (val' ~:vn [%type: t Ctypes.typ])

let view_result =
  item
    [%stri let ctype =
             Vk__result.view
               ~ok:(of_int,to_int)
               ~error:(of_int,to_int)
    ]
    (val' ~:vn [%type: t Ctypes.typ])

let pp_result =
  item [%stri let pp = Vk__result.pp raw_pp]
    (val' ~:"pp" [%type: Format.formatter -> t -> unit])

let somenone =
  item [%str
    let none = None
    let some x = Some x
   ]
  []

let view_opt =
  item
    [%stri let ctype_opt =
             let read x = if x = max_int then
                 none
               else some(of_int x) in
             let write: _ option -> _ = function None -> max_int
                                | Some x -> to_int x in
             Ctypes.view ~read ~write Ctypes.int
    ]
    (val' (L.simple [vn; "opt"]) [%type: t option Ctypes.typ])

let view_result_opt =
  item
    [%stri let ctype_opt =
             let read x = if x = max_int then none
               else if x < 0 then some(Error(of_int x))
               else some(Ok(of_int x))
             in
             let write: _ option -> _ = function
               | None -> max_int
               | Some (Error x| Ok x) -> to_int x in
             Ctypes.view ~read ~write Ctypes.int
    ]
    (val' (L.simple [vn; "opt"]) [%type: t option Ctypes.typ])


let extern_result =
    item [%stri type t = (core,core) result ]
      [%sigi: type t = (core,core) result ]

let make impl (name,constrs) =
  let is_result = Inspect.is_result_name name in
  let pre = if is_result then "raw_" else "" in
  let ty = impl, name in
  let opn = if is_result then [%type: [< core]] else [%type: t] in
  let cl = if is_result then [%type: [> core]] else [%type:t] in
  let tn = if is_result then "core" else "t" in
    somenone
    @* def tn ty constrs
    ^:: to_int opn ty constrs
    ^:: of_int cl ty constrs
    ^:: pp opn pre ty constrs
    ^:: (if is_result then
           extern_result ^:: pp_result ^:: view_result
           ^:: view_result_opt ^:: nil
         else view ^:: view_opt ^:: nil
        )
