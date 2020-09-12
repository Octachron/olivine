type integer = private I
type real = private R
type uint64 = Unsigned.ULLong.t
type uint = Unsigned.UInt.t

module Arith = struct

  type t =
    | Float of float
    | Int of int
    | UInt64 of uint64
    | UInt of uint
    | Complement of t
    | Minus of t * t

  let simplify x =
    let rec simplify = function
      | Complement (UInt n) -> true, UInt (Unsigned.UInt.lognot n)
      | Complement (UInt64 n) -> true, UInt64 (Unsigned.ULLong.lognot n)
      | Minus( Int n, Int n') -> true, Int (n - n')
      | Minus( UInt n, UInt n') -> true, UInt Unsigned.UInt.Infix.(n - n')
      | Minus( UInt64 n, UInt64 n') -> true, UInt64 Unsigned.ULLong.Infix.(n - n')
      | Minus( UInt n, Int n') ->
        let n' = Unsigned.UInt.of_int n' in
        true, UInt Unsigned.UInt.Infix.(n - n')
      | Minus( UInt64 n, Int n') ->
        let n' = Unsigned.ULLong.of_int n' in
        true, UInt64 Unsigned.ULLong.Infix.(n - n')
      | Minus( Float n, Float n') -> true, Float (n -. n')
      | Minus(a, b) ->
        let t, a' = simplify a in
        let t', b' = simplify b in
        let e = Minus(a',b') in
        if t || t' then simplify e else false, e
      | Complement n ->
        let m, s = simplify n in
        if m then simplify (Complement s) else false, Complement n
      | n -> false, n in
    snd @@ simplify x

  let rec pp ppf = function
    | Float f -> Fmt.float ppf f
    | Int n -> Fmt.int ppf n
    | UInt n -> Fmt.string ppf @@ Unsigned.UInt.to_string n
    | UInt64 n -> Fmt.string ppf @@ Unsigned.ULLong.to_string n
    | Complement n -> Fmt.pf ppf "~(%a)" pp n
    | Minus (a,b) -> Fmt.pf ppf "%a - %a" pp a pp b
end

module type name = sig
  type name
  val pp: Format.formatter -> name -> unit
end


type direction =
  | In
  | Out
  | In_Out

type pos = Abs of int | Bit of int | Offset of int

type ('a,'b) math =
  | Int: int -> (int,'b) math
  | Mult: ('a,'b) math * ('a,'b) math -> (int,'b) math
  | Var: 'b -> (int,'b) math
  | Div: ('a,'b) math * ('a,'b) math -> (int * int,'b) math
  | Ceil: (int * int,'b) math -> (int,'b) math
  | Floor: (int * int,'b) math -> (int,'b) math


let to_int: type a. (a,'b) math -> (int,'b) math = function
  | Var _ as v -> v
  | Div(_,_) as d -> Floor d
  | Ceil _ as c -> c
  | Int _ as n -> n
  | Floor _ as f -> f
  | Mult(_,_) as d -> d

let int_of_word s =
    try Int(int_of_string s) with
      Failure _ -> Var s

let rec vars: type a. 'b list -> (a,'b) math -> 'b list = fun l ->
  function
  | Var v -> v :: l
  | Int _ -> l
  | Floor x -> vars l x
  | Ceil x -> vars l x
  | Div (x,y) -> vars (vars l x) y
  | Mult (x,y) -> vars (vars l x) y

let rec math: Latex.item -> (int, 'name) math =
  let open Latex in function
    | Macro ("ceil", [Macro("over",[a;b])] ) ->
      Ceil (Div(math a, math b))
    | Macro("over", [a;b] ) -> Floor(Div(math a, math b))
    | Macro("mathit", [Word s]) -> Var ( s)
    | Macro("textrm", [Word x]) -> Var x
    | Macro("mathit", [Macro _ ]) ->
      Format.eprintf "Wrong argument: macro for mathit macro@."; exit 2
    | Macro("mathit", ([] | _ :: _ :: _) ) ->
      Format.eprintf "Wrong arity for mathit macro@."; exit 2
    | Macro(s, l) ->
      Format.eprintf "Unknown latex macro %s applied to: %a @." s Latex.pp l;
      exit 2
    | Word s -> int_of_word s
    | Group [x] -> math x
    | Group _ -> failwith "Not implemented group"

let rec rename: type a n name. ( n -> name) ->  (a,n) math -> (a,name) math  =
  fun namer -> function
  | Var x -> Var(namer x)
  | Int _ as n -> n
  | Div(x,y) -> Div(rename namer x, rename namer y)
  | Mult(x,y) -> Mult(rename namer x, rename namer y)
  | Ceil x -> Ceil (rename namer x)
  | Floor x -> Floor (rename namer x)

let math = function
  | [a] -> math @@ Latex.normalize a
  | _ -> failwith "Too much items in latex expression"

module Typexpr(X:name) = struct
  include X

  type constructor = name * pos

  type 'a constexpr =
    | Lit of 'a
    | Path of name list
    | Const of {factor:int; name:name}
    | Null_terminated
    | Math_expr of (int,name) math

  let pp_dir ppf x =
    Fmt.pf ppf "%s" (match x with
        | In -> "in"
        | Out -> "out"
        | In_Out -> "in|out"
      )


  type typedef =
    | Enum of  constructor list
    | Union of simple_field list
    | Bitset of { implementation:name; field_type:name option}
    | Bitfields of
        {
          fields: (name * int) list;
          values: (name * int) list
        }
    | Handle of { parent: name option; dispatchable:bool }
    | Record of {is_private:bool; fields: field list; extensions: name list }
    | Alias of typexpr

  and typexpr =
    | Const of typexpr
    | Name of name
    | Ptr of typexpr
    | Option of typexpr
    | String
    | Array of int constexpr option * typexpr
    | FunPtr of fn
    | Result of { ok: name list; bad: name list }
    | Width of { size:int; ty:typexpr }

  and simple_field = name * typexpr
  and field =
    | Simple of simple_field
    | Array_f of { index: simple_field; array: simple_field }
    | Record_extension of
        { exts: name list;
          tag:simple_field;
          ptr:simple_field
        }
  and fn_field = { dir:direction; field:field }
  and fn = { original_name:string; name:name; return: typexpr; args: fn_field list }

  type type_decl = name * typedef

  let field_to_type  = function
    | Array_f { array=_n, ty; _ } -> ty
    | Simple(_n,ty) -> ty
    | Record_extension _ -> failwith "Not implemented: typ for record_extension"

  let flatten_fields l =
    let f acc = function
      | Simple f -> f :: acc
      | Array_f {index; array } -> array :: index :: acc
      | Record_extension {tag;ptr; _ } -> ptr :: tag :: acc
    in
    List.rev @@ List.fold_left f [] l

  let flatten_fn_fields fs =
    let proj x = x.field in
    flatten_fields @@ List.map proj fs

  let is_simple fn =
    let is_simple_ty = function
      | Array _ | Result _ -> false
      | _ -> true in
    let is_simple = function
      | { dir = Out; _ } | { field = (Array_f _ | Record_extension _ ); _ } ->
        false
      | {field = Simple( _,  ty  ); _ } -> is_simple_ty ty in
    List.for_all is_simple fn.args && is_simple_ty fn.return

  let fp = Fmt.pf

  let pp_list sep = Fmt.list ~sep
  let const f ppf () = fp ppf f
  let dot ppf () = Fmt.pf ppf "."


  let pp_constexp pp ppf = function
    | Lit n -> fp ppf "%a" pp n
    | Null_terminated -> fp ppf "%s" "null-terminated"
    | Math_expr _ -> fp ppf "%s" "math-expr"
    | Const {factor; name} -> fp ppf "%d * %a" factor X.pp name
    | Path p -> fp ppf "[%a]" (Fmt.list ~sep:dot X.pp) p

  let pp_bitfield ppf (name,int) =
    fp ppf "%d:%a" int X.pp name

  let pp_name = X.pp

  let rec pp ppf = function
    | Const c -> fp ppf "const %a" pp c
    | Name n -> fp ppf "\"%a\"" X.pp n
    | Ptr (Name _ |Ptr _ as ty) -> fp ppf "ptr %a" pp ty
    | Ptr ty -> fp ppf "ptr (%a)" pp ty
    | Option (Name _ as ty) -> fp ppf "option %a" pp ty
    | Option (Ptr ty) -> fp ppf "option ptr (%a)" pp ty
    | Option ty -> fp ppf "option (%a)" pp ty
    | Array(None, ty) -> fp ppf "array (%a)" pp ty
    | Array(Some cexp, ty) -> fp ppf "array[%a](%a)"
                                (pp_constexp Fmt.int) cexp pp ty
    | FunPtr fn -> pp_fn ppf fn
    | String -> fp ppf "string"
    | Result {ok;bad} ->
      let ppl = pp_list (const "@ | ") X.pp in
      fp ppf "@[either@ [ok:%a]@ [bad:%a]@]"
        ppl ok ppl bad
    | Width { size; ty } ->
      Format.fprintf ppf "%a:%d" pp ty size


  and pp_simple_field ppf (name, t) =
    fp ppf "%a:%a" X.pp name pp t

  and pp_field ppf = function
    | Simple f -> pp_simple_field ppf f
    | Array_f {index;array} ->
      fp ppf "[{Array.index:%a; array:%a}@]"
        pp_simple_field index pp_simple_field array
    | Record_extension {exts; _ } ->
      fp ppf "[{Record extensions:%a}@]"
        (Fmt.list X.pp) exts

  and pp_fn_field ppf f =
    fp ppf "⟨%a:%a⟩" pp_dir f.dir pp_field f.field

  and pp_constr ppf (name,pos) =
    fp ppf "%a(%a)" X.pp name pp_pos pos

  and pp_pos ppf = function
    | Abs n -> fp ppf "%d" n
    | Offset n -> fp ppf "+%d" n
    | Bit n -> fp ppf "^%d" n

  and pp_fn ppf fn =
    fp ppf "fun %a@ (%a)@ ⇒ %a" X.pp fn.name
      (pp_list (const ",@ ") pp_fn_field) fn.args
      pp fn.return

  let pp_def ppf = function
    | Enum cs ->
      fp ppf "@[[%a]@]" (pp_list (const "@ | ") pp_constr) cs
    | Record {is_private; fields; extensions} ->
      let pp_extensions ppf =  function
        | [] -> ()
        | l -> Fmt.pf ppf "extended by [%a]" Fmt.(list ~sep:comma X.pp) l in
      fp ppf "@[%s{%a}%a@]" (if is_private then "private" else "")
        (pp_list (const ";@ ") pp_field) fields pp_extensions extensions
    | Union fields ->
      fp ppf "@[[%a]@]" (pp_list (const "@ ||") pp_simple_field) fields
    | Bitset {implementation; field_type} ->
      fp ppf "Bitset@[@ {implementation:%a;@ field_type:(%a)}@]"
        X.pp implementation Fmt.(option X.pp)  field_type
    | Bitfields {fields; values} ->
      let bitfields = pp_list (const ";@ ") pp_bitfield in

      fp ppf "Bitfields @ \
              @[{fields:@ @[[%a]@];@ \
              values:@ @[[%a]@] @ }@]"
        bitfields fields bitfields values
    | Handle h ->
      fp ppf "Handle@ @[{parent:%a; dispatchable:%b}@]"
        Fmt.(option X.pp) h.parent h.dispatchable
    | Alias typexp -> pp ppf typexp


  let pp_typedecl ppf (name,ty)=
    fp ppf "@[type %a=@ %a@]" X.pp name pp_def ty

end

module Simple_name = struct
  type name = string
  let pp = Fmt.string
end

module Ty = Typexpr(Simple_name)
