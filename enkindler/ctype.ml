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

module Typexpr(X:name) = struct
  include X

  type constructor = name * pos

  type 'a constexpr =
    | Lit of 'a
    | Var of name
    | Path of name list
    | Const of name
    | Null_terminated
    | Math_expr

  let pp_dir ppf x =
    Fmt.pf ppf "%s" (match x with
        | In -> "in"
        | Out -> "out"
        | In_Out -> "in|out"
      )

  type typexpr =
    | Const of typexpr
    | Name of name
    | Ptr of typexpr
    | Option of typexpr
    | String
    | Array of int constexpr option * typexpr
    | FunPtr of fn
    | Enum of  constructor list
    | Union of simple_field list
    | Bitset of { implementation:name; field_type:name option}
    | Bitfields of
        {
          fields: (name * int) list;
          values: (name * int) list
        }
    | Handle of { parent: name option; dispatchable:bool }
    | Result of { ok: name list; bad: name list }
    | Record of {is_private:bool; fields: field list}
    | Record_extensions of name list

  and simple_field = name * typexpr
  and field =
    | Simple of simple_field
    | Array_f of { index: simple_field; array: simple_field }
  and fn_field = { dir:direction; field:field }
  and fn = { name:name; return: typexpr; args: fn_field list }

  type type_decl = name * typexpr

  let flatten_fields l =
    let f acc = function
      | Simple f -> f :: acc
      | Array_f {index; array } -> array :: index :: acc in
    List.rev @@ List.fold_left f [] l

  let flatten_fn_fields fs =
    let proj x = x.field in
    flatten_fields @@ List.map proj fs

  let is_simple fn =
    let is_simple = function
      | { dir = Out; _ } | { field = Array_f _; _ } -> false
      | _ -> true in
    List.for_all is_simple fn.args

  let fp = Fmt.pf

  let pp_list sep = Fmt.list ~sep
  let const f ppf () = fp ppf f
  let dot ppf () = Fmt.pf ppf "."


  let pp_constexp pp ppf = function
    | Lit n -> fp ppf "%a" pp n
    | Var name -> fp ppf "%a" X.pp name
    | Null_terminated -> fp ppf "%s" "null-terminated"
    | Math_expr -> fp ppf "%s" "math-expr"
    | Const name -> fp ppf "%a" X.pp name
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
    | Option (Ptr _ as ty) -> fp ppf "ptr_opt %a" pp ty
    | Option ty -> fp ppf "option (%a)" pp ty
    | Array(None, ty) -> fp ppf "array (%a)" pp ty
    | Array(Some cexp, ty) -> fp ppf "array[%a](%a)"
                                (pp_constexp Fmt.int) cexp pp ty
    | FunPtr fn -> pp_fn ppf fn
    | Enum cs ->
      fp ppf "@[[%a]@]" (pp_list (const "@ | ") pp_constr) cs
    | Record {is_private; fields} ->
      fp ppf "@[%s{%a}@]" (if is_private then "private" else "")
        (pp_list (const ";@ ") pp_field) fields
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
    | String -> fp ppf "string"
    | Result {ok;bad} ->
      let ppl = pp_list (const "@ | ") X.pp in
      fp ppf "@[either@ [ok:%a]@ [bad:%a]@]"
        ppl ok ppl bad
    | Record_extensions exts ->
      let ppl = pp_list (const "@ | ") X.pp in
      fp ppf "@[record extensions [%a]@ " ppl exts


  and pp_simple_field ppf (name, t) =
    fp ppf "%a:%a" X.pp name pp t

  and pp_field ppf = function
    | Simple f -> pp_simple_field ppf f
    | Array_f {index;array} ->
      fp ppf "[{Array.index:%a; array:%a}@]"
        pp_simple_field index pp_simple_field array

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

  let pp_typedecl ppf (name,ty)=
    fp ppf "@[type %a=@ %a@]" X.pp name pp ty

end

module Simple_name = struct type name = string let pp = Fmt.string end

module Ty = Typexpr(Simple_name)
