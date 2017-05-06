type name = string

type pos = Abs of int | Bit of int | Offset of int
type constructor = name * pos

type integer = private I
type real = private R
type uint64 = Unsigned.ULLong.t
type uint = Unsigned.UInt.t


type num_expr =
  | Float of float
  | Int of int
  | UInt64 of uint64
  | UInt of uint
  | Complement of num_expr
  | Minus of num_expr * num_expr

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

let rec pp_num_expr ppf = function
  | Float f -> Fmt.float ppf f
  | Int n -> Fmt.int ppf n
  | UInt n -> Fmt.string ppf @@ Unsigned.UInt.to_string n
  | UInt64 n -> Fmt.string ppf @@ Unsigned.ULLong.to_string n
  | Complement n -> Fmt.pf ppf "~(%a)" pp_num_expr n
  | Minus (a,b) -> Fmt.pf ppf "%a - %a" pp_num_expr a pp_num_expr b


type 'a constexpr =
  | Lit of 'a
  | Var of name
  | Const of name
  | Null_terminated
  | Math_expr


(*
type 'a typexp =
  | Const: [`Not_const] typexp -> [`Const] typexp
  | Name: name -> [`Not_const] typexp
  | Ptr: 'any typexp -> [`Not_const] typexp
  | Array: constexpr option * 'any typexp -> [`Not_const] typexp
  | FunPtr: { name:name; return: 'any typexp; args: field list } ->
    [`Const] typexp
  | Enum: constructor list -> [`Not_const] typexp
  | Record: field list -> [`Not_const] typexp
  | Union: field list -> [`Not_const] typexp
and typexpr = T: 'a typexp -> typexpr *)
type typexpr =
  | Const of typexpr
  | Name of name
  | Ptr of typexpr
  | Option of typexpr
  | String
  | Array of int constexpr option * typexpr
  | FunPtr of fn
  | Enum of constructor list
  | Union of field list
  | Bitset of { implementation:name; field_type: name option}
  | Bitfields of
      {
        fields: (name * int) list;
        values: (name * int) list
      }
  | Handle of { parent: name option; dispatchable:bool }
  | Result of { ok: name list; bad: name list }
  | Record of {is_private:bool; fields: field list}

and field = name * typexpr
and fn = { name:name; return: typexpr; args: field list }

type type_decl = name * typexpr


let fp = Fmt.pf

let pp_list sep = Fmt.list ~sep
let const f ppf () = fp ppf f

let pp_constexp pp ppf = function
  | Lit n -> fp ppf "%a" pp n
  | Var name -> fp ppf "%s" name
  | Null_terminated -> fp ppf "%s" "null-terminated"
  | Math_expr -> fp ppf "%s" "math-expr"
  | Const name -> fp ppf "%s" name

let pp_bitfield ppf (name,int) =
  fp ppf "%d:%s" int name

let rec pp ppf = function
  | Const c -> fp ppf "const %a" pp c
  | Name n -> fp ppf "%s" n
  | Ptr (Name _ |Ptr _ as ty) -> fp ppf "ptr %a" pp ty
  | Ptr ty -> fp ppf "ptr (%a)" pp ty
  | Option (Name _ as ty) -> fp ppf "option %a" pp ty
  | Option (Ptr _ as ty) -> fp ppf "ptr_opt %a" pp ty
  | Option ty -> fp ppf "option (%a)" pp ty
  | Array(None, ty) -> fp ppf "array (%a)" pp ty
  | Array(Some cexp, ty) -> fp ppf "array[%a](%a)" (pp_constexp Fmt.int) cexp pp ty
  | FunPtr fn -> pp_fn ppf fn
  | Enum cs ->
    fp ppf "@[[%a]@]" (pp_list (const "@ | ") pp_constr) cs
  | Record {is_private; fields} ->
    fp ppf "@[%s{%a}@]" (if is_private then "private" else "")
      (pp_list (const ";@ ") pp_field) fields
  | Union fields ->
    fp ppf "@[[%a]@]" (pp_list (const "@ ||") pp_field) fields
  | Bitset {implementation; field_type} ->
    fp ppf "Bitset@[@ {implementation:%s;@ field_type:(%a)}@]"
      implementation Fmt.(option string)  field_type
 | Bitfields {fields; values} ->
    let bitfields = pp_list (const ";@ ") pp_bitfield in

    fp ppf "Bitfields @ \
            @[{fields:@ @[[%a]@];@ \
            values:@ @[[%a]@] @ }@]"
       bitfields fields bitfields values
  | Handle h ->
    fp ppf "Handle@ @[{parent:%a; dispatchable:%b}@]"
      Fmt.(option string) h.parent h.dispatchable
  | String -> fp ppf "string"
  | Result {ok;bad} ->
    let ppl = pp_list (const "@ | ") Fmt.string in
    fp ppf "@[either@ [ok:%a]@ [bad:%a]@]"
      ppl ok ppl bad

and pp_field ppf (name, t) =
  fp ppf "%s:%a" name pp t

and pp_constr ppf (name,pos) =
  fp ppf "%s(%a)" name pp_pos pos

and pp_pos ppf = function
  | Abs n -> fp ppf "%d" n
  | Offset n -> fp ppf "+%d" n
  | Bit n -> fp ppf "^%d" n

and pp_fn ppf fn =
  fp ppf "fun %s@ (%a)@ ⇒ %a" fn.name
      (pp_list (const ",@ ") pp_field) fn.args
      pp fn.return

let pp_typedecl ppf (name,ty)=
  fp ppf "@[type %s=@ %a@]" name pp ty
