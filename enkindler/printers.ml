module Ty = Lib_builder.Ty
module L = Name_study
module Arith = Ctype.Arith
module B = Lib_builder

let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@ @->"

module Enum = struct

  let contiguous_range =
    let rec range first current = function
      | [] -> Some(min first current, max first current)
      | (_, Ty.Abs n) :: q when abs(current - n) = 1 ->
        range first n q
      | _ -> None in
    function
    | [] -> None
    | (_, Ty.Abs n) :: q -> range n n q
    | _ -> None

  type implementation = Std | Poly

  let pp_constr name impl ppf (c, _ ) =
    let name = L.remove_context name c in
    match impl with
    | Std -> Fmt.pf ppf "%a" L.pp_constr name
    | Poly -> Fmt.pf ppf "`%a" L.pp_constr name

  let def impl ppf name constrs =
    let constr ppf c =
      Fmt.pf ppf "    | %a" (pp_constr name impl) c in
    match impl with
    | Std ->
      Fmt.pf ppf "  type t =@[<hov 2>%a@]"
        (Fmt.list ~sep constr) constrs
    | Poly ->
      Fmt.pf ppf "  type t =@[<hov 2>[%a]@]"
        (Fmt.list ~sep constr) constrs

  let to_int impl ppf name constrs =
    Fmt.pf ppf "\n  let to_int = function\n";
    let constr ppf = function
      | (_, Ty.Abs n as c) ->
        Fmt.pf ppf "    | %a -> %d" (pp_constr name impl) c n
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let of_int impl ppf name constrs =
    Fmt.pf ppf "\n  let of_int = function\n";
    let constr ppf = function
      | (_, Ty.Abs n as c) ->
        Fmt.pf ppf "    | %d -> %a" n (pp_constr name impl) c
      | _ -> () in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf "\n    | _ -> assert false\n"

  let pp impl ppf name constrs =
    Fmt.pf ppf
      "\n  let pp ppf x = Printer.fprintf ppf (match x with\n";
    let constr0 = pp_constr name impl in
    let constr ppf c =
      Fmt.pf ppf "    | %a -> \"%a\"" constr0 c constr0 c in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf ")\n"

  let view ppf () =
    Fmt.pf ppf
       "\n  let view = Ctypes.view ~write:to_int ~read:of_int int\n"

  let pp impl ppf (name,constrs) =
    Fmt.pf ppf "module %a = struct\n" L.pp_module name;
    List.iter (fun f -> f impl ppf name constrs)
    [def; to_int; of_int; pp ];
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.view\n\
               type %a = %a.t\n"
      L.pp_var name L.pp_module name
      L.pp_type name L.pp_module name

end


module Result = struct

  module M = B.Result.Map
  open Subresult
(*
  let pp_result ppf (ok,errors) =
    Fmt.pf ppf "%s" @@ composite_name dict ok errors
*)

  let find name m =
    try M.find name m with
    | Not_found ->
      Fmt.(pf stderr) "Either.find: not found %a\n%!"
        L.pp_var name;
      List.iter (fun (name,id) -> Fmt.(pf stderr) "%a:%d\n%!"
                    L.pp_var name id)
      @@ M.bindings m;
      raise Not_found

  let view m ppf name constrs =
    let constrs =
      List.map (fun name -> name, Ty.Abs (find name m)) constrs in
    Fmt.pf ppf "module %a = struct\n" L.pp_module name;
    Enum.(of_int Poly) ppf name constrs;
    Enum.(to_int Poly) ppf name constrs;
    Fmt.pf ppf "\nend\n"

  let pp_type ppf (ok,errors) =
    Fmt.pf ppf "%a" L.pp_type @@ composite_nominal ok errors
  let pp m ppf (name,ok,errors) =
    match ok, errors with
    | [], x | x, [] ->
      view m ppf name x;
    | _ ->
        let name = Subresult.composite_nominal ok errors in
        let ok_name = Subresult.side_name ok in
        let error_name = Subresult.side_name errors in
        Fmt.pf ppf
          "let %a = Vk__result.view \n\
           ~ok:%a.(of_int,to_int) ~error:%a.(of_int,to_int)\n"
          L.pp_var name
          L.pp_module ok_name
          L.pp_module error_name;
end


module Typexp = struct
  let rec pp ppf = function
    | Ty.Const t -> pp ppf t
    | Name n ->
      Fmt.pf ppf "%a" L.pp_type n
    | Ptr Name n | Ptr Const Name n ->
      Fmt.pf ppf "(ptr %a)" L.pp_type n
    | Ptr typ -> Fmt.pf ppf "(ptr (%a))" pp typ
    | Option Name n ->
      Fmt.pf ppf "(option %a)" L.pp_type n
    | Option (Ptr typ) ->
      Fmt.pf ppf "(ptr_opt (%a))" pp typ
    | Option typ -> Fmt.pf ppf "(option (%a))" pp typ

    | String -> Fmt.pf ppf "string"
    | Array (Some Const n ,typ) ->
      Fmt.pf ppf "(array %a @@@@ %a)" L.pp_var n pp typ
    | Array (_,typ) -> pp ppf (Ty.Ptr typ)
    | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
    | Handle _  ->
      failwith "Anonymous type"
    | Result {ok;bad} ->
      Result.pp_type ppf (ok,bad)
    | FunPtr _ ->
      failwith "Not_implemented: funptr"
end

module Structured = struct

  type kind = Union | Record

  let pp_kind ppf = function
    | Union -> Fmt.pf ppf "union"
    | Record -> Fmt.pf ppf "structure"

  let field name ppf (field_name,typ)=
    let field_name = L.remove_context name field_name in
    Fmt.pf ppf "  let %a = field t \"%a\" %a"
      L.pp_var field_name L.pp_var field_name Typexp.pp typ

  let def kind ppf name fields =
    Fmt.pf ppf "  type t\n";
    Fmt.pf ppf "  type %a = t\n" L.pp_type name;
    Fmt.pf ppf "  let t: t %a typ = %a \"%a\"\n"
      pp_kind kind
      pp_kind kind
      L.pp_type name
    ;
    Fmt.list ~sep (field name) ppf fields;
    Fmt.pf ppf "\n  let () = Ctypes.seal t\n"

  let pp kind ppf (name,fields) =
    Fmt.pf ppf "module %a = struct\n" L.pp_module name;
    def kind ppf name fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.t\n\
                type %a = %a.t\n"
      L.pp_type name L.pp_module name
      L.pp_var name L.pp_module name

end


module Bitset = struct

  let bit_name name =
  let rec bitname = function
    | ("flags", _ ) :: q ->
      ("bits", Some "Bits") :: ("flag", Some "Flag") :: q
    | [] ->
      raise @@ Invalid_argument "bitname []"
    | a :: q -> a :: bitname q in
  L.{ name with postfix = bitname name.postfix }

  let set_name name =
    let rec rename = function
      |  ("bits", Some "Bits") :: ("flag", Some "Flag") :: q ->
        ("flags", Some "Flags" ) :: q
      | [] ->
        raise @@ Invalid_argument "empty bitset name []"
      | a :: q -> a :: rename q in
    L.{ name with postfix = rename name.postfix }

  let value_name set_name name =
    L.remove_context set_name name

  let field_name set_name name = let open L in
    let context =
      { set_name with postfix = set_name.postfix @ [ word "bit" ]  } in
      remove_context context name

  let field set_name ppf (name, value) =
    Fmt.pf ppf "  let %a = make_index %d\n"
      L.pp_var (field_name set_name name) value

  let value set_name ppf (name,value) =
    let name = value_name set_name name in
    Fmt.pf ppf "  let %a = of_int %d\n" L.pp_var name value

  let values set_name ppf (fields,values) =
    List.iter (field set_name ppf) fields;
    List.iter (value set_name ppf) values

  let pp_pp set ppf (fields,_) =
    let field ppf (name, _) =
      let name' = field_name set name in
      Fmt.pf ppf "if mem %a set then\n\
                    Printer.fprintf ppf \"%a;@@ \"\n\
                  else ()"
        L.pp_var name' L.pp_var name' in
    let sep ppf () = Fmt.pf ppf ";\n" in
    Fmt.pf ppf "let pp ppf set=\n\
                Printer.fprintf ppf \"@@[{\";\n\
                %a;\n\
                Printer.fprintf ppf \"}@@]\"\n"
      (Fmt.list ~sep field) fields

  let resume ppf bitname name =
    Fmt.pf ppf "type %a = %a.t\n"
      L.pp_type name L.pp_module name;
    Fmt.pf ppf "let %a = %a.view\n"
      L.pp_var name L.pp_module name;
    Fmt.pf ppf "let %a = %a.index_view\n"
      L.pp_var bitname
      L.pp_module name

  let pp_with_bits ppf (bitname,fields) =
    let name = set_name bitname in
    let core_name = let open L in
      { name with postfix = List.filter (fun (x,_) -> x <> "flags") name.postfix }
    in
    Fmt.pf ppf "module %a = struct\n" L.pp_module name;
    Fmt.pf ppf "  include Bitset.Make()\n";
    values core_name ppf fields;
    pp_pp core_name ppf fields;
    Fmt.pf ppf "end\n";
    resume ppf bitname name

  let pp ppf (name,opt) =
    let bitname = bit_name name in
    match opt with
    | Some _ -> ()
    | None ->
      Fmt.pf ppf "module %a = Bitset.Make()\n"
        L.pp_module name;
      resume ppf bitname name

end

module Handle = struct
  let pp ppf name =
    Fmt.pf ppf
      "module %a = Handle.Make()\n\
       type %a = %a.t\n\
       let %a = %a.view\n"
      L.pp_module name
      L.pp_type name L.pp_module name
      L.pp_type name L.pp_module name
end

module Funptr = struct

  let pp_core ppf (fn:Ty.fn) =
    Fmt.pf ppf "@[(Foreign.funptr @@@@@ @ %a@ @->@ returning %a)@]"
      (Fmt.list ~sep:arrow Typexp.pp)
      (List.map snd fn.args)
      Typexp.pp fn.return

  let pp  ppf (tyname, (fn:Ty.fn)) =
    match List.map snd fn.args with
    | [] ->
      Fmt.pf ppf "@[<hov> let %a = ptr void @]@."
        L.pp_var tyname
    | _ ->
      Fmt.pf ppf "@[<hov> let %a = %a@]@."
        L.pp_var tyname pp_core fn

end

module Fn = struct

  let pp ppf (fn:Ty.fn) =
    let args' = match List.map snd fn.args with
      | [] -> [Ty.Name (L.simple [L.word "void"])]
      | l -> l in
    Fmt.pf ppf "@[<hov>let %a =\n\
                foreign@ \"%s\"@ (%a@ @->@ returning %a)@]@."
      L.pp_var fn.name (L.original fn.name)
      (Fmt.list ~sep:arrow Typexp.pp) args'
      Typexp.pp fn.return

end

module DFn = struct

  let pp ppf (fn:Ty.fn) =
    Fmt.pf ppf "@[<hov>let %a =\n\
                get \"%s\" (%a) @]@."
      L.pp_var fn.name
      (L.original fn.name) Funptr.pp_core fn
end


module Const = struct
  let pp ppf (name,const) =
    let rec expr ppf =
      function
      | Arith.Float f -> Fmt.pf ppf "%f" f
      | Int n ->  Fmt.pf ppf "%d" n
      | UInt64 n -> Fmt.pf ppf "Unsigned.ULLong.of_string \"%s\""
                      (Unsigned.ULLong.to_string n)
      | UInt n -> Fmt.pf ppf "Unsigned.UInt.of_string \"%s\""
                    (Unsigned.UInt.to_string n)
      | Complement num_expr -> Fmt.pf ppf "~(%a)" expr num_expr
      | Minus (a,b) -> Fmt.pf ppf "(%a)-(%a)" expr a expr b in
    Fmt.pf ppf "\nlet %a = %a\n" L.pp_var name expr const
end

let rec last = function
  | [] -> raise @@ Invalid_argument "last []"
  | [a] -> a
  | _ :: q -> last q

let remove_bits name =
  let rec remove_bits = function
    | [] -> raise @@ Invalid_argument "last []"
    | ("bits", _ ) :: ("flag", Some f ) ::  q -> ("flags", Some (f ^ "s") ) :: q
    | ("bits", _ ) :: ("flag", None ) ::  q -> ("flags", Some "flags") :: q
    | a :: q -> a :: remove_bits q
  in
  L.{ name with postfix = remove_bits name.postfix }

let is_bits name =
  match name.L.postfix with
  | ("bits", _) :: _  -> true
  | _ -> false

let pp_alias ppf (name,origin) =
  Fmt.pf ppf "let %a = view (fun x -> x) (fun x -> x) %a\n"
    L.pp_var name
    L.pp_type origin

let pp_type results ppf (name,ty) =
  match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> ()
  | Result {ok;bad} -> Result.pp results ppf (name,ok,bad)
  | Name t -> pp_alias ppf (name,t)
  | FunPtr fn -> Funptr.pp ppf (name,fn)
  | Union fields -> Structured.(pp Union) ppf (name,fields)
  | Bitset { field_type = Some _; _ } -> ()
  | Bitset { field_type = None; _ } -> Bitset.pp ppf (name,None)
  | Bitfields {fields;values} ->
    Bitset.pp_with_bits ppf (name,(fields,values))
  | Handle _ ->  Handle.pp ppf name
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let is_result = L.(canon @@ List.hd name.main) = "result" in
        let kind = if is_result then Enum.Poly else Enum.Std in
        Enum.pp kind ppf (name,constrs)
      end
  | Record r ->
    Structured.(pp Record) ppf (name,r.fields)

let pp_item results ppf (name, item) = match item with
  | B.Type t -> pp_type results ppf (name,t)
  | Const c -> Const.pp ppf (name,c)
  | Fn f -> Fn.pp ppf f


let rec pp_module results ppf (m:B.module') =
  Fmt.pf ppf
    "module %s%a= @[<v 2> struct@;\%aend@]@.
    "
    (String.capitalize_ascii m.name)
    (pp_sig results) m
    pp_args m.args
and pp_sig results ppf (m:B.module') =
    Fmt.pf ppf "%a@;@;%a@;"
    (Fmt.list @@ pp_item results) m.sig'
    (Fmt.list @@ pp_module results) m.submodules

and pp_args ppf = function
  | [] -> ()
  | args -> Fmt.list pp_arg ppf args
and pp_arg ppf arg = Fmt.pf ppf "(%s)" arg

let atlas ppf modules =
  let pp_alias ppf (m:B.module') =
    Fmt.pf ppf "module %s = Vk__%s@;"
      (String.capitalize_ascii m.name) m.name
  in
  Fmt.pf ppf "@[<v>%a@]@." (Fmt.list pp_alias) modules

let lib (lib:B.lib) =
  let open_file n =
    Format.formatter_of_out_channel @@ open_out @@ lib.root ^ "/" ^ n in
  let  pp_preambule ppf (m:B.module') =
    Fmt.pf ppf "%s\n%s\n" lib.preambule m.preambule in
  atlas (open_file "vk.ml") lib.content.submodules;
  let pp_sub (m:B.module') =
    let ppf = open_file ("vk__" ^ m.name ^ ".ml") in
    pp_preambule ppf m;
    pp_sig lib.result ppf m in
  List.iter pp_sub lib.content.submodules
