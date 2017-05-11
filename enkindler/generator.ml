let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@ @->"

module S = Misc.StringSet
module M = Misc.StringMap
module Ls = Set.Make(struct
    type t = string list
    let compare: t -> t -> int= compare
  end)

type submodule =
  | Type
  | Subresult
  | Const
  | Core

type info =
  { name: string; out: Format.formatter; depends:submodule list }

module Outmap =
  Map.Make(struct type t = submodule let compare = compare end)

type outmap = submodule -> info

type result_info = {
  set: Ls.t;
  map: int M.t
}

let add_result_info set ri =
  { ri with set = Ls.add set ri.set }

type out = { map:outmap; atlas:Format.formatter; root:string }

type gen =
  { generator: string -> gen -> gen;
    map: Typed.entity M.t;
    result_info: result_info;
    out: out;
    current: S.t }

let out p name = (p.out.map name).out

let remove name g = { g with current = S.remove name g.current }
let add_result set g =
  { g with result_info = add_result_info set g.result_info }

module Enum = struct

  let contiguous_range =
    let rec range first current = function
      | [] -> Some(min first current, max first current)
      | (_, Ctype.Abs n) :: q when abs(current - n) = 1 ->
        range first n q
      | _ -> None in
    function
    | [] -> None
    | (_, Ctype.Abs n) :: q -> range n n q
    | _ -> None

  type implementation = Std | Poly
  let pp_constr dict name impl ppf (c, _ ) =
    let name = match impl with
      | Poly -> Name_study.make dict c
      | Std -> let open Name_study in
      remove_context name (make dict c) in
    match impl with
    | Std -> Fmt.pf ppf "%a" Name_study.pp_constr name
    | Poly -> Fmt.pf ppf "`%a" Name_study.pp_constr name

  let def dict impl ppf name constrs =
    let constr ppf c =
      Fmt.pf ppf "    | %a" (pp_constr dict name impl) c in
    match impl with
    | Std ->
      Fmt.pf ppf "  type t =@[<hov 2>%a@]"
        (Fmt.list ~sep constr) constrs
    | Poly ->
      Fmt.pf ppf "  type t =@[<hov 2>[%a]@]"
        (Fmt.list ~sep constr) constrs

  let to_int dict impl ppf name constrs =
    Fmt.pf ppf "\n  let to_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) ->
        Fmt.pf ppf "    | %a -> %d" (pp_constr dict name impl) c n
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let of_int dict impl ppf name constrs =
    Fmt.pf ppf "\n  let of_int = function\n";
    let constr ppf = function
      | (_, Ctype.Abs n as c) ->
        Fmt.pf ppf "    | %d -> %a" n (pp_constr dict name impl) c
      | _ -> () in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf "\n    | _ -> assert false\n"

  let pp dict impl ppf name constrs =
    Fmt.pf ppf
      "\n  let pp ppf x = Printer.fprintf ppf (match x with\n";
    let constr0 = pp_constr dict name impl in
    let constr ppf c =
      Fmt.pf ppf "    | %a -> \"%a\"" constr0 c constr0 c in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf ")\n"

  let view ppf () =
    Fmt.pf ppf
       "\n  let view = Ctypes.view ~write:to_int ~read:of_int int\n"

  let make dict impl p name constrs =
    let ppf = out p Type in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    List.iter (fun f -> f dict impl ppf name constrs)
    [def; to_int; of_int; pp ];
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.view\n\
               type %a = %a.t\n"
      Name_study.pp_var name Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name
    ; p
end

module Either = struct

  let map constrs p =
    let map = List.fold_left
        (fun m (x,n) -> match n with
           | Ctype.Abs n -> M.add x n m
           | _ -> m) M.empty constrs in
    { p with result_info = { p.result_info with map } }

  let atoms_of_name dict name =
    let open Name_study in
    S.of_list @@ List.map fst
    @@ remove_prefix ["error", ""]
    @@ path dict name

  let atoms dict names =
    List.fold_left (fun set x -> S.union set @@ atoms_of_name dict x )
      S.empty names

  let composite_path dict ok errors  =
    S.elements @@ S.union (atoms dict ok) (atoms dict errors)

  let composite_name dict ok errors =
    String.concat "_" @@ composite_path dict ok errors

  let composite_nominal dict ok errors =
    Name_study.synthetize dict @@ composite_path dict ok errors

  let pp_result dict ppf (ok,errors) =
    Fmt.pf ppf "%s" @@ composite_name dict ok errors

  let side_name dict constrs =
    Name_study.synthetize dict
    @@ S.elements @@ atoms dict
    @@ constrs

  let find name m =
    try M.find name m with
    | Not_found ->
      Fmt.(pf stderr) "Either.find: not found %s\n%!" name;
      List.iter (fun (name,id) -> Fmt.(pf stderr) "%s:%d\n%!" name id)
      @@ M.bindings m;
      raise Not_found

  let view dict m ppf constrs =
    let name = side_name dict constrs in
    let constrs =
      List.map (fun name -> name, Ctype.Abs (find name m)) constrs in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    Enum.(of_int dict Poly) ppf name constrs;
    Enum.(to_int dict Poly) ppf name constrs;
    Fmt.pf ppf "\nend\n"

  let make dict g ok errors =
    let ppf = out g Subresult in
    let m = g.result_info.map in
    let g = if m = M.empty then
        g.generator "VkResult" g
      else
        g in
    let set = g.result_info.set in
    if Ls.mem (ok @ errors) set then
      g
    else
      begin
        let m = g.result_info.map in
        if not @@ Ls.mem ok set then
            view dict m ppf ok;
        if not @@ Ls.mem errors set then
            view dict m ppf errors;
        let name = composite_nominal dict ok errors in
        let ok_name = side_name dict ok in
        let error_name = side_name dict errors in
        Fmt.pf ppf
          "let %a = Vk__result.view \n\
           ~ok:%a.(of_int,to_int) ~error:%a.(of_int,to_int)\n"
          Name_study.pp_var name
          Name_study.pp_module ok_name
          Name_study.pp_module error_name;
        add_result (ok @ errors)
        @@ add_result ok
        @@ add_result errors
        @@ g
      end
end

module Typexp = struct
  let rec pp dict ppf = function
    | Ctype.Const t -> pp dict ppf t
    | Ctype.Name n ->
      Fmt.pf ppf "%a" Name_study.pp_type (Name_study.make dict n)
    | Ctype.Ptr Name n | Ptr Const Name n ->
      Fmt.pf ppf "(ptr %a)" Name_study.pp_type (Name_study.make dict n)
    | Ctype.Ptr typ -> Fmt.pf ppf "(ptr (%a))" (pp dict) typ
    | Ctype.Option Name n ->
      Fmt.pf ppf "(option %a)" Name_study.pp_type
        (Name_study.make dict n)
    | Ctype.Option (Ptr typ) ->
      Fmt.pf ppf "(ptr_opt (%a))" (pp dict) typ
    | Ctype.Option typ -> Fmt.pf ppf "(option (%a))" (pp dict) typ

    | Ctype.String -> Fmt.pf ppf "string"
    | Ctype.Array (Some Const n ,typ) ->
      Fmt.pf ppf "(array %a @@@@ %a)"
        Name_study.pp_var (Name_study.make dict n)
        (pp dict) typ
    | Ctype.Array (_,typ) -> pp dict ppf (Ctype.Ptr typ)
    | Ctype.Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
    | Ctype.Handle _  ->
      failwith "Anonymous type"
    | Result {ok;bad} ->
      Either.pp_result dict ppf (ok,bad)
    | FunPtr _ ->
      failwith "Not_implemented: funptr"

end

module Structured = struct

  type kind = Union | Record

  let pp_kind ppf = function
    | Union -> Fmt.pf ppf "union"
    | Record -> Fmt.pf ppf "structure"

  let rec check_typ dict p = function
    | Ctype.Ptr t | Const t | Option t -> check_typ dict p t
    | Array(_,t) -> check_typ dict p t
    | Name t ->
      if S.mem t p.current then p.generator t p else p
    | Result {ok;bad} -> Either.make dict p ok bad
    | _ -> p

  let check_fields dict = List.fold_left
      (fun acc (_,t) -> check_typ dict acc t )

  let field dict name ppf (field_name,typ)=
    let field_name =
      Name_study.(remove_context name @@ make dict field_name) in
    Fmt.pf ppf "  let %a = field t \"%a\" %a"
      Name_study.pp_var field_name Name_study.pp_var field_name
      (Typexp.pp dict) typ

  let def dict kind ppf name fields =
    Fmt.pf ppf "  type t\n";
    Fmt.pf ppf "  type %a = t\n" Name_study.pp_type name;
    Fmt.pf ppf "  let t: t %a typ = %a \"%a\"\n"
      pp_kind kind
      pp_kind kind
      Name_study.pp_type name
    ;
    Fmt.list ~sep (field dict name) ppf fields;
    Fmt.pf ppf "\n  let () = Ctypes.seal t\n"

  let make dict kind p name fields =
    let ppf = out p Type in
    let p = check_fields dict p fields in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def dict kind ppf name fields;
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.t\n\
                type %a = %a.t\n"
      Name_study.pp_type name Name_study.pp_module name
      Name_study.pp_var name Name_study.pp_module name;
    p

end


module Bitset = struct

  let field dict ppf (name, value) =
    Fmt.pf ppf "  let %a = make_index %d\n"
      Name_study.pp_var (Name_study.make dict name) value

  let value dict ppf (name,value) =
    Fmt.pf ppf "  let %a = of_int %d\n"
      Name_study.pp_var (Name_study.make dict name) value

  let values dict ppf (fields,values) =
    List.iter (field dict ppf) fields;
    List.iter (value dict ppf) values

  let pp dict ppf (fields,_) =
    let field ppf (name, _) =
      let name' = Name_study.make dict name in
      Fmt.pf ppf "if mem %a set then\n\
                    Printer.fprintf ppf \"%a;@@ \"\n\
                  else ()"
        Name_study.pp_var name' Name_study.pp_var name' in
    let sep ppf () = Fmt.pf ppf ";\n" in
    Fmt.pf ppf "let pp ppf set=\n\
                Printer.fprintf ppf \"@@[{\";\n\
                %a;\n\
                Printer.fprintf ppf \"}@@]\"\n"
      (Fmt.list ~sep field) fields


  let bitname name =
    let rec bitname = function
      | ("flags", _ ) :: q ->
         ("bits", Some "Bits") :: ("flag", Some "Flag") :: q
      | [] -> raise @@ Invalid_argument "bitname []"
      | a :: q -> a :: bitname q in
    Name_study.{ name with postfix = bitname name.postfix }

  let resume ppf name =
    Fmt.pf ppf "type %a = %a.t\n"
      Name_study.pp_type name Name_study.pp_module name;
    Fmt.pf ppf "let %a = %a.view\n"
      Name_study.pp_var name Name_study.pp_module name;
    Fmt.pf ppf "let %a = %a.index_view\n"
      Name_study.pp_var (bitname name)
      Name_study.pp_module name

  let make_with_bits dict p name field_name =
    let ppf = out p Type in
    let fields = match M.find field_name p.map with
      | Typed.Type Ctype.Bitfields {fields; values} -> fields, values
      | _ -> [], [] in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    Fmt.pf ppf "  include Bitset.Make()\n";
    values dict ppf fields;
    pp dict ppf fields;
    Fmt.pf ppf "end\n";
    resume ppf name;
    p

  let make dict p name =
    let ppf =  out p Type in
    function
    | Some field_info -> make_with_bits dict p name field_info
    | None ->
      Fmt.pf (out p Type) "module %a = Bitset.Make()\n"
        Name_study.pp_module name;
      resume ppf name;
      p

end

module Handle = struct
  let make ppf name =
    Fmt.pf ppf
      "module %a = Handle.Make()\n\
       type %a = %a.t\n\
       let %a = %a.view\n"
      Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name
end

module Funptr = struct

  let make dict p tyname (fn:Ctype.fn) =
    let ppf = out p Type in
    let p = Structured.check_fields dict p fn.args in
    let p = Structured.check_typ dict p fn.return in
    let args' = match List.map snd fn.args with
      | [] -> [Ctype.Name "void"]
      | l -> l in
    Fmt.pf ppf "@[<hov> let %a = Foreign.funptr@ \
                (%a@ @->@ returning %a)@]@."
      Name_study.pp_var tyname
      (Fmt.list ~sep:arrow @@ Typexp.pp dict) args'
      (Typexp.pp dict) fn.return;
    p
end

module Fn = struct

  let make dict p (fn:Ctype.fn) =
    let ppf = out p Core in
    let p = Structured.check_fields dict p fn.args in
    let p = Structured.check_typ dict p fn.return in
    let args' = match List.map snd fn.args with
      | [] -> [Ctype.Name "void"]
      | l -> l in
    let name' = Name_study.make dict fn.name in
    Fmt.pf ppf "@[<hov>let %a =\n\
                foreign@ \"%s\"@ (%a@ @->@ returning %a)@]@."
      Name_study.pp_var name' fn.name
      (Fmt.list ~sep:arrow @@ Typexp.pp dict) args'
      (Typexp.pp dict) fn.return;
    p
end

module Const = struct
  let make p name const =
    let ppf = out p Const in
    let rec expr ppf =
      function
      | Ctype.Float f -> Fmt.pf ppf "%f" f
      | Int n ->  Fmt.pf ppf "%d" n
      | UInt64 n -> Fmt.pf ppf "Unsigned.ULLong.of_string \"%s\""
                      (Unsigned.ULLong.to_string n)
      | UInt n -> Fmt.pf ppf "Unsigned.UInt.of_string \"%s\""
                    (Unsigned.UInt.to_string n)
      | Complement num_expr -> Fmt.pf ppf "~(%a)" expr num_expr
      | Minus (a,b) -> Fmt.pf ppf "(%a)-(%a)" expr a expr b in
    Fmt.pf ppf "\nlet %a = %a\n" Name_study.pp_var name expr const;
    p
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
  Name_study.{ name with postfix = remove_bits name.postfix }

let is_bits name =
  match name.Name_study.postfix with
  | ("bits", _) :: _  -> true
  | _ -> false

let alias dict ppf name origin =
  Fmt.pf ppf "let %a = view (fun x -> x) (fun x -> x) %a\n"
    Name_study.pp_var name
    Name_study.pp_type (Name_study.make dict origin)

let make_type dict p name = function
  | Ctype.Const _  | Option _ | Ptr _ | String | Array (_,_)
  | Result _ -> p
  | Name t -> alias dict (out p Type) name t; p
  | FunPtr fn -> Funptr.make dict p name fn
  | Union fields ->
    Structured.(make dict Union) p name fields
  | Bitset { field_type = Some ft; _ } ->
    Bitset.make dict (remove ft p) name (Some ft)
  | Bitset { field_type = None; _ } ->
    Bitset.make dict p name None
  | Bitfields _ ->
    let set = "Vk" ^ (Name_study.original @@ remove_bits name) in
    begin
    try
      p.generator set p
    with Not_found ->
      Fmt.pf Fmt.stderr "make_type: not found %s\n@." set;
      raise Not_found
    end
  | Handle _ ->  Handle.make (out p Type) name; p
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let res = Name_study.make dict "VkResult" in
        let is_result = name = res in
        let kind = if is_result then Enum.Poly else Enum.Std in
        let p = Enum.make dict kind p name constrs in
        if is_result then
          Either.map constrs p
        else p
      end
    else p
  | Record r ->
    Structured.(make dict Record) p name r.fields

let right_sys name =
  let check =
    function "xlib" | "xcb" | "wl" | "khx" |
             "android" | "mir" | "win32" -> true | _ -> false in
  not @@ List.exists
    (List.exists @@ fun w -> check (Name_study.canon w) )
    Name_study.[name.prefix;name.postfix;name.main]


let extension exts name =
  match name.Name_study.postfix with
  | (a,_) :: _ when S.mem a exts  ->
    Some a
  | _ -> None

let make_ideal dict name p =
  let name' = Name_study.make dict name in
  let p = remove name p in
  if right_sys name' then
    match M.find name p.map with
    | Typed.Type t -> make_type dict p name' t
    | Fn f -> Fn.make dict p f
    | Const c -> Const.make p name' c
    | exception Not_found ->
      (Fmt.epr "make ideal: not found %s@." name; exit 2)
  else
    p

let ri_empty = { set = Ls.empty; map = M.empty}

let gen ?(result_info=ri_empty) dict out map =
  let current = S.of_list @@ List.map fst @@ M.bindings map in
  { generator = make_ideal dict;
    out;
    current; map;
    result_info
  }

let preambule =
  "open Ctypes\n\
   let libvulkan = Dl.dlopen ~filename:\"libvulkan.so\" ~flags:Dl.[RTLD_NOW]\n\
   let foreign = Foreign.foreign ~from:libvulkan\n\
   open Wayland\n\
   open Xlib\n\
   module Printer = Format\n\
  "

let exec_gen g =
  let rec loop g =
    if not (g.current = S.empty) then
      loop @@ g.generator (S.choose g.current) g
    else
      g
  in
   loop g

let make_set ?result_info dict outmap map =
   exec_gen @@ gen ?result_info dict outmap map

let open_sub atlas root name =
  Fmt.pf atlas "module %s = Vk__%s@."
    (String.capitalize_ascii name)
    (String.uncapitalize_ascii name)
  ;
  let file = root ^"/vk__" ^ name ^ ".ml" in
  let out = open_out file in
  let ppf = Format.formatter_of_out_channel out in
  Fmt.string ppf preambule; ppf

let depend (out:out) ty =
  let info = out.map ty in
  let depname ppf ty = Fmt.pf ppf "open Vk__%s" (out.map ty).name in
  List.iter (fun ty -> Fmt.pf info.out "%a\n" depname ty)
    info.depends

let pp_extension result_info dict out name m =
  let ppf = open_sub out.atlas out.root name in
  let map = function
    | Type | Const | Subresult as s -> out.map s
    | Core -> { name; out = ppf; depends = [Type; Subresult] }
  in
  let out = { out with map } in
  depend out Core;
  Fmt.pf ppf "module Make()=struct";
  let g = make_set ~result_info dict out m in
  Fmt.pf ppf "\nend@.";
  g.result_info


let make_out root =
  let atlas = Format.formatter_of_out_channel
    @@ open_out @@ root ^"/vk.ml" in
  let m =
    let ($=) x (y,deps) =
      x,
      { name = y; out = open_sub atlas root y; depends = deps } in
    List.fold_left (fun m (k,x) -> Outmap.add k x m) Outmap.empty
      [ Type $= ("types", [Const]);
        Const $= ("consts", []);
        Core $= ("core", [Type; Const;Subresult]);
        Subresult $= ("subresult", []) ] in
  let map name = Outmap.find name m in
  let out = { root; atlas; map } in
  List.iter (depend out) [Core;Type]; out


let make_all extensions dict root map =
  let output = make_out root in
  let split name obj (major,exts) =
    match obj, extension extensions (Name_study.make dict name) with
    | _, None | Typed.(Type _| Const _) , _ ->
      M.add name obj major, exts
    | Typed.Fn _, Some ext ->
      let extmap = try M.find ext exts with Not_found -> M.empty in
      major, M.add ext (M.add name obj extmap) exts in
  let major,exts = M.fold split map (M.empty, M.empty) in
  let g = make_set dict output major in
  let _ri =
  M.fold (fun name ext ri ->
      pp_extension ri dict output name ext) exts g.result_info in
  List.iter (fun t -> Fmt.pf (out g t) "@.")
    [Type;Const;Core;Subresult]
