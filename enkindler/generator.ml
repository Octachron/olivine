let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@ @->"

module S = Misc.StringSet
module M = Misc.StringMap
module Ls = Set.Make(struct
    type t = string list
    let compare: t -> t -> int= compare
  end)

type gen =
  { generator: string -> gen -> gen;
    map: Typed.entity M.t;
    result_set: Ls.t;
    current: S.t }

let remove name g = { g with current = S.remove name g.current }
let add_result set g = { g with result_set = Ls.add set g.result_set }

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
    Fmt.pf ppf "  type t =\n%a" (Fmt.list ~sep constr) constrs

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

  let view ppf () =
    Fmt.pf ppf
       "\n  let view = Ctypes.view ~write:to_int ~read:of_int int\n"

  let make dict impl ppf name constrs =
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    def dict impl ppf name constrs;
    to_int dict impl ppf name constrs;
    of_int dict impl ppf name constrs;
    view ppf ();
    Fmt.pf ppf "end\n";
    Fmt.pf ppf "let %a = %a.view\n\
               type %a = %a.t\n"
      Name_study.pp_var name Name_study.pp_module name
      Name_study.pp_type name Name_study.pp_module name

end

module Either = struct

  let map g =
    match M.find "VkResult" g.map with
    | Typed.Type Ctype.Enum constrs ->
      let m = M.empty in
      List.fold_left
        (fun m (x,n) -> match n with
           | Ctype.Abs n -> M.add x n m
           | _ -> m) m constrs
    | _ -> assert false


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
      Fmt.(pf stderr) "not found: %s\n%!" name;
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

  let make dict g ppf ok errors =
    if Ls.mem (ok @ errors) g.result_set then
      g
    else
      begin
        let m = map g in
        if not @@ Ls.mem ok g.result_set then
            view dict m ppf ok;
        if not @@ Ls.mem errors g.result_set then
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
    | Ctype.Array (_,typ) -> Fmt.pf ppf "( ptr (%a) )" (pp dict) typ
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

  let rec check_typ dict ppf p = function
    | Ctype.Ptr t | Const t | Option t -> check_typ dict ppf p t
    | Array(_,t) -> check_typ dict ppf p t
    | Name t ->
      if S.mem t p.current then p.generator t p else p
    | Result {ok;bad} -> Either.make dict p ppf ok bad
    | _ -> p

  let check_fields dict ppf = List.fold_left
      (fun acc (_,t) -> check_typ dict ppf acc t )

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

  let make dict kind ppf p name fields =
    let p = check_fields dict ppf p fields in
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

  let make_with_bits dict ppf p name field_name =
    let fields = match M.find field_name p.map with
      | Typed.Type Ctype.Bitfields {fields; values} -> fields, values
      | _ -> [], [] in
    Fmt.pf ppf "module %a = struct\n" Name_study.pp_module name;
    Fmt.pf ppf "  include Bitset.Make()\n";
    values dict ppf fields;
    Fmt.pf ppf "end\n";
    resume ppf name;
    p

  let make dict ppf p name = function
    | Some field_info -> make_with_bits dict ppf p name field_info
    | None ->
      Fmt.pf ppf "module %a = Bitset.Make()\n"
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

  let make dict ppf p tyname (fn:Ctype.fn) =
    let p = Structured.check_fields dict ppf p fn.args in
    let p = Structured.check_typ dict ppf p fn.return in
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

  let make dict ppf p (fn:Ctype.fn) =
    let p = Structured.check_fields dict ppf p fn.args in
    let p = Structured.check_typ dict ppf p fn.return in
    let args' = match List.map snd fn.args with
      | [] -> [Ctype.Name "void"]
      | l -> l in
    let name' = Name_study.make dict fn.name in
    Fmt.pf ppf "@[<hov>let %a = \n\
                foreign@ \"%s\"@ (%a@ @->@ returning %a)@]@."
      Name_study.pp_var name' fn.name
      (Fmt.list ~sep:arrow @@ Typexp.pp dict) args'
      (Typexp.pp dict) fn.return;
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

let make_type dict ppf p name = function
  | Ctype.Const _  | Option _ | Ptr _ | String | Array (_,_)
  | Result _ -> p
  | Name t -> alias dict ppf name t; p
  | FunPtr fn -> Funptr.make dict ppf p name fn
  | Union fields ->
    Structured.(make dict Union) ppf p name fields
  | Bitset { field_type = Some ft; _ } ->
    Bitset.make dict ppf (remove ft p) name (Some ft)
  | Bitset { field_type = None; _ } ->
    Bitset.make dict ppf p name None
  | Bitfields _ ->
    let set = "Vk" ^ (Name_study.original @@ remove_bits name) in
    begin
    try
      p.generator set p
    with Not_found ->
      Fmt.pf Fmt.stderr "Not found %s\n%!" set;
      raise Not_found
    end
  | Handle _ ->  Handle.make ppf name; p
  | Enum constrs ->
    if not @@ is_bits name then
      Enum.make dict Enum.Std ppf name constrs
    ; p
  | Record r ->
    Structured.(make dict Record) ppf p name r.fields

let right_sys name =
  let check =
    function "xlib" | "xcb" | "wl" | "khx" |
             "android" | "mir" | "win32" -> true | _ -> false in
  not @@ List.exists
    (List.exists @@ fun w -> check (Name_study.canon w) )
    Name_study.[name.prefix;name.postfix;name.main]

let make_ideal dict ppf name p =
  let name' = Name_study.make dict name in
  let p = remove name p in
  if right_sys name' then
    let obj = M.find name p.map in
    match obj with
    | Typed.Type t -> make_type dict ppf p name' t
    | Fn f -> Fn.make dict ppf p f
    | Const _c -> p
  else
    p

let gen dict ppf map =
  let current = S.of_list @@ List.map fst @@ M.bindings map in
  { generator = make_ideal dict ppf;
    current; map;
    result_set = Ls.empty }


let preambule =
  "open Ctypes\n\
   let libvulkan = Dl.dlopen ~filename:\"libvulkan.so\" ~flags:Dl.[RTLD_NOW]\n\
   let foreign = Foreign.foreign ~from:libvulkan\n\
   \n\
   open Wayland\n\
   open Xlib\n\
  "


let make_all dict ppf map =
  Fmt.string ppf preambule;
  let g = gen dict ppf map in
  let rec loop g =
    if g.current = S.empty then
      ()
    else
      loop @@ g.generator (S.choose g.current) g in
  loop g
