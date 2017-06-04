module B = Lib_builder
module T = B.T
module Ty = B.Ty
module L = Name_study
module Arith = B.Arith

let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@ @->"

module H = struct

  let is_option = function
    | Ty.Option _ -> true
    | _ -> false

  let is_ptr_option = function
    | Ty.Ptr Option _ -> true
    | _ -> false


  let rec find_field_type name = function
    | [] -> None
    | Ty.Simple(n,ty) :: _  when n = name -> Some ty
    | Array_f { index= n, ty ; _ } :: _ when n = name -> Some ty
    | _ :: q -> find_field_type name q

  let is_option_f = function
    | Ty.Simple (_, (Option _ | Const Option _) )
    | Ty.Array_f { index = _, (Option _  | Const Option _ ); _ }
    | Ty.Array_f { array = _, (Option _ | Const Option _); _ }
    | Ty.Record_extension _ -> true
    | _ -> false

  let size_t = L.simple ["size";"t"]
  (* FIXME *)

  let rec opt pp ppf (n, typ as f) = match typ with
    | Ty.Option typ  -> Fmt.pf ppf "Some(%a)" (opt pp) (n,typ)
    | Name x ->
      Fmt.pf ppf  "%a.of_int(%a)" L.pp_module x pp f
    | _ -> pp ppf f

  let array_len var ppf ((_,typ), (name,_)) =
    let len ppf (n,_) =
      Fmt.pf ppf "Ctypes.CArray.length %a" var n in
    opt len ppf (name,typ)

  let label_name ppf = function
    | Ty.Simple (n,_) -> L.pp_var ppf n
    | Ty.Array_f { array=(n,_); _ } -> L.pp_var ppf n
    | Ty.Record_extension _ -> Fmt.pf ppf "extension"

    let arg_name pre ppf = function
    | Ty.Simple (n,_) -> pre ppf; L.pp_var ppf n
    | Ty.Array_f { array=(n,_); _ } -> pre ppf; L.pp_var ppf n
    | Ty.Record_extension _ -> Fmt.pf ppf "(%text=No_extension)" pre

  let pp_label prefix ppf f =
    let kind= if is_option_f f then "?" else "~" in
    Fmt.pf ppf "%s%a:%a" kind label_name f (arg_name prefix) f

  let pp_fnlabel prefix ppf f = pp_label prefix ppf f.Ty.field

  let pp_terminator fields ppf =
    if List.exists is_option_f fields then
      Fmt.pf ppf "()"
    else ()

  let is_extension =
    function
    | Ty.Record_extension _ -> true
    | _ -> false

  let record_extension fields =
    match List.find is_extension fields with
    | exception Not_found -> None
    | Ty.Record_extension {exts;_} ->  Some exts
    | _ -> None

  let pp_opty ty pp ppf = match ty with
    | Ty.Option _ -> Fmt.pf ppf "Some(%a)" pp
    | _ -> pp ppf

  let pp_start pp ppf = Fmt.pf ppf "Ctypes.CArray.start %a" pp

  let pp_fname var ppf (n, _ ) = var ppf n

  let is_result_name = function
    | {Name_study.main = ["result"]; prefix=[]; postfix = [] } ->
      true
    | _ -> false

  let is_void = function
    | Ty.Name {Name_study.main = ["void"]; prefix=[]; postfix = [] }->
      true
    | _ -> false

  let is_result = function
    | Ty.Result _ -> true
    | _ -> false




  let rec pp_to_int var ppf = function
    | Ty.Ptr t ->
      let var ppf = Fmt.pf ppf "Ctypes.(!@@)(%t)" var in
      pp_to_int var ppf t
    | Option t ->
      let var ppf = Fmt.pf ppf "unwrap(%t)" var in
      pp_to_int var ppf t
    | Name { main = ["uint32"; "t"] ; prefix =[]; postfix = [] }
      -> var ppf
    | Name t -> Fmt.pf ppf "%a.to_int (%t)" L.pp_module t var
    | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"

  let find_record tn types =
    match List.assoc tn types with
    | B.Type Ty.Record{ fields; _ } -> fields
    | B.Type ty ->
      Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
      raise @@ Invalid_argument "Non-record path, yet a type"
    | _ ->
      raise @@ Invalid_argument "Non-record path: not even a type"

  let type_path types fields p =
    let rec type_path (types) acc (ty, fields) = function
      | [] -> acc
      | [a] -> (ty,a) :: acc
      | a :: q ->
        match find_field_type a fields with
        | Some (Ty.Const Ptr Name tn| Ptr Name tn) ->
          let fields = find_record tn types in
              type_path types ( (ty, a) :: acc) (Some tn,fields) q
        | Some ty ->
          Fmt.epr "Path ended with %a@.%!" Ty.pp ty;
          raise @@ Invalid_argument "Non-record path"
        | _ -> raise @@ Invalid_argument "Unknown type in path"
    in
    type_path types [] (None, fields) p

  let rec pp_path pp ppf =
    function
    | [] -> raise @@ Invalid_argument "Printers.pp_path: empty path"
    | [None, a] -> pp ppf a
    | (Some ty, a) :: q ->
      Fmt.pf ppf "Ctypes.getf@ (Ctypes.(!@@)@ (%a)) %a.Fields.%a"
        (pp_path pp) q L.pp_module ty L.pp_var a
    | (None, _) :: _ -> raise @@
      Invalid_argument "Printers.pp_path: p artially type type path"

  let to_fields = List.map (fun f -> f.Ty.field)

  let rec final_option types fields =
    function
    | [] -> raise (Invalid_argument "Empty type path")
    | [ name ] ->
      begin match find_field_type name fields with
        | Some Ty.Option _ -> true
        | _  -> false
      end
    | name :: q ->
      final_option types (find_record name types) q

  let pp_path_full pp types fields ppf p =
    let p = type_path types fields p in
    pp_path pp ppf p

  let nl ppf () = Fmt.cut ppf ()
end

module Bitset_pp = struct
  open Aster.Bitset

  let field set_name ppf (name, value) =
    Fmt.pf ppf "let %a = make_index %d@;"
      L.pp_var (field_name set_name name) value

  let value set_name ppf (name,value) =
    let name = value_name set_name name in
    Fmt.pf ppf "let %a = of_int %d@;" L.pp_var name value

  let values set_name ppf (fields,values) =
    List.iter (field set_name ppf) fields;
    List.iter (value set_name ppf) values

  let pp_pp set ppf (fields,_) =
    let field ppf (name, _) =
      let name' = field_name set name in
      Fmt.pf ppf {|%a,"%a"|}
        L.pp_var name' L.pp_var name' in
    let sep ppf () = Fmt.pf ppf ";@ " in
    Fmt.pf ppf "@[<v 2> let pp x = pp_tags @[<hov>[%a]@]@;@] x"
      (Fmt.list ~sep field) fields

  let resume ppf bitname name =
    Fmt.pf ppf "type %a = %a.t@;"
      L.pp_type name L.pp_module name;
    Fmt.pf ppf "let %a, %a_opt = %a.(view, view_opt)@;"
      L.pp_var name L.pp_var name L.pp_module name;
    Fmt.pf ppf "let %a = %a.index_view@;"
      L.pp_var bitname
      L.pp_module name;
    Fmt.pf ppf "let %a_opt = %a.index_view_opt@;"
      L.pp_var bitname
      L.pp_module name


  let pp_with_bits ppf (bitname,fields) =
    let name = set_name bitname in
    let core_name = let open L in
      { name with postfix = List.filter (fun x -> x <> "flags") name.postfix }
    in
    Fmt.pf ppf "@[<v 2> module %a = struct@;" L.pp_module name;
    Fmt.pf ppf "  include Bitset.Make()@;";
    values core_name ppf fields;
    pp_pp core_name ppf fields;
    Fmt.pf ppf "@; end@]@;";
    resume ppf bitname name

  let pp ppf (name,opt) =
    let bitname = bit_name name in
    match opt with
    | Some _ -> ()
    | None ->
      Fmt.pf ppf "@[module %a = Bitset.Make()@]@."
        L.pp_module name;
      resume ppf bitname name

end

module Bitset = struct
  let pp_with_bits ppf data =
    Pprintast.structure ppf @@ Aster.Bitset.make_extended data
  let pp ppf data =
    Pprintast.structure ppf @@ Aster.Bitset.make data
end

module Handle_pp = struct
  let pp ppf name =
    Fmt.pf ppf
      "module %a = Handle.Make()\n\
       type %a = %a.t\n\
       let %a, %a_opt = %a.(view,view_opt)\
\n"
      L.pp_module name
      L.pp_type name L.pp_module name
      L.pp_var name L.pp_var name L.pp_module name
end

module Handle = struct
  let pp ppf name = Aster.Handle.make name |> Pprintast.structure ppf
end

module Enum = struct
  let pp impl ppf (name,constrs) =
    Aster.Enum.make impl (name,constrs) |>
    Pprintast.structure ppf
end

module Enum_pp = struct

  let contiguous_range =
    let rec range first current = function
      | [] -> Some(min first current, max first current)
      | (_, T.Abs n) :: q when abs(current - n) = 1 ->
        range first n q
      | _ -> None in
    function
    | [] -> None
    | (_, T.Abs n) :: q -> range n n q
    | _ -> None

  type implementation = Std | Poly

  let pp_constr name_0 impl ppf (c, _ ) =
    let name = L.remove_context name_0 c in
    let name = if name = L.mu then name_0 else name in
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
      | (_, T.Abs n as c) ->
        Fmt.pf ppf "    | %a -> %d" (pp_constr name impl) c n
      | _ -> () in
    Fmt.list ~sep constr ppf constrs

  let of_int impl ppf name constrs =
    Fmt.pf ppf "\n  let of_int = function\n";
    let constr ppf = function
      | (_, T.Abs n as c) ->
        Fmt.pf ppf "    | %d -> %a" n (pp_constr name impl) c
      | _ -> () in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf "\n    | _ -> assert false\n"

  let pp pre impl ppf name constrs =
    Fmt.pf ppf
      "\n  let %spp ppf x = Printer.fprintf ppf (match x with\n" pre;
    let constr0 = pp_constr name impl in
    let constr ppf c =
      Fmt.pf ppf "    | %a -> \"%a\"" constr0 c constr0 c in
    Fmt.list ~sep constr ppf constrs;
    Fmt.pf ppf ")\n"

  let view ppf () =
    Fmt.pf ppf
      "\n  let view = Ctypes.view ~write:to_int ~read:of_int int\n"

  let view_result ppf () =
    Fmt.pf ppf
    "\n  let view = Vk__result.view ~ok:(of_int,to_int) ~error:(of_int,to_int)\n"

  let pp_result ppf () =
    Fmt.pf ppf "\n   let pp = Vk__result.pp raw_pp\n"

  let view_opt ppf () =
    Fmt.pf ppf
       "@[<hov 2> let view_opt =\
              let read x: _ option = if x = max_int then@ \
        None else@ \
        Some (of_int x) in@;\
        let write: _ option -> _ = @;\
        function@ | None -> max_int@ | Some x -> to_int x in@;\
        Ctypes.view ~write ~read int@;@]"

  let pp impl ppf (name,constrs) =
    let is_result = H.is_result_name name in
    let pre = if is_result then "raw_" else "" in
    Fmt.pf ppf "@[<v 2> module %a = struct\n" L.pp_module name;
    List.iter (fun f -> f impl ppf name constrs)
    [def; to_int; of_int; pp pre ];
    if is_result then (pp_result ppf (); view_result ppf ())
    else view ppf ();
    view_opt ppf ();
    Fmt.pf ppf "@; @]end\n";
    Fmt.pf ppf "let %a, %a_opt = %a.(view,view_opt)\n\
               type %a = %a.t\n"
      L.pp_var name  L.pp_var name L.pp_module name
      L.pp_type name L.pp_module name

end


module Result_pp = struct

  open Subresult
(*
  let pp_result ppf (ok,errors) =
    Fmt.pf ppf "%s" @@ composite_name dict ok errors
*)
  open Aster.Result

  let view m ppf name constrs =
    let constrs =
      List.map (fun name -> name, T.Abs (find name m)) constrs in
    Fmt.pf ppf "@[<v 2>module %a = struct\n" L.pp_module name;
    Enum_pp.(of_int Poly) ppf name constrs;
    Enum_pp.(to_int Poly) ppf name constrs;
    Fmt.pf ppf "@;end@]\n"

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

module Result = struct

  let pp m ppf data = Aster.Result.make m data |> Pprintast.structure ppf

end

module Record_extension = struct

  let stype ppf = Fmt.pf ppf "generated__stype__"
  let pnext ppf = Fmt.pf ppf "generated__pnext__"

  let def ppf (name,exts) =
    let sep _ppf () = () in
    let name ppf = Fmt.pf ppf "%a_extension" L.pp_type name in
    let pp_ext ppf ext = Fmt.pf ppf "@;| %a of %a Ctypes.structure Ctypes.ptr"
        L.pp_constr ext L.pp_type ext in
    Fmt.pf ppf "@[<v> \
                exception Unknown_record_extension@;\
                type %t = ..@;@]\
                type %t +=@;| No_extension%a@;\
                @]
               "
      name name
      (Fmt.list ~sep pp_ext) exts

  let extract ppf (name,exts) =
    let pp_ext ppf ext =
      Fmt.pf ppf "| %a x ->@;\
                  Structure_type.%a,@;\
                  Ctypes.( coerce (ptr %a) (ptr void) x )@;"
        L.pp_constr ext L.pp_constr ext L.pp_type ext in
    Fmt.pf ppf
    "@[<v 2>let %t, %t = match arg__ext with@;\
     | No_extension -> Structure_type.%a, Ctypes.null@;\
     %a\
     | _ -> raise Unknown_record_extension in@;\
      @]\
    " stype pnext L.pp_constr name
    (Fmt.list ~sep:(fun _ _ -> ()) pp_ext) exts
end

module Typexp = struct
  let rec pp degraded ppf =
    let pp ppf = pp degraded ppf in
    function
    | Ty.Const t -> pp ppf t
    | Name n ->
      Fmt.pf ppf "%a" L.pp_type n
    | Ptr Name n | Ptr Const Name n ->
      Fmt.pf ppf "(ptr %a)" L.pp_type n
    | Ptr typ -> Fmt.pf ppf "(ptr (%a))" pp typ
    | Option Name n ->
      Fmt.pf ppf "%a_opt" L.pp_type n
    | Option (Ptr typ) ->
      Fmt.pf ppf "(ptr_opt (%a))" pp typ
    | Option Array (Some Const n ,typ) ->
      Fmt.pf ppf "(array_opt %a @@@@ %a)" L.pp_var n pp typ
    | Option Array (Some (Lit n) ,typ) when not degraded ->
      Fmt.pf ppf "(array_opt %d @@@@ %a)" n pp typ
    | Option Array (_,t) -> Fmt.pf ppf "(ptr_opt (%a))" pp t
    | Option String -> Fmt.pf ppf "string_opt"
    | Option t -> Fmt.epr "Not implemented: option %a@." Ty.pp t; exit 2
    | String -> Fmt.pf ppf "string"
    | Array (Some Const n ,typ) ->
      Fmt.pf ppf "(array %a @@@@ %a)" L.pp_var n pp typ
    | Array (Some (Lit n) ,typ) when not degraded ->
      Fmt.pf ppf "(array %d @@@@ %a)" n pp typ
    | Array (_,typ) -> pp ppf (Ty.Ptr typ)
    | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
    | Handle _  ->
      failwith "Anonymous type"
    | Result {ok;bad} ->
      Result_pp.pp_type ppf (ok,bad)
    | Record_extensions _ -> Fmt.pf ppf "(ptr void)"
    (* ^FIXME^?: better typing? *)
    | FunPtr _ ->
      failwith "Not_implemented: funptr"
end

module Structured = struct

  type 'field kind =
    | Union: Ty.simple_field kind
    | Record: Ty.field kind

  let pp_kind (type a) ppf: a kind -> unit = function
    | Union -> Fmt.pf ppf "union"
    | Record -> Fmt.pf ppf "structure"

  let sfield _name ppf (field_name,typ)=
    (*Should we remove context? Field names are quite short in general
      L.remove_context name field_name *)
    let field_name =  field_name in
    Fmt.pf ppf "  let %a = field t \"%a\" %a"
      L.pp_var field_name L.pp_var field_name (Typexp.pp false) typ

  let field name ppf = function
    | Ty.Simple f -> sfield name ppf f
    | Array_f { index; array } ->
      Fmt.pf ppf "%a@;%a" (sfield name) index (sfield name) array
    | Record_extension { tag; ptr; _ } ->
      Fmt.pf ppf "%a@;%a" (sfield name) tag (sfield name) ptr

  let get_field ppf x =
    Fmt.pf ppf "Ctypes.getf record Fields.%a" L.pp_var x
  let mk_array ppf (n,x)=
    Fmt.pf ppf "Ctypes.CArray.from_ptr (%t) (%t)" x n

  let pp_index ty pp ppf = H.pp_to_int pp ppf ty

  let string s ppf = Fmt.pf ppf "%s" s
  let var x ppf = L.pp_var ppf x

  let array_index types fields = function
    | Ty.Lit n -> fun ppf -> Fmt.pf ppf "%d" n
    | Path p -> fun ppf ->
      let index ppf = get_field ppf in
      H.pp_path_full index types fields ppf p
    | _ -> raise @@ Invalid_argument "Math-expression array not yet implemented"

  let unopt = function
    | Ty.Option ty -> ty
    | ty -> ty

  let repr_name = function
    | Ty.Array_f { array = a, _ ; _ } -> fun ppf -> L.pp_var ppf a
    | Simple(n, _ ) -> fun ppf ->  L.pp_var ppf n
    | Record_extension _ -> fun ppf -> Fmt.string ppf "ext"

  let lens types fields ppf (out,field)=
    Fmt.pf ppf "@[let %t record =@ " out;
    begin match field with
      | Ty.Array_f {array= a, tya; index = i, ty } as t
        when H.is_option_f t  ->
      Fmt.pf ppf "match %a, %a with@;\
                  | %t(n), %t(a) -> Some(%a)@;\
                  | _ -> None@;"
        get_field i get_field a
        (if H.is_option ty then string "Some" else string "")
        (if H.is_option tya then string "Some" else string "")
        mk_array (pp_index (unopt ty) (string "n"), string "a")
    | Ty.Array_f  {array= x, tya; index = n, ty } ->
      Fmt.pf ppf "let %a = %a and %a = %a in@;"
      L.pp_var n get_field n L.pp_var x get_field x;
      if H.is_option tya then
        Fmt.pf ppf "may (fun x -> %a) %a@;"
        mk_array (pp_index ty (var n), string "x")
        L.pp_var x
      else
        Fmt.pf ppf "%a@;"
        mk_array (pp_index ty (var n), var x)
    | Record_extension _ -> Fmt.string ppf "assert false"
    | Simple (n, Array(Some (Lit int), Name { Name_study.main = ["char"]; _ } ))
      -> Fmt.pf ppf "Ctypes.string_from_ptr (A.start @@@@ %a) %d"
           get_field n int
    | Simple (n, Array(Some (Const s), Name { Name_study.main = ["char"]; _ } ))
      ->
      Fmt.pf ppf "Ctypes.string_from_ptr@ \
                  (Ctypes.CArray.start @@@@ %a)@ %a@]"
        get_field n L.pp_var s
    | Simple (n, Array(Some Path p, _ )) ->
      Fmt.pf ppf "let a = %a in@;"
        get_field n;
      let p' = H.type_path types fields p in
      Fmt.pf ppf "let i = %a in@;" (H.pp_path get_field) p';
      if H.final_option types fields p then
        Fmt.pf ppf "may (fun i -> %a) i@;"
          mk_array (string "i", string "a")
      else
          mk_array ppf (string "i", string "a")
    | Simple (n, Option Array(Some Path p, _ )) ->
      Fmt.pf ppf "let a = %a in@;"
        get_field n;
      let p' = H.type_path types fields p in
      Fmt.pf ppf "let i = %a in@;" (H.pp_path get_field) p';
      if H.final_option types fields p then
        Fmt.pf ppf "maybe (may (fun i a -> %a) i) a@;"
          mk_array (string "i", string "a")
      else
        Fmt.pf ppf "may (fun a -> %a) a@;"
          mk_array (string "i", string "a")
    | Simple (name, _ ) -> get_field ppf name
    end;
    Fmt.pf ppf "@]"
  let rec meta_pp_ty types ppf = function
    | Ty.Name t ->
      begin match List.assoc t types with
      | exception Not_found ->  Fmt.pf ppf "%a.pp" L.pp_module t
      | B.Type Ty.FunPtr _ -> Fmt.pf ppf "pp_abstract"
      | B.Type Ty.Bitfields _ ->
        Fmt.pf ppf "%a.pp" L.pp_module (Aster.Bitset.set_name t)
      | B.Type Ty.Union _ -> Fmt.pf ppf "pp_abstract"
      | _ -> Fmt.pf ppf "%a.pp" L.pp_module t
      end
    | Array(_, Name t) when Name_study.to_path t = ["char"] ->
      Fmt.pf ppf "pp_string"
    | Array(Some Math_expr,_t) -> (*FIXME*)
      Fmt.pf ppf "pp_abstract"
    | Array(_,t) -> Fmt.pf ppf "(pp_array %a)" (meta_pp_ty types) t
    | Option t -> Fmt.pf ppf "(pp_opt %a)" (meta_pp_ty types) t
    | Const t -> meta_pp_ty types ppf t
    | Ptr t when H.is_void t -> Fmt.pf ppf "pp_addr"
    | Ptr t -> Fmt.pf ppf "(pp_ptr %a)" (meta_pp_ty types) t
    | String -> Fmt.pf ppf "pp_string"
    | ty ->
      Fmt.epr "Not implemented: %a@." Ty.pp ty;
      raise @@ Invalid_argument "Meta pp ty not implemented"

  let metapp types ppf fields =
    let pf ppf = Fmt.pf ppf "Printer.fprintf ppf" in
    let pp_f ppf (name,ty) =
      Fmt.pf ppf {|%t"%a=%%a" %a (%a x_')|} pf L.pp_var name
        (meta_pp_ty types) ty
        L.pp_var name in
    Fmt.pf ppf {|@[<v 2> let pp ppf x_' =@;%t"@@[{@@ ";@;|} pf;
    let pp_field ppf (field:Ty.field) = match field with
      | Record_extension _ -> Fmt.pf ppf {|%t "ext=⟨unsupported⟩" |} pf
      | Array_f { array=name, (Ty.Ptr ty| Const Ptr ty | Array(_,ty));
                  index=_,tyi  } ->
        let opt x = if H.is_option tyi then Ty.Option x else x in
        pp_f ppf (name, opt @@ Array(None, ty))
     | Array_f { array=name, Ty.Option(Array(_,ty)|Ptr ty| Const Ptr ty); _} ->
        pp_f ppf (name, Option (Array(None, ty)))
      | Array_f { array=_,ty; _ } ->
        Fmt.epr "metapp: %a@." Ty.pp ty;
        assert false
      | Simple f -> pp_f ppf f in
    let sep ppf () = Fmt.pf ppf {|@;;%t ";@@ ";@;|} pf in
    Fmt.list ~sep pp_field ppf fields;
    Fmt.pf ppf {|;@;%t"}@@]"@;@]|} pf

  let def_fields (type a) (kind: a kind) types name ppf (fields: a list) =
    match kind with
      | Union ->
        Fmt.list  (sfield name) ppf fields
      | Record ->
        let lens ppf f = lens types fields ppf (repr_name f, f) in
        Fmt.pf ppf "@[<v 2> module Fields=struct@;%a@;end@;@]%a@;%a@;"
          (Fmt.list ~sep:H.nl @@ field name) fields
          (Fmt.list ~sep:H.nl lens) fields
          (metapp types) fields

  let def types kind ppf name fields =
    Fmt.pf ppf "type t@;";
    Fmt.pf ppf "type %a = t@;" L.pp_type name;
    Fmt.pf ppf "let t: t %a typ = %a \"%a\"@;"
      pp_kind kind
      pp_kind kind
      L.pp_type name
    ;
    def_fields kind types name ppf fields;
    Fmt.pf ppf "@;let () = Ctypes.seal t@;"

  let pp_make tyname ppf (fields: Ty.field list) =
    let gen = "generated__x__" in
    let prx ppf = Fmt.pf ppf "arg__" in
    let pp_arg ppf = Fmt.pf ppf "%t%a" prx L.pp_var in
    let name x = (fst x) and ty =snd in
    let set_field ppf  = function
      | Ty.Simple (f,_) ->
        Fmt.pf ppf "Ctypes.setf %s Fields.%a %a;" gen L.pp_var f pp_arg f
      | Ty.Array_f { index; array } as t when H.is_option_f t ->
        Fmt.pf ppf "@[<v 2>begin match %a with@;\
                    | None ->@;\
                      (Ctypes.setf %s Fields.%a %s;@;\
                       Ctypes.setf %s Fields.%a (Obj.magic @@@@ Ctypes.null))@;\
                    |Some %a ->\
                      (Ctypes.setf %s Fields.%a (%a);@;\
                      Ctypes.setf %s Fields.%a (%a))@;\
                   end;@]@;"
          pp_arg (name array)
          gen L.pp_var (name index)
          (if H.is_option (snd index) then "None" else "0")
          gen L.pp_var (name array)
          pp_arg (name array)
          gen L.pp_var (name index) (H.array_len pp_arg) (index,array)
          gen L.pp_var (name array)
          (H.pp_opty (ty array) @@ H.pp_start pp_arg) (name array)
      | Ty.Array_f { index; array } ->
        Fmt.pf ppf "Ctypes.setf %s Fields.%a (%a);@;\
                    Ctypes.setf %s Fields.%a (%a);"
          gen L.pp_var (name index) (H.array_len pp_arg) (index,array)
          gen L.pp_var (name array)
          (H.pp_opty (snd array) @@ H.pp_start pp_arg) (name array)
      | Ty.Record_extension { exts; _ } ->
        Fmt.pf ppf "%a@;\
                   Ctypes.setf %s Fields.next %t;@;\
                   Ctypes.setf %s Fields.s_type %t;@;\
                   " Record_extension.extract (tyname, exts)
          gen Record_extension.pnext gen Record_extension.stype
    in
    Fmt.pf ppf "@;@[let make %a@ %t=@ let %s = Ctypes.make t in@ \
                %a@;\
                Ctypes.addr %s\
                @]@;"
      Fmt.(list ~sep:cut @@ H.pp_label prx) fields
      (H.pp_terminator fields)
      gen
      Fmt.(list set_field) fields
      gen

  let pp (type a) types (kind: a kind) ppf (name, (fields: a list)) =
    Fmt.pf ppf "@[<hov 2> module %a =@ struct@;" L.pp_module name;
    def types kind ppf name fields;
    begin match kind with
      | Record ->
        let exts = H.record_extension fields in
        begin match exts with
          | Some exts -> Record_extension.def ppf (name,exts)
          | None -> ()
        end;
        pp_make name ppf fields;
      | Union -> ()
    end;
    Fmt.pf ppf "end@]@.";
    Fmt.pf ppf "let %a = %a.t\n\
                type %a = %a.t\n"
      L.pp_type name L.pp_module name
      L.pp_var name L.pp_module name

end

module Funptr = struct

  let opt = "funptr_opt"
  let direct = "funptr"

  let pp_ty ppf (fn:Ty.fn) =
    Fmt.pf ppf "%a@ @->@ returning %a"
      (Fmt.list ~sep:arrow (Typexp.pp true))
      (List.map snd @@ Ty.flatten_fn_fields fn.args)
      (* Composite fields does not make sense here *)
      (Typexp.pp true) fn.return

  let pp_old  ppf (tyname, (fn:Ty.fn)) =
    match List.map snd @@ Ty.flatten_fn_fields fn.args with
    | [] ->
      Fmt.pf ppf "@[<hov> let %a = ptr void @]@."
        L.pp_var tyname
    | _ ->
      Fmt.pf ppf "@[<hov> let %a, %a_opt =@ \
                  let ty = %a in@ \
                  (Foreign.funptr ty,@ \
                  Foreign.funptr_opt ty)\
                  @]@."
        L.pp_var tyname L.pp_var tyname
        pp_ty fn

  let pp ppf x =
    let ast = Aster.Funptr.make x in
    Pprintast.structure ppf [ast]

end

module Fn = struct


  let pp_raw ppf (fn:Ty.fn) =
    let args' = match List.map snd @@ Ty.flatten_fn_fields fn.args with
      | [] -> [Ty.Name (L.simple ["void"])]
      | l -> l in
    Fmt.pf ppf "@[<v 2>let %a =\n\
                foreign@ \"%s\"@ (%a@ @->@ returning %a)@]@."
      L.pp_var fn.name fn.original_name
      (Fmt.list ~sep:arrow (Typexp.pp true)) args'
      (Typexp.pp true) fn.return

  exception Not_implemented

  let rec ptr_to_name = function
    | Ty.Name t -> Some(0, t)
    | Ty.Ptr p | Array(_,p) ->
      begin match ptr_to_name p with
      | Some(n,elt) -> Some(n+1,elt)
      | None -> None
      end
    | _ -> None

  let is_ptr_to_name n = not (ptr_to_name n = None)

  let rec last_pointer ppf (n,x) =
    if n < 1 then assert false
    else if n = 1 then
      L.pp_type ppf x
    else
      Fmt.pf ppf "ptr @@@@ %a" last_pointer (n-1,x)

  let nullptr ppf t =
    match ptr_to_name t with
    | Some x ->
      Fmt.pf ppf "nullptr (%a)" last_pointer x
    | None -> ()

  let allocate n ppf elt =
    let allocate ppf x =
      Fmt.pf ppf "Ctypes.allocate_n (%a) %t" last_pointer x n in
    let t =
      match elt with
      | Ty.Option t -> t
      | t -> t in
    match ptr_to_name t with
    | Some x -> H.pp_opty elt allocate ppf x
    | None -> Fmt.epr "Error, elt:%a@." Ty.pp elt

  let pp_some pp ppf = Fmt.pf ppf "Some (%a)" pp
  let pp_direct pp ppf = Fmt.pf ppf "%a" pp

  let extract_opt def arg pp_extract ppf y =
    Fmt.pf ppf "let %t = match %t with@;\
                | None -> None, Obj.magic(Ctypes.null) @;\
                | Some %t -> %a in @;"
      def arg arg pp_extract y

  let pp_len var ppf (x,_) = Fmt.pf ppf "Ctypes.CArray.length %a" var x

  let extract_array var ppf (index,array) =
    Fmt.pf ppf "%a, %a"
      (H.opt @@ pp_len var) (fst array, snd index)
      (H.pp_start @@ H.pp_fname var) array

  let result ppf inp seq s =
    Fmt.pf ppf "@[<v 2>match %t with@;\
                | Error _ as e -> e@;\
                | Ok %t ->@;@[<v 2> begin@;%aend@]@;\
                @]" inp inp seq s

  let zip inp pp_out ppf out = Fmt.pf ppf "Ok(%t,%a)" inp pp_out out

  let one ppf = Fmt.pf ppf "%d" 1

  let pp_smart types ppf (fn:Ty.fn) =
    let input, output = List.partition
        (fun {Ty.dir; _ } -> dir = In || dir = In_Out) fn.args in
    let all = List.map fst @@ Ty.flatten_fn_fields fn.args in
    let space ppf () = Fmt.pf ppf "@ " in
    let list x = Fmt.list ~sep:space x in
    let prx ppf = Fmt.string ppf "gen__" in
    let var_out ppf x = Fmt.pf ppf "%t%a" prx L.pp_var x in
    let var_in = var_out in
    let var_all = var_out in
    let pp_size ppf = Fmt.pf ppf "size__%a" L.pp_var in
    let pp_label = H.pp_fnlabel prx in
    let def ppf =
      Fmt.pf ppf "@[<v 2> let %a %a %t@ =@;" L.pp_var fn.name
        (list pp_label) input
        (H.pp_terminator @@ List.map (fun x -> x.Ty.field) input) in
    let out_def ppf (f:Ty.fn_field) =
      match f.field with
      | Simple(f, Array(Some(Path p), Name elt)) ->
        Fmt.pf ppf "let %a = %a in@;" pp_size f
          (H.pp_path_full var_in types @@ H.to_fields fn.args) p;
        Fmt.pf ppf "let %a = Ctypes.allocate_n (%a) %a in @;"
          var_out f L.pp_type elt pp_size f
      | Ty.Simple (f, t) when is_ptr_to_name t  ->
        Fmt.pf ppf "let %a = %a in@;" var_out f (allocate one) t
      | Array_f { array=a, elt; index=i, size }
        when is_ptr_to_name elt && is_ptr_to_name size ->
        Fmt.pf ppf "let %a = %a in@;\
                    let %a = %a in@;"
          var_out i (allocate one) size
          var_out a nullptr elt
      | Array_f { array=a, Option _; index=i, Ptr Option Name t } ->
        Fmt.pf ppf "let %a = Ctypes.allocate %a_opt None in@;\
                    let %a = None in@;"
          var_out i L.pp_type t
          var_out a
      | _ ->
        Fmt.epr "Smart function not implemented for: %a@." Ty.pp_fn_field f;
        raise Not_implemented in
    let out_redef ppf (f:Ty.fn_field) = match f.field with
      | Array_f { array = a, elt; index = i, it  } ->
        let var ppf = var_out ppf i in
        let size ppf = Fmt.pf ppf "(%a)" (H.pp_to_int var) it in
        Fmt.pf ppf "let %a = %a in@;"
          var_out a (allocate size) elt
      | _ -> () in
    let in_expand ppf (f:Ty.fn_field) = match f.field with
      | Array_f { array= (a, _ as array) ; index = (i, _ as index)  } as f ->
        let arg ppf = var_in ppf a in
        let def ppf = Fmt.pf ppf "%a,%a" var_in i var_in a in
        if H.is_option_f f then
          extract_opt def arg (extract_array var_in) ppf (index,array)
        else
          Fmt.pf ppf "let %t = %a in@;"
            def
            (extract_array var_in) (index, array)
      | Simple(name, Array(Some _, _)) ->
        Fmt.pf ppf "let %a = Ctypes.CArray.start %a in@;" var_in name var_in name
      | _ -> () in
    let apply_twice = List.exists
        (function { Ty.field = Array_f _ ; _ } -> true | _ -> false) output in
    let resname ppf = Fmt.pf ppf "generated__res__" in
    let res_pat ppf = if H.is_void fn.return then Fmt.string ppf "_"
      else resname ppf in
    let void ppf = Fmt.string ppf "()" in
    let res_val = if H.is_void fn.return then void else resname in
    let apply ppf =
      Fmt.pf ppf "let %t = %a %a in@;"
        res_pat L.pp_var fn.name (list var_all) all in
    let comma ppf () = Fmt.pf ppf "," in
    let out_result ppf f = match f.Ty.field with
      | Ty.Array_f { array = (n, Ty.Option _) ; index = i , it } ->
        Fmt.pf ppf "Ctypes.CArray.from_ptr (unwrap %a) (%a)"
          var_out n (H.pp_to_int @@ fun ppf -> var_out ppf i) it
      | Ty.Array_f { array = (n,_) ; _ } -> var_out ppf n
      | Simple(f, Array(Some(Path _), Name _ )) ->
        Fmt.pf ppf "(Ctypes.CArray.from_ptr %a %a)"
          var_out f pp_size f
      | Simple(n, _) -> Fmt.pf ppf "Ctypes.(!@@)(%a)" var_out n
      | Record_extension _ ->
        raise @@ Invalid_argument "Record extension used as a function argument"
    in
    def ppf;
    List.iter (in_expand ppf) input;
    Fmt.list out_def ppf output;
    apply ppf;
    let with_out =  List.length output > 0 in
    let pp_out = Fmt.list ~sep:comma out_result in
    let pp_redef ppf output =  Fmt.list out_redef ppf output; apply ppf in
    if not (H.is_result fn.return) || not with_out then
      begin
        if apply_twice then
          pp_redef ppf output;
        if with_out && H.is_void fn.return then
          Fmt.pf ppf "%a" pp_out output
        else if with_out then
          Fmt.pf ppf "%t,%a" res_val pp_out output
        else if H.is_result fn.return then
          result ppf res_val (zip res_pat @@ fun x () -> void x) ()
        else
          res_val ppf
      end
    else
      begin
        if not apply_twice then
          result ppf res_val (zip res_pat pp_out) output
        else
          result ppf res_val (fun ppf out ->
              pp_redef ppf out;
              result ppf res_val (zip res_pat pp_out) out
            ) output
      end;
    Fmt.pf ppf "@]@."


    let pp types kind = if kind then pp_raw else pp_smart types

end

module DFn = struct

  let pp ppf (fn:Ty.fn) =
    Fmt.pf ppf "@[<hov>let %a =\n\
                get \"%s\" (Foreign.funptr %a) @]@."
      L.pp_var fn.name
      fn.original_name Funptr.pp_ty fn
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
    | "bits" :: "flag" ::  q -> "flags" :: q
    | a :: q -> a :: remove_bits q
  in
  L.{ name with postfix = remove_bits name.postfix }

let is_bits name =
  match name.L.postfix with
  | "bits" :: _  -> true
  | _ -> false

let pp_alias builtins ppf (name,origin) =
  if not @@ B.Name_set.mem name builtins then
    Fmt.pf ppf
      "@[<hov 2> module %a = Alias(%a)@]@. \
       @[let %a = %a.ctype@]@.\
       @[let %a_opt = integer_opt (module %a)@]@."
      L.pp_module name
      L.pp_module origin
      L.pp_var name
      L.pp_module name
      L.pp_var name
      L.pp_module name

(*       type t = %a@ \
       let ctype = %a@ \
       let of_int = %a.of_int@ \
       let to_int = %a.to_int@ \
       end)@]@.\
       @[let %a = %a.ctype@]@.
      L.pp_module name
      L.pp_type origin
      L.pp_var origin
      L.pp_module origin
      L.pp_module origin
      L.pp_var name
      L.pp_module name
    *)
let pp_type builtins results types ppf (name,ty) =
  match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> ()
  | Result {ok;bad} -> Result.pp results ppf (name,ok,bad)
  | Name t -> pp_alias builtins ppf (name,t)
  | FunPtr fn -> Funptr.pp ppf (name,fn)
  | Union fields -> Structured.(pp types Union) ppf (name,fields)
  | Bitset { field_type = Some _; _ } -> ()
  | Bitset { field_type = None; _ } -> Bitset.pp ppf (name,None)
  | Bitfields {fields;values} ->
    Bitset.pp_with_bits ppf (name,(fields,values))
  | Handle _ ->  Handle.pp ppf name
  | Enum constrs ->
    if not @@ is_bits name then
      begin
        let is_result = name.main = ["result"] in
        let kind = if is_result then Aster.Enum.Poly else Aster.Enum.Std in
        Enum.pp kind ppf (name,constrs)
      end
  | Record r ->
    Structured.(pp types Record) ppf (name,r.fields)
  | Record_extensions _ -> (* FIXME *)
    Fmt.pf ppf "(ptr void)"

let pp_item (lib:B.lib) ppf (name, item) =
  let types = (B.find_submodule "types" lib).sig' in
  match item with
  | B.Type t -> pp_type lib.builtins lib.result types ppf (name,t)
  | Const c -> Const.pp ppf (name,c)
  | Fn f -> Fn.pp types  f.simple ppf f.fn


let pp_open ppf m =
  if m.B.args = [] then
    Fmt.pf ppf "open %s@." (String.capitalize_ascii m.B.name)

let rec pp_module lib ppf (m:B.module') =
  Fmt.pf ppf
    "@[<v 2>module %s%a= struct@;%a@;end@]@;%a@;"
    (String.capitalize_ascii m.name)
    pp_args m.args
    (pp_sig lib) m
    pp_open m
and pp_sig lib ppf (m:B.module') =
    Fmt.pf ppf "%a@;%a@;"
      (Fmt.list @@ pp_module lib) m.submodules
      (Fmt.list @@ pp_item lib) m.sig'

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
    if not (B.is_empty m) then
      begin
        let ppf = open_file ("vk__" ^ m.name ^ ".ml") in
        Fmt.pf ppf "%a\n%a%!"
          pp_preambule m
          (pp_sig lib) m
      end in
  List.iter pp_sub lib.content.submodules
