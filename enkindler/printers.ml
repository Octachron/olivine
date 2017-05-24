module B = Lib_builder
module T = B.T
module Ty = B.Ty
module L = Name_study
module Arith = Ctype.Arith

let sep ppf () = Fmt.pf ppf "\n"
let arrow ppf () = Fmt.pf ppf "@ @->"

module H = struct

  let is_option = function
    | Ty.Option _ -> true
    | _ -> false

  let is_ptr_option = function
    | Ty.Ptr Option _ -> true
    | _ -> false

  let is_option_f = function
    | Ty.Simple (_, (Option _ | Const Option _) )
    | Ty.Array_f { index = _, (Option _  | Const Option _ ); _ }
    | Ty.Record_extension _ -> true
    | _ -> false


  let size_t = Name_study.{ mu with main = [word "size"; word "t"]}
  (* FIXME *)

  let rec opt pp ppf (n, typ as f) = match typ with
    | Ty.Option typ  -> Fmt.pf ppf "Some(%a)" (opt pp) (n,typ)
    | Name x ->
      Fmt.pf ppf  "%a.of_int(%a)" L.pp_module x pp f
    | _ -> pp ppf f

  let array_len prefix ppf ((_,typ), (name,_)) =
    let len ppf (n,_) = Fmt.pf ppf "Ctypes.CArray.length %t%a" prefix L.pp_var n in
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

  let pp_fname ppf (n, _ ) = L.pp_var ppf n

  let is_result_name = function
    | {Name_study.main = ["result", _]; prefix=[]; postfix = [] } ->
      true
    | _ -> false

  let is_void = function
    | Ty.Name {Name_study.main = ["void", _]; prefix=[]; postfix = [] } ->
      true
    | _ -> false

  let is_result = function
    | Ty.Result _ -> true
    | _ -> false

end

module Enum = struct

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

  let view_result ppf () =
    Fmt.pf ppf
    "\n  let view = Vk__result.view ~ok:(of_int,to_int) ~error:(of_int,to_int)\n"

  let view_opt ppf () =
    Fmt.pf ppf
       "\n  let view_opt =\
              let read x: _ option = if x = max_int then None else Some x in\n\
              let write: _ option -> _ = \n\
                function None -> max_int | Some x -> x in\n\
              Ctypes.view ~write ~read int\n"

  let pp impl ppf (name,constrs) =
    Fmt.pf ppf "@[<v 2> module %a = struct\n" L.pp_module name;
    List.iter (fun f -> f impl ppf name constrs)
    [def; to_int; of_int; pp ];
    if H.is_result_name name then view_result ppf () else view ppf ();
    view_opt ppf ();
    Fmt.pf ppf "@; @]end\n";
    Fmt.pf ppf "let %a, %a_opt = %a.(view,view_opt)\n\
               type %a = %a.t\n"
      L.pp_var name  L.pp_var name L.pp_module name
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
      List.map (fun name -> name, T.Abs (find name m)) constrs in
    Fmt.pf ppf "@[<v 2>module %a = struct\n" L.pp_module name;
    Enum.(of_int Poly) ppf name constrs;
    Enum.(to_int Poly) ppf name constrs;
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
  let rec pp ppf = function
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
    | Option Array (_,t) -> Fmt.pf ppf "(ptr_opt (%a))" pp t
    | Option String -> Fmt.pf ppf "string_opt"
    | Option t -> Fmt.epr "Not implemented: option %a@." Ty.pp t; exit 2
    | String -> Fmt.pf ppf "string"
    | Array (Some Const n ,typ) ->
      Fmt.pf ppf "(array %a @@@@ %a)" L.pp_var n pp typ
    | Array (_,typ) -> pp ppf (Ty.Ptr typ)
    | Enum _ | Record _ | Union _ | Bitset _ | Bitfields _
    | Handle _  ->
      failwith "Anonymous type"
    | Result {ok;bad} ->
      Result.pp_type ppf (ok,bad)
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

  let sfield name ppf (field_name,typ)=
    let field_name = L.remove_context name field_name in
    Fmt.pf ppf "  let %a = field t \"%a\" %a"
      L.pp_var field_name L.pp_var field_name Typexp.pp typ

  let field name ppf = function
    | Ty.Simple f -> sfield name ppf f
    | Array_f { index; array } ->
      (* FIXME: WHY??? *)
      Fmt.pf ppf "%a@;%a" (sfield name) index (sfield name) array
    | Record_extension { tag; ptr; _ } ->
      Fmt.pf ppf "%a@;%a" (sfield name) tag (sfield name) ptr

  let def (type a) (kind: a kind) ppf name (fields:a list) =
    Fmt.pf ppf "type t@;";
    Fmt.pf ppf "type %a = t@;" L.pp_type name;
    Fmt.pf ppf "let t: t %a typ = %a \"%a\"@;"
      pp_kind kind
      pp_kind kind
      L.pp_type name
    ;
    begin match kind with
      | Union ->
        Fmt.list (sfield name) ppf fields
      | Record ->
        Fmt.list  (field name) ppf fields
    end;
    Fmt.pf ppf "@;let () = Ctypes.seal t@;"

  let pp_make name ppf (fields: Ty.field list) =
    let gen = "generated__x__" in
    let prx ppf = Fmt.pf ppf "arg__" in
    let pp_arg ppf = Fmt.pf ppf "%t%a" prx L.pp_var in
    let set_field ppf  = function
      | Ty.Simple (f,_) ->
        Fmt.pf ppf "Ctypes.setf %s %a %a;" gen L.pp_var f pp_arg f
      | Ty.Array_f { index; array } as t when H.is_option_f t ->
        Fmt.pf ppf "@[<v 2>begin match %a with@;\
                    | None ->@;\
                      (Ctypes.setf %s %a None;@;\
                       Ctypes.setf %s %a (Obj.magic @@@@ Ctypes.null))@;\
                    |Some %a ->\
                      (Ctypes.setf %s %a (%a);@;\
                      Ctypes.setf %s %a (%a))@;\
                   end;@]@;"
          pp_arg (fst array)
          gen L.pp_var (fst index)
          gen L.pp_var (fst array)
          pp_arg (fst array)
          gen L.pp_var (fst index) (H.array_len prx) (index,array)
          gen L.pp_var (fst array)
          (H.pp_opty (snd array) @@ H.pp_start pp_arg) (fst array)
      | Ty.Array_f { index; array } ->
        Fmt.pf ppf "Ctypes.setf %s %a (%a);@;\
                    Ctypes.setf %s %a (%a);"
          gen L.pp_var (fst index) (H.array_len prx) (index,array)
          gen L.pp_var (fst array)
          (H.pp_opty (snd array) @@ H.pp_start pp_arg) (fst array)
      | Ty.Record_extension { exts; _ } ->
        Fmt.pf ppf "%a@;\
                   Ctypes.setf %s p_next %t;@;\
                   Ctypes.setf %s s_type %t;@;\
                   " Record_extension.extract (name, exts)
          gen Record_extension.pnext gen Record_extension.stype
    in
    Fmt.pf ppf "@;@[let make %a@ %t=@ let %s = Ctypes.make t in@ \
                %a@;\
                Ctypes.addr %s\
                @]@;"
      Fmt.(list @@ H.pp_label prx) fields
      (H.pp_terminator fields)
      gen
      Fmt.(list set_field) fields
      gen

  let pp (type a) (kind: a kind) ppf (name, (fields: a list)) =
    Fmt.pf ppf "@[<hov 2> module %a =@ struct@;" L.pp_module name;
    def kind ppf name fields;
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
      Fmt.pf ppf "if mem %a set then\n\
                    Printer.fprintf ppf \"%a;@@ \"\n\
                  else ()"
        L.pp_var name' L.pp_var name' in
    let sep ppf () = Fmt.pf ppf ";\n" in
    Fmt.pf ppf "@[<v 2> let pp ppf set=@;\
                Printer.fprintf ppf \"@@[{\";@;\
                %a;@;\
                Printer.fprintf ppf \"}@@]\"@;"
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
      { name with postfix = List.filter (fun (x,_) -> x <> "flags") name.postfix }
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

module Handle = struct
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

module Funptr = struct

  let opt = "funptr_opt"
  let direct = "funptr"

  let pp_ty ppf (fn:Ty.fn) =
    Fmt.pf ppf "%a@ @->@ returning %a"
      (Fmt.list ~sep:arrow Typexp.pp)
      (List.map snd @@ Ty.flatten_fn_fields fn.args)
      (* Composite fields does not make sense here *)
      Typexp.pp fn.return

  let pp  ppf (tyname, (fn:Ty.fn)) =
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

end

module Fn = struct


  let pp_raw ppf (fn:Ty.fn) =
    let args' = match List.map snd @@ Ty.flatten_fn_fields fn.args with
      | [] -> [Ty.Name (L.simple [L.word "void"])]
      | l -> l in
    Fmt.pf ppf "@[<v 2>let %a =\n\
                foreign@ \"%s\"@ (%a@ @->@ returning %a)@]@."
      L.pp_var fn.name (L.original fn.name)
      (Fmt.list ~sep:arrow Typexp.pp) args'
      Typexp.pp fn.return

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

  let pp_len ppf (x,_) = Fmt.pf ppf "Ctypes.CArray.length %a" L.pp_var x

  let extract_array ppf (index,array) =
    Fmt.pf ppf "%a, %a"
      (H.opt pp_len) (fst array, snd index) (H.pp_start H.pp_fname) array

  let result ppf inp seq s =
    Fmt.pf ppf "@[<v 2>match %t with@;\
                | Error _ as e -> e@;\
                | Ok %t ->@;@[<v 2> begin@;%aend@]@;\
                @]" inp inp seq s

  let zip inp pp_out ppf out = Fmt.pf ppf "Ok(%t,%a)" inp pp_out out

  let one ppf = Fmt.pf ppf "%d" 1

  let rec pp_to_int var ppf = function
    | Ty.Ptr t ->
      let var ppf = Fmt.pf ppf "Ctypes.(!@@)(%t)" var in
      pp_to_int var ppf t
    | Option t ->
      let var ppf = Fmt.pf ppf "unwrap(%t)" var in
      pp_to_int var ppf t
    | Name { Name_study.main = ["uint32",_; "t", _ ] ; prefix =[]; postfix = [] }
      -> var ppf
    | Name t -> Fmt.pf ppf "%a.to_int (%t)" L.pp_module t var
    | _ -> raise @@ Invalid_argument "Invalid Printers.Fn.to_int ty"

  let pp_smart ppf (fn:Ty.fn) =
    let input, output = List.partition
        (fun {Ty.dir; _ } -> dir = In || dir = In_Out) fn.args in
    let all = List.map fst @@ Ty.flatten_fn_fields fn.args in
    let space ppf () = Fmt.pf ppf "@ " in
    let list x = Fmt.list ~sep:space x in
    let prx _ppf = () in
    let pp_label = H.pp_fnlabel prx in
    let def ppf =
      Fmt.pf ppf "@[<v 2> let %a @[<hov>%a%t@]=@;" L.pp_var fn.name
        (list pp_label) input
        (H.pp_terminator @@ List.map (fun f -> f.Ty.field) fn.args) in
    let out_def ppf (f:Ty.fn_field) =
      match f.field with
      | Ty.Simple (f, t) when is_ptr_to_name t  ->
        Fmt.pf ppf "let %a = %a in@;" L.pp_var f (allocate one) t
      | Simple(f, Array(Some(Var i), Name elt)) ->
        Fmt.pf ppf "let %a = Ctypes.allocate_n (%a) (%a) in @;"
        L.pp_var f L.pp_type elt L.pp_var i
      | Array_f { array=a, elt; index=i, size }
        when is_ptr_to_name elt && is_ptr_to_name size ->
        Fmt.pf ppf "let %a = %a in@;\
                    let %a = %a in@;"
          L.pp_var i (allocate one) size
          L.pp_var a nullptr elt
      | Array_f { array=a, Option _; index=i, Ptr Option Name t } ->
        Fmt.pf ppf "let %a = Ctypes.allocate %a_opt None in@;\
                    let %a = None in@;"
          L.pp_var i L.pp_type t
          L.pp_var a
      | _ ->
        Fmt.epr "Smart function not implemented for: %a@." Ty.pp_fn_field f;
        raise Not_implemented in
    let out_redef ppf (f:Ty.fn_field) = match f.field with
      | Array_f { array = a, elt; index = i, it  } ->
        let var ppf = L.pp_var ppf i in
        let size ppf = Fmt.pf ppf "(%a)" (pp_to_int var) it in
        Fmt.pf ppf "let %a = %a in@;"
          L.pp_var a (allocate size) elt
      | _ -> () in
    let in_expand ppf (f:Ty.fn_field) = match f.field with
      | Array_f { array= (a, _ as array) ; index = (i, _ as index)  } as f ->
        let arg ppf = L.pp_var ppf a in
        let def ppf = Fmt.pf ppf "%a,%a" L.pp_var i L.pp_var a in
        if H.is_option_f f then
          extract_opt def arg extract_array ppf (index,array)
        else
          Fmt.pf ppf "let %t = %a in @;"
            def
            extract_array (index, array)
      | _ -> () in
    let apply_twice = List.exists
        (function { Ty.field = Array_f _ ; _ } -> true | _ -> false) output in
    let res ppf = Fmt.pf ppf "generated__res__" in
    let apply ppf =
      Fmt.pf ppf "let %t = %a %a in@;"
        res L.pp_var fn.name (list L.pp_var) all in
    let comma ppf () = Fmt.pf ppf "," in
    let out_result ppf f = match f.Ty.field with
      | Ty.Array_f { array = (n, Ty.Option _) ; index = i , it } ->
        Fmt.pf ppf "Ctypes.CArray.from_ptr (unwrap %a) (%a)"
          L.pp_var n (pp_to_int @@ fun ppf -> L.pp_var ppf i) it
      | Ty.Array_f { array = (n,_) ; _ } -> L.pp_var ppf n
      | Simple(f, Array(Some(Var i), Name _ )) ->
        Fmt.pf ppf "(Ctypes.CArray.from_ptr %a %a)"
          L.pp_var f L.pp_var i
      | Simple(n, _) -> Fmt.pf ppf "Ctypes.(!@@ %a)" L.pp_var n
      | Record_extension _ ->
        raise @@ Invalid_argument "Record extension used as a function argument"
    in
    def ppf;
    List.iter (in_expand ppf) input;
    Fmt.list out_def ppf output;
    apply ppf;
    let with_out =  List.length output > 0 in
    let pp_out =  Fmt.list ~sep:comma out_result in
    let pp_redef ppf output =  Fmt.list out_redef ppf output; apply ppf in
    if not (H.is_result fn.return) || not with_out then
      begin
        if apply_twice then
          pp_redef ppf output;
        if with_out && H.is_void fn.return then
          Fmt.pf ppf "%a" pp_out output
        else if with_out then
          Fmt.pf ppf "%t,%a" res pp_out output
        else
          res ppf
      end
    else
      begin
        if not apply_twice then
          result ppf res (zip res pp_out) output
        else
          result ppf res (fun ppf out ->
              pp_redef ppf out;
              result ppf res (zip res pp_out) out
            ) output
      end;
    Fmt.pf ppf "@]@."


    let pp kind = if kind then pp_raw else pp_smart

end

module DFn = struct

  let pp ppf (fn:Ty.fn) =
    Fmt.pf ppf "@[<hov>let %a =\n\
                get \"%s\" (Foreign.funptr %a) @]@."
      L.pp_var fn.name
      (L.original fn.name) Funptr.pp_ty fn
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

let pp_alias builtins ppf (name,origin) =
  if not @@ B.Name_set.mem name builtins then
    Fmt.pf ppf
      "@[<hov 2> module %a = Alias(struct@ \
       type t = %a@ \
       let ctype = %a@ \
       let of_int = %a.of_int@ \
       let to_int = %a.to_int@ \
       end)@]@.\
       @[let %a = %a.ctype@]@."
      L.pp_module name
      L.pp_type origin
      L.pp_var origin
      L.pp_module origin
      L.pp_module origin
      L.pp_var name
      L.pp_module name

let pp_type builtins results ppf (name,ty) =
  match ty with
  | Ty.Const _  | Option _ | Ptr _ | String | Array (_,_) -> ()
  | Result {ok;bad} -> Result.pp results ppf (name,ok,bad)
  | Name t -> pp_alias builtins ppf (name,t)
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
  | Record_extensions _ -> (* FIXME *)
    Fmt.pf ppf "(ptr void)"

let pp_item builtins results ppf (name, item) =
  match item with
  | B.Type t -> pp_type builtins results ppf (name,t)
  | Const c -> Const.pp ppf (name,c)
  | Fn f -> Fn.pp f.simple ppf f.fn


let pp_open ppf m =
  if m.B.args = [] then
    Fmt.pf ppf "open %s@." (String.capitalize_ascii m.B.name)

let rec pp_module builtins results ppf (m:B.module') =
  Fmt.pf ppf
    "@[<v 2>module %s%a= struct@;%a@;end@]@;%a@;"
    (String.capitalize_ascii m.name)
    pp_args m.args
    (pp_sig builtins results) m
    pp_open m
and pp_sig builtins results ppf (m:B.module') =
    Fmt.pf ppf "%a@;%a@;"
      (Fmt.list @@ pp_module builtins results) m.submodules
      (Fmt.list @@ pp_item builtins results) m.sig'

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
          (pp_sig lib.builtins lib.result) m
      end in
  List.iter pp_sub lib.content.submodules
