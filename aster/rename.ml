module L = Info.Linguistic
module Cty = Info.Refined_types.Ty

module Full_name = struct
  type name = L.name
  let pp ppf x = L.pp_var ppf x
end
module Ty = Info.Refined_types.Typexpr(Full_name)



let may f = function
  | None -> None
  | Some x -> Some (f x)

let elt dict = L.make dict
let rec typ (!) x =
  let typ = typ (!) in
  match x with
  | Cty.Name n -> Ty.Name !n
  | Const ty -> Ty.Const(typ ty)
  | Ptr ty -> Ty.Ptr(typ ty)
  | Option ty  -> Ty.Option(typ ty)
  | String -> String
  | Handle p ->
    Ty.Handle { parent = may (!) p.parent; dispatchable=p.dispatchable }
  | Array (cexpr, t) -> Ty.Array( may (const (!)) cexpr, typ t)
  | FunPtr f -> Ty.FunPtr (fn (!) f)
  | Enum constrs -> Ty.Enum(List.map (constr(!)) constrs)
  | Union f -> Ty.Union (sfields (!) f)
  | Record r ->
    Ty.Record{ is_private = r.is_private; fields = fields (!) r.fields }
  | Bitset b ->
    Ty.Bitset { implementation = ! (b.implementation);
                field_type = may (!) b.field_type }
  | Bitfields b ->
    Ty.Bitfields { fields = List.map (bitfield (!)) b.fields;
                   values =  List.map (bitfield (!)) b.values
                 }
  | Result r -> Ty.Result{ ok = List.map (!) r.ok;
                           bad = List.map (!) r.bad }
  | Record_extensions l ->
    Ty.Record_extensions (record_extension (!) l)
  | Width { size; ty } -> Width { size; ty = typ ty }
and const (!) = function
  | Cty.Lit a -> Ty.Lit a
  | Path p -> Ty.Path (List.map (!) p)
  | Const { name;factor} -> Ty.Const { factor; name = !name }
  | Null_terminated -> Ty.Null_terminated
  | Math_expr x -> Ty.Math_expr (Info.Refined_types.rename (!) x)
and fn (!) {Cty.args; name; original_name; return } =
  Ty.{ args = fn_fields (!) args;
       name = ! name; original_name;
       return = typ (!) return }
and bitfield (!) (name, n) = !name, n
and sfield (!) (n,ty) = !n, typ (!) ty
and field (!) = function
  | Cty.Simple f -> Ty.Simple(sfield (!) f)
  | Array_f r ->
    Array_f { index = sfield (!) r.index;
              array = sfield (!) r.array }
  | Record_extension { exts; tag; ptr } ->
    Record_extension {
      exts = record_extension (!) exts;
      tag = sfield (!) tag;
      ptr = sfield (!) ptr
    }
and fn_field (!) (r:Cty.fn_field) =
  { Ty.dir = r.dir; field = field (!) r.field }
and sfields (!) = List.map @@ sfield (!)
and fields (!) = List.map @@ field (!)
and fn_fields (!) = List.map @@ fn_field (!)
and constr (!) (n, p) = !n, p
and record_extension (!) l =
  List.filter (fun x -> not @@ Sys_info.is_specific x)
  @@ List.map (!) l
