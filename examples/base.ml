module A = Ctypes.CArray

let get, set = Ctypes.(getf,setf)
let null = Ctypes.null

let ($=) field value str= set str field value
let ( ^ ) = get

let make typ updates =
  let str=Ctypes.make typ in
  List.iter (fun f -> f str) updates;
  str

let layers = Ctypes.allocate_n ~count:0 Ctypes.string
let extensions = Ctypes.allocate_n ~count:0 Ctypes.string

let () =
  let _info =
    make Vk.instance_create_info
      Vk.Instance_create_info.[
        s_type $= Vk.Structure_type.Instance_create_info;
        p_next $= null;
        flags $= Vk.Instance_create_flags.empty;
        p_application_info $= None;
        enabled_layer_count $= Unsigned.UInt32.of_int 0;
        pp_enabled_layer_names $= layers;
        enabled_extension_count $= Unsigned.UInt32.of_int 0;
        pp_enabled_extension_names $= extensions;
      ] in
  ()
