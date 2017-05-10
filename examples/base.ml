module A = Ctypes.CArray

let get, set = Ctypes.(getf,setf)
let null = Ctypes.null

let ($=) field value str= set str field value
let ( % ) = get

let make typ updates =
  let str=Ctypes.make typ in
  List.iter (fun f -> f str) updates;
  str

let mk_ptr typ updates = Ctypes.addr @@ make typ updates

let (<?>) x s = match x with
  | Ok _ -> Format.printf "Success: %s@." s
  | Error k ->
    Format.eprintf "Error %a: %s @."
      Vk.Result.pp k s; exit 1

let (<?>*) x s = match x with
  | Ok x -> Format.printf "SDL:Success: %s@." s; x
  | Error _ ->
    Format.eprintf "SDL:Error: %s @." s; exit 1

let (!) = Ctypes.(!@)
let (~:) = Unsigned.UInt32.of_int
let to_int = Unsigned.UInt32.to_int

open Tsdl
let () = Sdl.(init Init.(video + events)) <?>* "Sdl init"

let w =
  Sdl.create_window "Vulkan + SDL test" ~w:512 ~h:512
    Sdl.Window.(allow_highdpi) <?>* "Window creation"

let () = Sdl.show_window w
let e = Sdl.Event.create ()
let rec event_loop e =
  let open Sdl.Event in
  if
    Sdl.poll_event @@ Some e
    && get e typ = key_down
    && get e keyboard_keycode = Sdl.K.escape
  then
        exit 0
  else event_loop e

let debug fmt = Format.printf ("Debug: " ^^ fmt ^^ "@.")

let layers = Ctypes.allocate_n ~count:0 Ctypes.string

let (+@) = Ctypes.(+@)
let ( <-@ ) = Ctypes.( <-@ )
let extensions =
  let e = Ctypes.allocate_n ~count:2 Ctypes.string in
  e <-@ "VK_KHR_surface";
  (e +@ 1) <-@ "VK_KHR_xlib_surface";
  e

let info =
  make Vk.instance_create_info
    Vk.Instance_create_info.[
      s_type $= Vk.Structure_type.Instance_create_info;
      p_next $= null;
      flags $= Vk.Instance_create_flags.empty;
      p_application_info $= None;
      enabled_layer_count $= Unsigned.UInt32.of_int 0;
      pp_enabled_layer_names $= layers;
      enabled_extension_count $= ~:2 ;
      pp_enabled_extension_names $= extensions;
    ]

;; debug "Info created"

let instance = Ctypes.allocate_n Vk.instance 1
;; debug "Instance pointer allocated"

;; Vk.create_instance (Ctypes.addr info) None instance
   <?> "instance"

let instance = !instance

let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
let to_array n p = Array.init (to_int !n) (fun i -> !(p +@ i) )
let to_string carray =
  String.init (A.length carray)
    (fun n -> A.get carray n)

let get_array msg elt f =
  let n = Ctypes.(allocate uint32_t) ~:0 in
  msg "count" @@ f n (nullptr elt);
  let e =
    Ctypes.allocate_n ~count:(to_int !n) elt in
  msg "allocation" (f n e);
  to_array n e

let msg name minor r =
  r <?> name ^ ":" ^ minor

let silent _minor _r = ()

let extension_properties =
  get_array (msg "Extension properties") Vk.extension_properties
  @@ Vk.enumerate_instance_extension_properties ""

let print_extension_property e =
  let open Vk.Extension_properties in
  Format.printf "%s\n" (to_string @@ e % extension_name)

;; Array.iter print_extension_property extension_properties

let phy_devices =
  let d =
    get_array (msg "physical device") Vk.physical_device
    @@ Vk.enumerate_physical_devices instance in
  debug "Number of devices: %d \n" (Array.length d);
  d

let property device =
  let p = Ctypes.make Vk.physical_device_properties in
  debug "Device properties acquisition";
  Vk.get_physical_device_properties device (Ctypes.addr p);
  debug "Device properties acquired";
  p

let print_property device =
  let p = property device in
  Format.printf "Device: %s\n"
   (to_string @@ p % Vk.Physical_device_properties.device_name)

;; Array.iter print_property phy_devices

let phydevice = phy_devices.(0)

let queue_family_properties =
  get_array silent Vk.queue_family_properties
  @@ Vk.get_physical_device_queue_family_properties phydevice

let print_queue_property ppf property =
  Format.fprintf ppf "Queue flags: %a \n" Vk.Queue_flags.pp
    (property % Vk.Queue_family_properties.queue_flags)

;; Array.iter (print_queue_property Format.std_formatter)
  queue_family_properties

let queue_create_info =
  let open Vk.Device_queue_create_info in
  mk_ptr t [
    s_type $= Vk.Structure_type.Device_queue_create_info;
    p_next $= null;
    flags $= Vk.Device_queue_create_flags.empty;
    queue_family_index $= ~:0;
    queue_count $= ~:1;
    p_queue_priorities $= Ctypes.(allocate float) 1.
  ]

let device_extensions =
  get_array (msg "device extensions")
    Vk.extension_properties
    (Vk.enumerate_device_extension_properties phydevice "")

;; Format.printf "Device extensions:\n@[<v 2>"
;; Array.iter print_extension_property device_extensions
;; Format.printf "@]@."


let device =
  let d = Ctypes.allocate_n Vk.Device.t 1 in
  let info =
    let open Vk.Device_create_info in
    mk_ptr t [
      s_type $= Vk.Structure_type.Device_create_info;
      p_next $= null;
      flags $= Vk.Device_create_flags.empty;
      queue_create_info_count $= ~:0;
      p_queue_create_infos $= queue_create_info;
      enabled_layer_count $= ~:0;
      pp_enabled_layer_names $= layers;
      enabled_extension_count $= ~:0;
      pp_enabled_extension_names $= extensions;
      p_enabled_features $= None
    ] in
  Vk.create_device phy_devices.(0) info None d
  <?> "Create logical device";
  !d

;; let surface =
     let s = Ctypes.allocate_n ~count:1 Vk.surface_khr in
     Vk__sdl.create_surface instance w None s
       <?> "Obtaining surface";
     s

;; event_loop e
;; debug "End"
