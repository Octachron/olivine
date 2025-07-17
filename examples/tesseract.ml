module A = Ctypes.CArray
module Vkt = Vk.Types
module Bt = Vk__builtin__types
module Vkh = Vk__helpers
module Vkc = Vk.Core

;; Random.self_init ()
module Dim = struct let value = 4 end

module Float_array = struct
  include A
  let zeroes n = make Ctypes.float n ~initial:0.
  let map = A.map Ctypes.float
  let init n f =
    let a = make Ctypes.float n in
    for i =0 to (n-1) do A.set a i @@ f i done;
    a
  let map2 f a b = init (length a) (fun i -> f (A.get a i) (A.get b i))
end
module Vec = Vec.Make(Float_array)(Dim)

module Utils = struct
  (** Ctype utility functions *)

  let get, set = Ctypes.(getf,setf)
  let null = Ctypes.null

  let pp_opt pp ppf = function
    | None -> ()
    | Some x -> pp ppf x

  let pp_array ~sep pp ppf a = Format.fprintf ppf "[|%a|]" (Format.pp_print_list ~pp_sep:sep pp) @@ A.to_list a
  let space = Format.pp_print_cut
  let ( #. ) x f= f x

  let debug fmt = Format.printf ("@[Debug: " ^^ fmt ^^ "@]@.")

  let (<?>) x s = match x with
    | Ok (r, x) -> Format.printf "%a: %s@." Vkt.Result.raw_pp r s; x
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.raw_pp k s; exit 1

  let (<!>) x s = match x with
    | Ok (`Success, x) -> x
    | Ok (`Suboptimal_khr as r, x) ->
      Format.printf "%a: %s@." Vkt.Result.raw_pp r s; x
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.raw_pp k s; exit 1

  let (!) = Ctypes.(!@)
  let ( <-@ ) = Ctypes.( <-@ )

  let read_spirv filename =
    let chan = open_in_bin filename in
    let len = in_channel_length chan in
    really_input_string chan len

  let fsize = Ctypes.(sizeof float)
  let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
end
open Utils

let width = 512
let heigth = 512

module Sdl = struct
  (** Sdl related function *)

  open Tsdl

  let (<?>) x s = match x with
    | Ok x -> Format.printf "SDL:Success: %s@." s; x
    | Error _ ->
      Format.eprintf "SDL:Error: %s @." s; exit 1

  let () = Sdl.(init Init.(video + events)) <?> "Sdl init"

  let w =
    Sdl.create_window "Vulkan + SDL test" ~w:width ~h:heigth
      Sdl.Window.(allow_highdpi + vulkan) <?> "Window creation"

  let () = Sdl.show_window w
  let e = Sdl.Event.create ()
  let rec event_loop idle data e =
    let data = idle data in
    let open Sdl.Event in
    if
      Sdl.poll_event @@ Some e
      && get e typ = key_down
      && get e keyboard_keycode = Sdl.K.escape
    then
      exit 0
    else event_loop idle data e

end

  let print_extension_property e =
    let open Vkt.Extension_properties in
    Format.printf "%s\n" e#.extension_name

let zero_offset = Vkt.Device_size.zero

let create_buffer phy device flag mem_size =
  let module Memprop = Vkt.Physical_device_memory_properties in
    let buffer_info =
      Vkt.Buffer_create_info.make
        ~size: mem_size
        ~usage:flag
        ~sharing_mode:Vkt.Sharing_mode.Exclusive
        () in
    let buffer = Vkc.create_buffer device buffer_info () <?> "Buffer creation" in

    let memory_rqr = Vkc.get_buffer_memory_requirements ~device ~buffer in
    debug "memory requirement: %a" Vkt.Memory_requirements.pp memory_rqr;
    let bit =
      memory_rqr #. Vkt.Memory_requirements.memory_type_bits in
    let phymem = Vkc.get_physical_device_memory_properties phy in
    let memory_type_index = 1 in
    let alloc_info =
      Vkt.Memory_allocate_info.make
        ~allocation_size:memory_rqr#.Vkt.Memory_requirements.size
        ~memory_type_index
        () in

    let buffer_memory =  Vkc.allocate_memory device alloc_info ()
                        <?> "Buffer memory allocation" in
    let () =
      Vkc.bind_buffer_memory device buffer buffer_memory zero_offset
      <!> "Bind buffer to buffer datatypes" in
    buffer, buffer_memory


module Instance = struct
  (** Creating a vulkan instance *)

  let extensions =
    A.of_list Ctypes.string ["VK_KHR_surface"; "VK_KHR_xlib_surface" ]

  let info =
    Vkt.Instance_create_info.make
      ~flags: Vkt.Instance_create_flags.empty
      ~enabled_extension_names: extensions
      (* ?layers: validation layers *)
      ()

  ;; debug "Info created"

  let x =
    Vkc.create_instance info () <?> "instance"

  let extension_properties =
    Vkc.enumerate_instance_extension_properties () <?> "Extension properties"

  ;; A.iter print_extension_property extension_properties
end
let instance = Instance.x

(** Once an instance has been created, load the KHR extensions *)
module Surface = Vk.Khr.Surface(Instance)


module Device = struct
  let phy_devices =
    let d = Vkc.enumerate_physical_devices instance <?> "physical device" in
    debug "Number of devices: %d \n" (A.length d);
    d

  let property device =
    debug "Device properties acquisition";
    let p = Vkc.get_physical_device_properties device in
    debug "Device properties acquired";
    p

  let print_property device =
    let p = property device in
    debug "Device: %s\n"
      p#.Vkt.Physical_device_properties.device_name

  ;; A.iter print_property phy_devices

  let phy = A.get phy_devices 0

   let phymem = Vkc.get_physical_device_memory_properties phy
(*    TODO: look at the memory flags output: lot of strange entries *)

  ;; debug "memory flags,@ %a" Vkt.Physical_device_memory_properties.pp phymem


  let queue_family_properties =
    Vkc.get_physical_device_queue_family_properties phy

  ;; debug "Queue properties: %a"
    (Vkh.Pp.array Vkt.Queue_family_properties.pp)
    queue_family_properties

  let queue_family = 0
  let priorities = A.of_list Ctypes.float [ 1.]

  let queue_create_info =
    Vkt.Device_queue_create_info.make
      ~queue_family_index:queue_family
      ~queue_priorities: priorities
      ()

  let device_extensions =
    Vkc.enumerate_device_extension_properties phy () <?> "device extensions"

  ;; Format.printf "Device extensions:\n@[<v 2>"
  ;; A.iter print_extension_property device_extensions
  ;; Format.printf "@]@."

  let surface_khr =
    Vkt.Surface_khr.unsafe_from_int64
    @@ Tsdl.Sdl.Vulkan.unsafe_uint64_of_surface
    @@ match
      Tsdl.Sdl.Vulkan.create_surface Sdl.w
      @@ Tsdl.Sdl.Vulkan.unsafe_instance_of_ptr
      @@ Vkt.Instance.to_ptr instance
    with
    | None -> exit 2
    | Some s -> s

  let capabilities =
    Surface.get_physical_device_surface_capabilities_khr phy surface_khr
    <?> "Surface capabilities"

  ;;debug "Surface capabilities: %a" Vkt.Surface_capabilities_khr.pp capabilities

  let supported_formats =
    Surface.get_physical_device_surface_formats_khr phy surface_khr
    <?> "supported surface formats"

  ;; debug "%a" (Vkh.Pp.array Vkt.Surface_format_khr.pp)
    supported_formats

  let present_modes =
    Surface.get_physical_device_surface_present_modes_khr phy surface_khr
    <?> "present modes"

  ;; debug "@[<v 2>%a@]" (pp_array ~sep:space Vkt.Present_mode_khr.pp) present_modes

  let support =
    let x = Surface.get_physical_device_surface_support_khr phy queue_family
      surface_khr <?> "Compatibility surface/device" in
    assert (x = true )

  let features = Vkc.get_physical_device_features phy
  ;; debug "features %a" Vkt.Physical_device_features.pp features

  let x =
    let exts = A.of_list Ctypes.string ["VK_KHR_swapchain"] in
    let info =
      let queue_create_infos = Vkt.Device_queue_create_info.array
          [queue_create_info] in
      Vkt.Device_create_info.make
        ~queue_create_infos
        ~enabled_features:features
        (*  ~pp_enabled_layer_names: layers *)
        ~enabled_extension_names: exts
      ()
      in
    Vkc.create_device phy info () <?> "Create logical device"

end
let device = Device.x
let surface_khr = Device.surface_khr

let zero_offset = Vkt.Device_size.of_int 0

module Swapchain = Vk.Khr.Swapchain(Device)

module Image = struct

  let surface_format = A.get Device.supported_formats 0


  let allocate device image =
    let reqr = Vkc.get_image_memory_requirements device image in
    let size = reqr#.Vkt.Memory_requirements.size in
    debug "Depth buffer size:%d" (Vkt.Device_size.to_int size);
    let allocate_info =
      Vkt.Memory_allocate_info.make
        ~allocation_size:size
        ~memory_type_index:0 () in
    Vkc.allocate_memory device allocate_info ()

  let format, colorspace =
    let open Vkt.Surface_format_khr in
    surface_format#.format, surface_format#.color_space

  let image_count, extent = let open Vkt.Surface_capabilities_khr in
    Device.capabilities#.min_image_count,
    Device.capabilities#. current_extent

  let swap_chain_info =
    let qfi = A.of_list Bt.uint_32_t [0] in
    Vkt.Swapchain_create_info_khr.make
      ~surface: surface_khr
      ~min_image_count: image_count
      ~image_format:format
      ~image_color_space: colorspace
      ~image_extent: extent
      ~image_array_layers:  1
      ~image_usage:Vkt.Image_usage_flags.(color_attachment + sampled)
      ~image_sharing_mode: Vkt.Sharing_mode.Exclusive
      ~queue_family_indices: qfi
      ~pre_transform:
        Vkt.Surface_transform_flags_khr.identity
      ~composite_alpha:
        Vkt.Composite_alpha_flags_khr.opaque
      ~present_mode: Vkt.Present_mode_khr.Fifo
      ~clipped: true
      ()

  let swap_chain =
    Swapchain.create_swapchain_khr device swap_chain_info ()
    <?> "swap chain creation"

  let images =
    Swapchain.get_swapchain_images_khr device swap_chain
    <?> "Swapchain images"

  ;; debug "Swapchain: %d images" (A.length images)

  let components =
    let id = Vkt.Component_swizzle.Identity in
    Vkt.Component_mapping.make ~r:id ~g:id ~b:id ~a:id

  let subresource_range aspect_mask =
    Vkt.Image_subresource_range.make
    ~aspect_mask
    ~base_mip_level: 0
    ~level_count: 1
    ~base_array_layer: 0
    ~layer_count: 1

  let image_view_info aspect format im =
    Vkt.Image_view_create_info.make
      ~image: im
      ~view_type: Vkt.Image_view_type.N2d
      ~format
      ~subresource_range:(subresource_range aspect)
      ~components
      ()

  let create_view device aspect format im =
      Vkc.create_image_view device (image_view_info aspect format im) ()

  let views =
    let aspect = Vkt.Image_aspect_flags.color in
    let create im =
      create_view device aspect format im
      <?> "Creating image view" in
    A.map Vkt.Image_view.ctype create images

end

let one_time  queue command_pool f =
  let alloc =
    Vkt.Command_buffer_allocate_info.make
      ~command_pool ~level:Vkt.Command_buffer_level.Primary
      ~command_buffer_count:1 () in
  let buffers =
    Vkc.allocate_command_buffers device alloc <?> "one time buffer"  in
  let b = A.get buffers 0 in
  let begin_info = Vkt.Command_buffer_begin_info.make
      ~flags:Vkt.Command_buffer_usage_flags.one_time_submit () in
  Vkc.begin_command_buffer b begin_info <!> "Begin one-time command";
  f b;
  Vkc.end_command_buffer b <!> "End one time-command";
  let submits = Vkt.Submit_info.array
      [ Vkt.Submit_info.make ~command_buffers:buffers ()] in
  Vkc.queue_submit ~queue ~submits () <?> "Submit one-time command";
  Vkc.queue_wait_idle queue <!> "Wait end one-time command";
  debug "Transition done";
  Vkc.free_command_buffers device command_pool buffers


module Depth = struct

  let layout = Vkt.Image_layout.Depth_stencil_attachment_optimal
  let format = Vkt.Format.D_32_sfloat

  let format_properties =
    Vkc.get_physical_device_format_properties Device.phy format

  ;; debug "Depth format %a" Vkt.Format_properties.pp format_properties

  let extent =
    let open Vkt.Extent_2d in
    Vkt.Extent_3d.make ~width:Image.extent#.width
      ~height:Image.extent#.height
      ~depth:1

  let info = Vkt.Image_create_info.make
      ~image_type:Vkt.Image_type.N2d
      ~format ~extent
      ~mip_levels:1
      ~array_layers:1
      ~samples: Vkt.Sample_count_flags.n1
      ~tiling: Vkt.Image_tiling.Optimal
      ~usage:Vkt.Image_usage_flags.(depth_stencil_attachment + transfer_src)
      ~sharing_mode:Vkt.Sharing_mode.Exclusive
      (* vvv FIXME vvv: this can only be PREINIALIZED or UNDEFINED *)
      ~initial_layout:Vkt.Image_layout.Undefined
      ()
  let image = Vkc.create_image device info () <?> "Depth image creation"

  let memory = Image.allocate device image <?> "Depth memory"
  let () = Vkc.bind_image_memory device image memory zero_offset
           <?> "Depth binding"

  let view =
    let aspect = Vkt.Image_aspect_flags.depth in
    Vkc.create_image_view device (Image.image_view_info aspect format image) ()
    <?> "Depth view"


  let transition b =
    let subresource_range =
      Image.subresource_range Vkt.Image_aspect_flags.depth in
    let dst_access_mask= let open Vkt.Access_flags in
      depth_stencil_attachment_read + depth_stencil_attachment_write
    in
    let barrier = Vkt.Image_memory_barrier.make
        ~old_layout:Vkt.Image_layout.Undefined
        ~new_layout:Vkt.Image_layout.Depth_stencil_attachment_optimal
        ~dst_access_mask
        ~src_access_mask:dst_access_mask
        ~src_queue_family_index:0 ~dst_queue_family_index:0
        ~image ~subresource_range ()
    in
    (* Different stages support only subset of src and dst masks *)
    let stage = Vkt.Pipeline_stage_flags.top_of_pipe  in
    Vkc.cmd_pipeline_barrier
      ~command_buffer:b ~src_stage_mask:stage ~dst_stage_mask:stage
      ~image_memory_barriers:(Vkt.Image_memory_barrier.array  [barrier]) ()

end

module Geom = struct
  (** Create a graphical pipeline *)

  let geom_size = Vec.dim
  let texel_size = 2
  let stride = geom_size + texel_size

  let dx = 1. /. 4.
  let flip (x, y) =y, x
  let texcoord c = (dx *. float (c mod 4), dx *. float (c/4))

  let scale = 0.5
  let u () = Random.float 1.

  let point (u,v) is a k =
    let k = stride * k in
    List.iter (fun i  -> A.set a (k+i) scale) is;
    let k = k + geom_size in
    A.set a k u;
    A.set a (k+1) v

  let tex (u,v as p ) = function
    | 0 -> p
    | 1 -> (u +. dx, v)
    | 2 -> (u, v +. dx)
    | 3 -> (u +. dx , v +. dx)
    | 4 ->  (u, v +. dx)
    | 5 ->  (u +. dx, v)
    | _ -> assert false
  
  let vertex_by_face = 6
  let face l i j a k =
    let c = texcoord k in
    let k = vertex_by_face * k in
    List.iteri (fun v is -> point (tex c v) is a @@ k + v)
    [ l; i::l ; [j] @ l ; [i;j] @ l ; [j] @ l ; [i] @ l ]

  let cube i j k =
    [ face [] k i; face [i] k j; face [j] i k; face [] j k;
      face [k] j i; face [] i j ]
   (* [face [] j k; face [] i j; face [] k i;
     face [k] j i; face [j] i k; face [i] k j]
   *)

  let faces = cube 0 1 2
  let nfaces = List.length faces
  ;; debug "Cube with %d faces" nfaces
  let input =
    let a = A.make Ctypes.float (stride * vertex_by_face * nfaces) ~initial:0. in
    List.iteri (fun i f -> f a i) faces;
    a

  let pp_input ppf a=
    for k = 0 to A.length a/ stride - 1 do
      Format.fprintf ppf "@[";
        for i = 0 to geom_size - 1 do
          Format.fprintf ppf "%f " @@ A.get a (stride * k + i)
        done;
      Format.fprintf ppf "|";
      for i = 0 to texel_size - 1 do
        Format.fprintf ppf " %f" @@ A.get a (stride * k + i + geom_size)
      done;
        Format.fprintf ppf "@]@."
    done

  (* ;; Gc.major() trigger a wrong memory read ?? *)

  ;; debug "Input:@;%a" pp_input input


  let mem_size = Vkt.Device_size.of_int @@ fsize * A.length input

  let buffer, buffer_memory =
    create_buffer Device.phy device Vkt.Buffer_usage_flags.vertex_buffer mem_size

  let () =
    let len = A.length input in
    let mapped_mem =
      Vkc.map_memory device buffer_memory zero_offset mem_size ()
      <?> "Memory mapped" in
    let a = A.from_ptr Ctypes.(coerce (ptr void) (ptr float) mapped_mem) len in
    for i = 0 to len - 1 do
      A.set a i (A.get input i)
    done;
    Vkc.unmap_memory device buffer_memory


end

module Heat_equation = struct

  let xs = 64 and ys = 64 and nface = 6 and form_factor = 16
  let xsize = 256 and ysize = 256
  let texsize = xsize * ysize
  let model_size  = xs * ys * nface
  let columns = xsize / xs
  ;; assert(columns = 4)

  let data = Array.init texsize (fun _ -> Random.float 1.)
  let data' = Array.make texsize 0.

  type dir = X | Y
  type connection = Direct | Reverse
  type edge = { dir: dir; normal: int; lim: int; face:int; len: int }

  let reorient connection edge x =
    let t = if connection = Direct then x else edge.len - x - 1 in
    let n = edge.lim in
    if edge.dir = X then (t,n, edge.face) else (n,t, edge.face)

  let hit (src, connection, dest) (x,y,_k) =
    let norm, transv = if src.dir = X then (x,y) else (y,x) in
    if norm = src.lim + src.normal then Some(reorient connection dest transv)
    else None


  let rec edge_transition pos = function
  | [] -> pos
  | edge :: q ->
      match hit edge pos with
      | None -> edge_transition pos q
      | Some pos -> pos

  let len = function X -> ys | Y -> xs
  let edge dir lim face =
  let lim, normal = if lim = 0 then lim, -1 else lim - 1, 1 in
  { dir;lim;face; normal; len = len dir }

let (++) atlas (e1,c,e2 as l) =
  atlas.(e1.face) <- l :: atlas.(e1.face);
  atlas.(e2.face) <- (e2,c,e1) :: atlas.(e2.face);
  atlas

let v k = edge X xs k, Direct, edge X 0 ( (k+1) mod 4)

let atlas =
  [ v 0; v 1; v 2; v 3;
    edge Y ys 0, Direct, edge Y 0 4;
    edge Y ys 4, Reverse, edge Y ys 2;
    edge Y 0 2, Direct, edge Y ys 5;
    edge Y 0 5, Reverse, edge Y 0 0;
    edge Y ys 1, Direct, edge X 0 4;
    edge X xs 4, Reverse, edge Y ys 3;
    edge Y 0 3, Direct, edge X 0 5;
    edge X xs 5, Reverse, edge Y 0 1
  ]

let index (x,y,k) =
    x + xs * (k mod columns) + xsize * ( y + ys * (k/columns))


let set_edge dir edge values =
  let n = Array.length values in
  let pl = edge.len / n in
  for seg = 0 to n - 1 do
    for k = 0 to pl - 1 do
      let pos = reorient dir edge (k + seg * pl)  in
      data.(index(pos)) <- values.(seg)
    done;
  done

let set_link (e,dir,f) =
  let u () =
    let f = 1. -. (Random.float 1.) ** 2. in
    [| f; 1. -. f |] in
  set_edge Direct e @@ u ();
  set_edge dir f @@ u ()

;; List.iter set_link atlas
let paint_face k c =
  for x = 1 to xs - 2 do for y = 1 to ys - 2 do
    data.(index(x,y,k)) <- c done; done

let border_index atlas (_x,_y, k as pos ) =
  let pos = edge_transition pos atlas.(k) in
  index pos

  let laplacian dt data data' (x,y,k) =
    let pos = index (x,y,k) in
    data'.(pos) <- data.(pos) -.
    dt *.( 4. *. data.(pos) -. data.(index(x+1,y,k)) -. data.(index(x-1,y,k)) -. data.(index(x,y+1,k)) -. data.(index(x,y-1,k)))

  let diffop dt data data'=
    for k = 0 to nface - 1 do
      for y = 1 to ys -2 do
        for x = 1 to xs -2 do
          laplacian dt data data' (x,y,k)
        done;
      done;
    done

    let operator dt data data' = diffop dt data data'; diffop dt data' data


   let dt = 0.001
   let niter = int_of_float @@ 0.1 /. dt
   let iter () = operator dt data data'
   let () =
     for _i = 0 to niter do
       iter ()
     done
   ;;debug "Heat equation computed"
 end

module Texture = struct
  open Heat_equation
  let memsize = Vkt.Device_size.of_int @@ texsize * fsize

  let format = Vkt.Format.R_32_sfloat
  let src_buffer, memory = create_buffer Device.phy device
      Vkt.Buffer_usage_flags.transfer_src memsize

  let transfer_data () = (* fill texture data *)
    let mem =
      Vkc.map_memory ~device ~memory ~offset:zero_offset ~size:memsize ()
      <!> "Mapping memory for texture" in
    let data = Ctypes.(coerce (ptr void) (ptr float)) mem   in
    let a = A.from_ptr data Heat_equation.texsize in
    for i = 0 to texsize - 1 do A.set a i Heat_equation.data.(i) done;
    Vkc.unmap_memory device memory

;; transfer_data ()
  
  let extent = Vkt.Extent_3d.make ~width:xsize ~height:ysize ~depth:1
  let image_info =
    Vkt.Image_create_info.make
      ~image_type:Vkt.Image_type.N2d
      ~format
      ~extent
      ~mip_levels:1
      ~array_layers:1
      ~tiling:Vkt.Image_tiling.Optimal
      ~usage:Vkt.Image_usage_flags.(transfer_dst + sampled)
      ~initial_layout:Vkt.Image_layout.Preinitialized
      ~sharing_mode:Vkt.Sharing_mode.Exclusive
      ~samples:Vkt.Sample_count_flags.n1
      ()

  let image = Vkc.create_image device image_info ()
              <!> "Texture image"

  let mem = Image.allocate device image <!> "Texture image memory allocation"
  let () = Vkc.bind_image_memory device image mem zero_offset
           <!> "Binding texture image"


  let qf_ignored = Unsigned.UInt.to_int Vk.Const.queue_family_ignored

  let transition image src_mask aspect_mask dst_mask old_layout new_layout cmd =
    let subresource_range =  Vkt.Image_subresource_range.make
        ~aspect_mask
        ~base_mip_level:0 ~level_count:1
        ~base_array_layer:0 ~layer_count:1 in
    let barrier =
      Vkt.Image_memory_barrier.array [
        Vkt.Image_memory_barrier.make
          ~src_access_mask:src_mask
          ~dst_access_mask:dst_mask
          ~old_layout
          ~new_layout
          ~src_queue_family_index:qf_ignored
          ~dst_queue_family_index:qf_ignored
          ~image
          ~subresource_range
          ()
    ]
    in
    Vkc.cmd_pipeline_barrier ~command_buffer:cmd
      ~src_stage_mask:Vkt.Pipeline_stage_flags.top_of_pipe
      ~dst_stage_mask:Vkt.Pipeline_stage_flags.top_of_pipe
      ~image_memory_barriers:barrier
      ()

  let copy_buffer_to_image aspect b img cmd =
    let image_subresource =
      Vkt.Image_subresource_layers.make
        ~aspect_mask:aspect
        ~mip_level:0 ~base_array_layer:0 ~layer_count:1 in
    let image_offset = Vkt.Offset_3d.make ~x:0 ~y:0 ~z:0 in
    let region =
      Vkt.Buffer_image_copy.array [ Vkt.Buffer_image_copy.make
        ~image_subresource
        ~buffer_offset:zero_offset
        ~buffer_row_length:0
        ~buffer_image_height:0
        ~image_offset
        ~image_extent:extent ] in
    Vkc.cmd_copy_buffer_to_image
      cmd b img Vkt.Image_layout.Transfer_dst_optimal region

  let transfer src dst cmd =
    let copy =
      Vkt.Buffer_copy.(array [make zero_offset zero_offset memsize]) in
    Vkc.cmd_copy_buffer cmd src dst copy

  let color = Vkt.Image_aspect_flags.color


  let transfer_full cmd =
    let do' f = f cmd in
    let host, transfer, shader =
      Vkt.Access_flags.( host_write, transfer_write, shader_read) in
    List.iter do' [
      transition image host color transfer
        Vkt.Image_layout.Preinitialized Vkt.Image_layout.Transfer_dst_optimal;
      copy_buffer_to_image color src_buffer image;
      transition image transfer color shader
        Vkt.Image_layout.Transfer_dst_optimal
        Vkt.Image_layout.Shader_read_only_optimal]

  let view =
    Image.create_view device color format image <?> "Creating texture view"

  let sampler_info =
    let repeat = Vkt.Sampler_address_mode.Repeat in
    Vkt.Sampler_create_info.make
      ~mag_filter:Vkt.Filter.Linear
      ~min_filter:Vkt.Filter.Linear
      ~mipmap_mode:Vkt.Sampler_mipmap_mode.Linear
      ~address_mode_u:repeat ~address_mode_v:repeat ~address_mode_w:repeat
      ~compare_enable:false ~compare_op:Vkt.Compare_op.Always
      ~mip_lod_bias:0. ~min_lod:0. ~max_lod:0.
      ~anisotropy_enable:true
      ~max_anisotropy:16.
      ~border_color:Vkt.Border_color.Float_opaque_black
      ~unnormalized_coordinates:false ()

  let sampler = Vkc.create_sampler device sampler_info ()
                <?> "Creating texture sampler"

  let descriptor_binding =
    Vkt.Descriptor_set_layout_binding.make
      ~binding:1
      ~descriptor_count:1
      ~descriptor_type:Vkt.Descriptor_type.Combined_image_sampler
      ~stage_flags:Vkt.Shader_stage_flags.fragment
      ()

end

module Uniform = struct

  let binding =
    Vkt.Descriptor_set_layout_binding.make
      ~binding:0  ~descriptor_count:1
      ~descriptor_type:Vkt.Descriptor_type.Uniform_buffer
      ~stage_flags:Vkt.Shader_stage_flags.vertex ()

  let bindings =
    Vkt.Descriptor_set_layout_binding.array
      [binding; Texture.descriptor_binding]

  let layout_info =
    Vkt.Descriptor_set_layout_create_info.make ~bindings ()

  let layout = Vkc.create_descriptor_set_layout device layout_info ()
               <?> "Descriptor layout creation"

  let layouts = Vkt.Descriptor_set_layout.array [layout]

  let size = Vkt.Device_size.of_int ( (Vec.dim + 1 ) * Vec.dim * fsize )
  let buffer, memory =
    create_buffer Device.phy device Vkt.Buffer_usage_flags.uniform_buffer size

  let pool_sizes = let open Vkt.Descriptor_pool_size in
    array
      [make Vkt.Descriptor_type.Uniform_buffer 1;
       make Vkt.Descriptor_type.Combined_image_sampler 1]

  let pool_info =
    Vkt.Descriptor_pool_create_info.make ~max_sets:1 ~pool_sizes ()

  let pool =
    Vkc.create_descriptor_pool device pool_info () <?> "Descriptor pool"

  let alloc =
    Vkt.Descriptor_set_allocate_info.make pool layouts ()

  let descriptor_sets=
    Vkc.allocate_descriptor_sets device alloc <?> "Descriptor set"

  ;; debug "Allocated %d descriptor sets" (A.length descriptor_sets)

  let image_info = Vkt.Descriptor_image_info.array
    [Vkt.Descriptor_image_info. make
           ~sampler:Texture.sampler
           ~image_view:Texture.view
           ~image_layout:Vkt.Image_layout.Shader_read_only_optimal
    ]

  let buffer_info =
    Vkt.Descriptor_buffer_info.array
      [Vkt.Descriptor_buffer_info.make ~buffer ~offset:zero_offset ~range:size ()]

  let empty_array ty = A.make ty 0

  let write_info =
    Vkt.Write_descriptor_set.array [
     Vkt.Write_descriptor_set.make
        ~dst_set:(A.get descriptor_sets 0) ~dst_binding:0 ~descriptor_count:1
        ~dst_array_element:0
      ~descriptor_type:Vkt.Descriptor_type.Uniform_buffer
      (* TODO: the three fields belows corresponds to the
         constructors of a sum type:
         descriptor_type is the sum flag, and only on of  *)
      ~buffer_info
      ~texel_buffer_view:(empty_array Vkt.Buffer_view.ctype)
      ~image_info:(empty_array Vkt.Descriptor_image_info.ctype) ();
     Vkt.Write_descriptor_set.make
        ~dst_set:(A.get descriptor_sets 0) ~dst_binding:1 ~descriptor_count:1
        ~dst_array_element:0
      ~descriptor_type:Vkt.Descriptor_type.Combined_image_sampler
      (* TODO: the three fields belows corresponds to the
         constructors of a sum type:
         descriptor_type is the sum flag, and only on of  *)
      ~buffer_info:(empty_array Vkt.Descriptor_buffer_info.ctype)
      ~texel_buffer_view:(empty_array Vkt.Buffer_view.ctype)
      ~image_info ()
      ]


    let transfer vec matrix =
    let m = Vkc.map_memory device memory zero_offset size () <!> "map memory" in
    let typed = Ctypes.(coerce (ptr void) (ptr float) m) in
    let a = A.from_ptr typed (Vec.size vec + Vec.size matrix)  in
    Vec.blit_to ~from:vec ~to':a ();
    Vec.blit_to ~offset:(Vec.size vec) ~from:matrix ~to':a ();
    Vkc.unmap_memory device memory

  end

module Pipeline = struct
  let vertex_binding =
    Vkt.Vertex_input_binding_description.make
      ~binding:0
      ~stride:(Geom.stride * fsize)
      ~input_rate:Vkt.Vertex_input_rate.Vertex

  let attributes = A.make Vkt.Vertex_input_attribute_description.ctype 2

  let geom_attribute =
    Vkt.Vertex_input_attribute_description.make
      ~location:0 ~binding:0 ~format:Vkt.Format.R32g32b32a_32_sfloat
      ~offset:0

  let texel_attribute =
    Vkt.Vertex_input_attribute_description.make
      ~location:1 ~binding:0 ~format:Vkt.Format.R32g_32_sfloat
      ~offset:(Geom.geom_size * fsize)

  ;; A.set attributes 0 geom_attribute
  ;; A.set attributes 1 texel_attribute

  let bindings =
    Vkt.Vertex_input_binding_description.array [vertex_binding]

  let input_description =
    Vkt.Pipeline_vertex_input_state_create_info.make
      ~vertex_binding_descriptions:bindings
      ~vertex_attribute_descriptions:attributes
      ()

  let input_assembly =
    Vkt.Pipeline_input_assembly_state_create_info.make
      ~topology: Vkt.Primitive_topology.Triangle_list
      ~primitive_restart_enable: false
    ()

  let viewport =
    let width = float Image.extent#.Vkt.Extent_2d.width
    and height = float Image.extent#.Vkt.Extent_2d.height in
  Vkt.Viewport.make ~x: 0. ~y: 0. ~width ~height
      ~min_depth: 0.
      ~max_depth: 10.

  let scissor =
    Vkt.Rect_2d.make
      ~offset: Vkt.Offset_2d.(make ~x:0 ~y:0)
      ~extent:Image.extent

  let viewports = Vkt.Viewport.array [viewport]
  let scissors = Vkt.Rect_2d.array [scissor]

  let viewport_state =
    Vkt.Pipeline_viewport_state_create_info.make
      ~viewports ~scissors ()

  let rasterizer =
    Vkt.Pipeline_rasterization_state_create_info.make
      ~depth_clamp_enable: false
      ~rasterizer_discard_enable: false
      ~polygon_mode: Vkt.Polygon_mode.Fill
      ~cull_mode: Vkt.Cull_mode_flags.front
      ~front_face: Vkt.Front_face.Counter_clockwise
      ~depth_bias_enable: false
      ~depth_bias_constant_factor: 0.
      ~depth_bias_clamp: 0.
      ~depth_bias_slope_factor: 0.
      ~line_width: 1.
      ()

  let no_multisampling =
    Vkt.Pipeline_multisample_state_create_info.make
      ~rasterization_samples: Vkt.Sample_count_flags.n1
      ~sample_shading_enable: false
      ~min_sample_shading: 1.
      ~alpha_to_coverage_enable: false
      ~alpha_to_one_enable: false
      ()

  let no_blend =
    Vkt.Pipeline_color_blend_attachment_state.make
     ~blend_enable: false
     ~color_write_mask: Vkt.Color_component_flags.(r + g + b + a)
     ~src_color_blend_factor: Vkt.Blend_factor.One
     ~dst_color_blend_factor: Vkt.Blend_factor.Zero
     ~color_blend_op: Vkt.Blend_op.Add
     ~src_alpha_blend_factor: Vkt.Blend_factor.One
     ~dst_alpha_blend_factor: Vkt.Blend_factor.Zero
     ~alpha_blend_op: Vkt.Blend_op.Add
     ()

  let depth_info =
    let st = Vkt.Stencil_op_state.unsafe_make () in
    Vkt.Pipeline_depth_stencil_state_create_info.make
      ~depth_test_enable:true
      ~depth_write_enable:true
      ~depth_compare_op:Vkt.Compare_op.Less
      (* Fixme vv Option group vv *)
      ~depth_bounds_test_enable:false
      ~min_depth_bounds:(0.)
      ~max_depth_bounds:10.
      (* Fixme vv Option group vv *)
      ~stencil_test_enable:false
      ~front:st
      ~back:st
      ()

  let blend_state_info =
    let attachments =
      Vkt.Pipeline_color_blend_attachment_state.array [no_blend] in
    let consts = A.of_list Ctypes.float [ 0.; 0.; 0.; 0. ] in
    Vkt.Pipeline_color_blend_state_create_info.make
     ~logic_op_enable: false
     ~logic_op: Vkt.Logic_op.Copy
     ~attachments
     ~blend_constants: consts
     ()


  let no_uniform =
    Vkt.Pipeline_layout_create_info.make ()

  let simple_uniform =
    Vkt.Pipeline_layout_create_info.make
      ~set_layouts:Uniform.layouts
      ()

  let layout =
    Vkc.create_pipeline_layout device simple_uniform ()
    <?> "Creating pipeline layout"

  let make_attachment store_op format final_layout =
    Vkt.Attachment_description.make
      ~format
      ~samples: Vkt.Sample_count_flags.n1
      ~load_op: Vkt.Attachment_load_op.Clear
      ~store_op
      ~stencil_load_op: Vkt.Attachment_load_op.Dont_care
      ~stencil_store_op: Vkt.Attachment_store_op.Dont_care
      ~initial_layout: Vkt.Image_layout.Undefined
      ~final_layout
      ()

  let store = Vkt.Attachment_store_op.Store
  let dont = Vkt.Attachment_store_op.Dont_care
  let color_description =
    make_attachment store Image.format Vkt.Image_layout.Present_src_khr

  let depth_description = make_attachment store Depth.format Depth.layout

  let color_attachment =
    Vkt.Attachment_reference.make
      ~attachment: 0
      ~layout: Vkt.Image_layout.Color_attachment_optimal

  let depth_stencil_attachment =
    Vkt.Attachment_reference.make
      ~attachment: 1
      ~layout:Depth.layout

  let dependencies = Vkt.Subpass_dependency.array
    [ let stage = Vkt.Pipeline_stage_flags.color_attachment_output in
    Vkt.Subpass_dependency.make
      ~src_subpass:(Unsigned.UInt.to_int Vk.Const.subpass_external)
      ~dst_subpass:0
      ~src_stage_mask:stage
      ~dst_stage_mask:stage
      ~dst_access_mask:
        Vkt.Access_flags.(color_attachment_read+color_attachment_write)
      ()
    ]

  let subpass =
    let color = Vkt.Attachment_reference.array [color_attachment] in
    Vkt.Subpass_description.make
      ~pipeline_bind_point: Vkt.Pipeline_bind_point.Graphics
      ~color_attachments: color
      ~depth_stencil_attachment
      ()

  let render_pass_info =
    let attachments = A.of_list Vkt.Attachment_description.ctype
        [color_description; depth_description] in
    let subpasses = A.of_list Vkt.Subpass_description.ctype [subpass] in
    Vkt.Render_pass_create_info.make
      ~attachments ~subpasses ~dependencies ()

  let simple_render_pass =
    Vkc.create_render_pass device render_pass_info () <?> "Creating render pass"


  module Shaders = struct

    let frag = read_spirv "shaders/tesseract/frag.spv"
    let vert = read_spirv "shaders/tesseract/vert.spv"

    let shader_module_info s =
      let len = String.length s in
      let c = A.make Bt.uint_32_t (len / Ctypes.(sizeof uint32_t)) in
      let c' =
        A.from_ptr Ctypes.(coerce (ptr Bt.uint_32_t) (ptr char) @@ A.start c) len in
      String.iteri (fun n x -> A.set c' n x) s;
      Vkt.Shader_module_create_info.make
        ~code_size:(Unsigned.Size_t.of_int len)
        ~code:c
        ()

    let create_shader name s =
      let info = shader_module_info s in
      Vkc.create_shader_module device info () <?> "Shader creation :" ^ name

    let frag_shader = create_shader "fragment" frag
    let vert_shader = create_shader "vertex" vert

    let name = "main"
    let make_stage stage module' =
      Vkt.Pipeline_shader_stage_create_info.make
        ~flags: Vkt.Pipeline_shader_stage_create_flags.empty
        ~stage
        ~module'
        ~name
        ()

    let frag_stage = make_stage Vkt.Shader_stage_flags.fragment frag_shader
    let vert_stage = make_stage Vkt.Shader_stage_flags.vertex vert_shader
  end


  let pipeline_info =
    let stages = A.of_list Vkt.Pipeline_shader_stage_create_info.ctype
        Shaders.[ vert_stage; frag_stage ] in
    Vkt.Graphics_pipeline_create_info.make
      ~stages
      ~vertex_input_state: input_description
      ~input_assembly_state: input_assembly
      ~viewport_state: viewport_state
      ~rasterization_state: rasterizer
      ~multisample_state: no_multisampling
      ~color_blend_state: blend_state_info
      ~depth_stencil_state:depth_info
      ~layout
      ~render_pass: simple_render_pass
      ~subpass: 0
      ~base_pipeline_index: 0
      ()

  let pipeline_infos = A.of_list Vkt.Graphics_pipeline_create_info.ctype
      [pipeline_info]

  let pipelines =
    Vkc.create_graphics_pipelines device pipeline_infos ()
    <?> "Graphics pipeline creation"

  let x = A.get pipelines 0

end




module Cmd = struct

  ;; Vkc.update_descriptor_sets ~device
    ~descriptor_writes:Uniform.write_info ()


  let images =
    Array.init 2 ( fun i ->
        Vkt.Image_view.array [A.get Image.views i; Depth.view])
  let framebuffer_info i =

    Vkt.Framebuffer_create_info.make
      ~render_pass: Pipeline.simple_render_pass
      ~attachments: images.(i)
      ~width: Image.extent#.Vkt.Extent_2d.width
      ~height: Image.extent#.Vkt.Extent_2d.height
      ~layers:  1
      ()

  let framebuffer index =
    Vkc.create_framebuffer device (framebuffer_info index) ()
    <?> "Framebuffer creation"

  let queue =
    Vkc.get_device_queue device Device.queue_family 0

  let command_pool_info =
    Vkt.Command_pool_create_info.make
      ~queue_family_index: Device.queue_family
      ()

  let command_pool =
    Vkc.create_command_pool device command_pool_info ()
    <?> "Command pool creation"


  let () = (* initialize depth buffer *)
    one_time queue command_pool Depth.transition;
    (* inialize texture *)
    one_time queue command_pool Texture.transfer_full
  let framebuffers =
    Vkt.Framebuffer.array [framebuffer 0; framebuffer 1]

  let n_cmd_buffers =  (A.length framebuffers)
  let buffer_allocate_info =
    Vkt.Command_buffer_allocate_info.make
      ~command_pool
      ~level: Vkt.Command_buffer_level.Primary
      ~command_buffer_count: n_cmd_buffers
      ()

  let cmd_buffers =
    Vkc.allocate_command_buffers device buffer_allocate_info
    <?> "Command buffers allocation"

  ;;debug "Created %d cmd buffers" n_cmd_buffers

  let cmd_begin_info =
    Vkt.Command_buffer_begin_info.make
      ~flags: Vkt.Command_buffer_usage_flags.simultaneous_use
      ()

  let clear_colors =
    let a = A.of_list Ctypes.float [ 0.;0.;0.; 1.] in
    let c =Vkt.Clear_color_value.float_32 a in
    Vkt.Clear_value.color c

  let clear_depths =
    let f = Vkt.Clear_depth_stencil_value.make ~depth:10. ~stencil:0 in
    Vkt.Clear_value.depth_stencil f

  let clear_values =
    Vkt.Clear_value.array [clear_colors;clear_depths]

  let render_pass_info fmb =
    Vkt.Render_pass_begin_info.make
      ~render_pass: Pipeline.simple_render_pass
      ~framebuffer: fmb
      ~render_area: Pipeline.scissor
      ~clear_values
      ()

  let render_pass_infos = A.map Vkt.Render_pass_begin_info.ctype
      render_pass_info framebuffers
  let vertex_buffers = Vkt.Buffer.array_opt [Geom.buffer]
  let offsets = A.of_list Vkt.Device_size.ctype Vkt.Device_size.[of_int 0]
  let cmd b ifmb =
    Vkc.begin_command_buffer b cmd_begin_info <!> "Begin command buffer";
    Vkc.cmd_begin_render_pass b (A.get render_pass_infos ifmb)
      Vkt.Subpass_contents.Inline;
    Vkc.cmd_bind_pipeline b Vkt.Pipeline_bind_point.Graphics Pipeline.x;
    Vkc.cmd_bind_vertex_buffers b 0 vertex_buffers offsets;
    Vkc.cmd_bind_descriptor_sets ~command_buffer:b
      ~pipeline_bind_point:Vkt.Pipeline_bind_point.Graphics
      ~layout:Pipeline.layout ~first_set:0
      ~descriptor_sets: Uniform.descriptor_sets ();
    Vkc.cmd_draw b Geom.( vertex_by_face* nfaces) 1 0 0;
    Vkc.cmd_end_render_pass b;
    Vkc.end_command_buffer b <!> "Command buffer recorded"

  let iteri f a =
    for i=0 to A.length a - 1 do
      f (A.get a i) i
    done

  let () = iteri cmd cmd_buffers
end

module Render = struct

  let semaphore_info =
    Vkt.Semaphore_create_info.make ()

  let create_semaphore () =
    Vkc.create_semaphore device semaphore_info () <?> "Created semaphore"

  let im_semaphore = create_semaphore ()
  let render_semaphore = create_semaphore ()

  let wait_sems = Vkt.Semaphore.array [im_semaphore]
  let sign_sems = Vkt.Semaphore.array [render_semaphore]


  let wait_stage = let open Vkt.Pipeline_stage_flags in
    A.of_list ctype [color_attachment_output]

  let submit_info _index (* CHECK-ME *) =
    Vkt.Submit_info.array [
    Vkt.Submit_info.make
      ~wait_semaphores: wait_sems
      ~wait_dst_stage_mask: wait_stage
      ~command_buffers: Cmd.cmd_buffers
      ~signal_semaphores: sign_sems ()
  ]

  let swapchains = A.of_list Vkt.Swapchain_khr.ctype [Image.swap_chain]

  let present_indices = A.of_list Bt.uint_32_t [0]
  (* Warning need to be alive as long as present_info can be used! *)

  let present_info =
    Vkt.Present_info_khr.make
      ~wait_semaphores: sign_sems
      ~swapchains
      ~image_indices: present_indices
      ()

  let debug_draw () =
    let n = Swapchain.acquire_next_image_khr ~device ~swapchain:Image.swap_chain
      ~timeout:Unsigned.UInt64.max_int ~semaphore:im_semaphore ()
            <?> "Acquire image" in
    A.set present_indices 0 n;
    debug "Image %d acquired" n;
    let () = Uniform.transfer (Vec.zero `vec) Vec.id in
    Vkc.queue_submit ~queue:Cmd.queue ~submits:(submit_info n) ()
    <?> "Submitting command to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <!> "Image presented"

  let rec acquire_next () =
    match  Swapchain.acquire_next_image_khr ~device
             ~swapchain:Image.swap_chain
               ~timeout:Unsigned.UInt64.max_int ~semaphore:im_semaphore () with
      | Ok ((`Success|`Suboptimal_khr), n) -> n
      | Ok ((`Timeout|`Not_ready), _ ) -> acquire_next ()
      | Error x ->
        (Format.eprintf "Error %a in acquire_next" Vkt.Result.raw_pp x; exit 2)


  let r i j (x,_) = Vec.axis_rotation i j x
  let u f = f *. (Random.float 2. -. 1.)

  let vec_phase (position,speed) =
    Vec.(0.9 *. position + speed, 0.99 *. speed + Vec.vec (fun _ -> u 0.001))
  let rot x y z t = Vec.( r 0 1 x * r 1 2 y * r 2 0 z * r 0 3 t)
  let phase (angle,speed) = (angle +. speed, speed +. u 0.001)
  let draw (p, (x, y, z, t)) =
    A.set present_indices 0 @@ acquire_next ();
    let x,y,z,t as state = phase x, phase y, phase z, phase t in
    let p = vec_phase p in
    let () = Uniform.transfer (fst p) (rot x y z t) in
    Vkc.queue_submit ~queue:Cmd.queue ~submits:(submit_info present_indices) ()
    <!> "Submit to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <!> "Present to queue";
    p, state

end
let vec: Vec.vec * Vec.vec = Vec.(zero `vec, zero `vec)
;; Render.(debug_draw(); debug_draw ())
 ; Sdl.(event_loop Render.draw (let z = 0., 0. in vec, (z,z,z,z))  e)
;; debug "End"
