module A = Ctypes.CArray
module Vkt = Vk.Types
module Vkc = Vk.Core
module Vkr = Vk.Raw
module Vkw = Vk__sdl

;; Gc.set
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

  let ( #. ) = get

  let debug fmt = Format.printf ("Debug: " ^^ fmt ^^ "@.")

  let (<*>) x s = match x with
    | Ok (_,x) -> x
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (<!>) x s = match x with
    | Ok (r, x) -> Format.printf "%a: %s@." Vkt.Result.pp r s; x
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (<!*>) x s = match x with
    | Ok r -> Format.printf "%a: %s@." Vkt.Result.pp r s; x
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (<!!>) x s = match x with
    | Ok `Success -> ()
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (<??>) x s = match x with
    | Ok (`Success|`Suboptimal_khr) -> ()
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (!) = Ctypes.(!@)
  let (+@) = Ctypes.(+@)
  let ( <-@ ) = Ctypes.( <-@ )

  let from_array typ a =
    let n = Array.length a in
    let a' = Ctypes.allocate_n ~count:n typ in
    Array.iteri (fun i x -> (a' +@ i) <-@ x ) a;
    n, a'

  let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
  let to_string carray =
    String.init (A.length carray)
      (fun n -> A.get carray n)

  let read_spirv filename =
    let chan = open_in_bin filename in
    let len = in_channel_length chan in
    really_input_string chan len

end
open Utils

module Sdl = struct
  (** Sdl related function *)
  open Tsdl

  let (<?>) x s = match x with
    | Ok x -> Format.printf "SDL:Success: %s@." s; x
    | Error _ ->
      Format.eprintf "SDL:Error: %s @." s; exit 1

  let () = Sdl.(init Init.(video + events)) <?> "Sdl init"

  let w =
    Sdl.create_window "Vulkan + SDL test" ~w:512 ~h:512
      Sdl.Window.(allow_highdpi) <?> "Window creation"

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
    Format.printf "%s\n" (to_string e#.extension_name)

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
    Vkc.create_instance info () <!> "instance"

  let extension_properties =
    Vkc.enumerate_instance_extension_properties () <!> "Extension properties"

  ;; A.iter print_extension_property extension_properties
end
let instance = Instance.x

(** Once an instance has been created, load the KHR extensions *)
module Surface = Vk.Khr.Surface(Instance)


module Device = struct
  let phy_devices =
    let d = Vkc.enumerate_physical_devices instance <!> "physical device" in
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
      (to_string p#.Vkt.Physical_device_properties.device_name)

  ;; A.iter print_property phy_devices

  let phy = A.get phy_devices 0

  let queue_family_properties = Vkc.get_physical_device_queue_family_properties phy

  let print_queue_property ppf property =
    Format.fprintf ppf "Queue flags: %a \n" (pp_opt Vkt.Queue_flags.pp)
      property#.Vkt.Queue_family_properties.queue_flags

  ;; A.iter (print_queue_property Format.std_formatter) queue_family_properties

  let queue_family = 0
  let priorities = A.of_list Ctypes.float [ 1.]

  let queue_create_info =
    Vkt.Device_queue_create_info.make
      ~queue_family_index:queue_family
      ~queue_priorities: priorities
      ()

  let device_extensions =
    Vkc.enumerate_device_extension_properties phy () <!> "device extensions"

  ;; Format.printf "Device extensions:\n@[<v 2>"
  ;; A.iter print_extension_property device_extensions
  ;; Format.printf "@]@."

  let surface_khr =
    Vkw.create_surface instance Sdl.w () <!> "Obtaining surface"

  let capabilities =
    Surface.get_physical_device_surface_capabilities_khr phy surface_khr
    <!> "Surface capabilities"

  let pp_extent_2d ppf extent = let open Vkt.Extent_2d in
    Format.fprintf ppf "[%d×%d]"
      extent#.width extent#.height

  let pp_capability ppf cap = let open Vkt.Surface_capabilities_khr in
    Format.fprintf ppf
      "@[min_image:%d; max_image_count:%d; current_extent:%a;@;\
       transform:%a;@ composite_alpha:%a;@ usage_flags:%a@]"
      cap#.min_image_count cap#.max_image_count
      pp_extent_2d cap#.current_extent
      (pp_opt Vkt.Surface_transform_flags_khr.pp) cap#.supported_transforms
      (pp_opt Vkt.Composite_alpha_flags_khr.pp) cap#.supported_composite_alpha
      (pp_opt Vkt.Image_usage_flags.pp) cap#.supported_usage_flags

  ;; debug "Surface capabilities: %a" pp_capability capabilities

  let supported_formats =
    Surface.get_physical_device_surface_formats_khr phy surface_khr
    <!> "supported surface formats"

  let pp_sformat ppf sformat = let open Vkt.Surface_format_khr in
    Format.fprintf ppf "surface format @[{@ format=@[%a@];@ color_space=@[%a@]}"
      Vkt.Format.pp sformat#.format Vkt.Color_space_khr.pp sformat#.color_space

  ;; A.iter (debug "%a" pp_sformat) supported_formats

  let present_modes =
    Surface.get_physical_device_surface_present_modes_khr phy surface_khr
    <!> "present modes"

  ;; A.iter (debug "%a" Vkt.Present_mode_khr.pp) present_modes

  let support =
    let x = Surface.get_physical_device_surface_support_khr phy queue_family
      surface_khr <!> "Compatibility surface/device" in
    assert (x = true )

  let x =
    let exts = A.of_list Ctypes.string ["VK_KHR_swapchain"] in
    let info =
      let queue_create_infos = A.from_ptr queue_create_info 1 in
      Vkt.Device_create_info.make
        ~queue_create_infos
        (*  ~pp_enabled_layer_names: layers *)
        ~enabled_extension_names: exts
      ()
      in
    Vkc.create_device phy info () <!> "Create logical device"

end
let device = Device.x
let surface_khr = Device.surface_khr

let zero_offset = Vkt.Device_size.of_int 0

module Swapchain = Vk.Khr.Swapchain(Device)

module Image = struct

  let surface_format = A.get Device.supported_formats 0

  let format, colorspace =
    let open Vkt.Surface_format_khr in
    surface_format#.format, surface_format#.color_space

  let image_count, extent = let open Vkt.Surface_capabilities_khr in
    Device.capabilities#.min_image_count,
    Device.capabilities#. current_extent

  let swap_chain_info =
    let qfi = A.of_list Vkt.uint_32_t [0] in
    Vkt.Swapchain_create_info_khr.make
      ~surface: surface_khr
      ~min_image_count: image_count
      ~image_format:format
      ~image_color_space: colorspace
      ~image_extent: extent
      ~image_array_layers:  1
      ~image_usage:Vkt.Image_usage_flags.(
          of_list [
            color_attachment;
            sampled
          ])
      ~image_sharing_mode: Vkt.Sharing_mode.Exclusive
      ~queue_family_indices: qfi
      ~pre_transform:
        Vkt.Surface_transform_flags_khr.identity
      ~composite_alpha:
        Vkt.Composite_alpha_flags_khr.opaque
      ~present_mode: Vkt.Present_mode_khr.Fifo
      ~clipped: true
      ~old_swapchain:Vkt.Swapchain_khr.null
      ()

  let swap_chain =
    Swapchain.create_swapchain_khr device swap_chain_info ()
    <!> "swap chain creation"

  let images =
    Swapchain.get_swapchain_images_khr device swap_chain
    <!> "Swapchain images"

  ;; debug "Swapchain: %d images" (A.length images)

  let components =
    let id = Vkt.Component_swizzle.Identity in
    (!) @@ Vkt.Component_mapping.make ~r:id ~g:id ~b:id ~a:id

  let subresource_range aspect_mask =
    (!) @@ Vkt.Image_subresource_range.make
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

  let views =
    let aspect = Vkt.Image_aspect_flags.(singleton color) in
    let create im =
      Vkc.create_image_view device (image_view_info aspect format im) ()
      <!> "Creating image view" in
    A.map Vkt.image_view create images

end

module Depth = struct

  let layout = Vkt.Image_layout.Depth_stencil_attachment_optimal
  let format = Vkt.Format.D_32_sfloat

  let format_properties =
    Vkc.get_physical_device_format_properties Device.phy format

  let () = let open Vkt.Format_properties in
    debug "Depth format {linear tiling:%a; optimal tiling:%a; buffer feature:%a}"
      (pp_opt Vkt.Format_feature_flags.pp)
      format_properties#.linear_tiling_features
      (pp_opt Vkt.Format_feature_flags.pp)
      format_properties#.optimal_tiling_features
      (pp_opt Vkt.Format_feature_flags.pp) format_properties#.buffer_features
  let extent = (!) @@
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
      ~usage:Vkt.Image_usage_flags.(of_list
                                      [depth_stencil_attachment; transfer_src])
      ~sharing_mode:Vkt.Sharing_mode.Exclusive
      (** vvv FIXME vvv: this can only be PREINIALIZED or UNDEFINED *)
      ~initial_layout:Vkt.Image_layout.Undefined
      ()
  let image = Vkc.create_image device info () <!> "Depth image creation"

  let reqr = Vkc.get_image_memory_requirements device image
  let size = reqr#.Vkt.Memory_requirements.size
  ;; debug "Depth buffer size:%d" (Vkt.Device_size.to_int size)

  let allocate_info =
    Vkt.Memory_allocate_info.make
      ~allocation_size:size
      ~memory_type_index:0 ()

  let memory = Vkc.allocate_memory device allocate_info () <!> "Depth memory"
  let () = Vkc.bind_image_memory device image memory zero_offset
           <!!> "Depth binding"

  let view =
    let aspect = Vkt.Image_aspect_flags.(of_list [depth]) in
    Vkc.create_image_view device (Image.image_view_info aspect format image) ()
    <!> "Depth view"

  let one_time  queue command_pool f =
    let alloc =
      Vkt.Command_buffer_allocate_info.make
        ~command_pool ~level:Vkt.Command_buffer_level.Primary
        ~command_buffer_count:1 () in
    let buffers =
      Vkc.allocate_command_buffers device alloc <!> "one time buffer"  in
    let b = A.get buffers 0 in
    let begin_info = Vkt.Command_buffer_begin_info.make
        ~flags:Vkt.Command_buffer_usage_flags.(singleton one_time_submit) () in
    Vkc.begin_command_buffer b begin_info <!!> "Begin one-time command";
    f b;
    Vkc.end_command_buffer b <!!> "End one time-command";
    let submits = A.from_ptr
        begin Vkt.Submit_info.make
        (* vvvv FIXME: wait_dst_stage_mask should be zipped
            with wait_semaphores *)
        ~wait_dst_stage_mask:(nullptr Vkt.pipeline_stage_flags)
        ~command_buffers:buffers ()
    end 1 in
    Vkc.queue_submit ~queue ~submits () <!!> "Submit one-time command";
    Vkc.queue_wait_idle queue <!!> "Wait end one-time command";
    debug "Transition done";
    Vkc.free_command_buffers device command_pool buffers

  let transition b =
    let subresource_range =
      Image.subresource_range Vkt.Image_aspect_flags.(of_list [depth])
    in
    let dst_access_mask= let open Vkt.Access_flags in
      of_list [depth_stencil_attachment_read; depth_stencil_attachment_write]
    in
    let barrier = Vkt.Image_memory_barrier.make
        ~old_layout:Vkt.Image_layout.Undefined
        ~new_layout:Vkt.Image_layout.Depth_stencil_attachment_optimal
        ~dst_access_mask
        ~src_queue_family_index:0 ~dst_queue_family_index:0
        ~image ~subresource_range ()
    in
    let stage = Vkt.Pipeline_stage_flags.(singleton top_of_pipe) in
    Vkc.cmd_pipeline_barrier
      ~command_buffer:b ~src_stage_mask:stage ~dst_stage_mask:stage
      ~image_memory_barriers:(A.from_ptr barrier 1) ()

end

module Pipeline = struct
  (** Create a graphical pipeline *)

  module Shaders = struct

    let frag = read_spirv "shaders/tesseract/frag.spv"
    let vert = read_spirv "shaders/tesseract/vert.spv"

    let shader_module_info s =
      let len = String.length s in
      let c = A.make Vkt.uint_32_t (len / Ctypes.(sizeof uint32_t)) in
      let c' =
        A.from_ptr Ctypes.(coerce (ptr Vkt.uint_32_t) (ptr char) @@ A.start c) len in
      String.iteri (fun n x -> A.set c' n x) s;
      Vkt.Shader_module_create_info.make
        ~code_size:(Unsigned.Size_t.of_int len)
        ~code:(A.start c)
        ()

    let create_shader name s =
      let info = shader_module_info s in
      Vkc.create_shader_module device info () <!> "Shader creation :" ^ name

    let frag_shader = create_shader "fragment" frag
    let vert_shader = create_shader "vertex" vert

    let make_stage stage module' =
      Vkt.Pipeline_shader_stage_create_info.make
        ~flags: Vkt.Pipeline_shader_stage_create_flags.empty
        ~stage
        ~module'
        ~name: "main"
        ()

    let frag_stage = make_stage Vkt.Shader_stage_flags.fragment frag_shader
    let vert_stage = make_stage Vkt.Shader_stage_flags.vertex vert_shader
  end

  let u () = Random.float 1.
  let c () = u (), u (), u ()
  let vec (r,g,b) is a k =
    let k = 6 * k in
    A.set a k r;
    A.set a (k+1) g;
    A.set a (k+2) b;
    List.iter (fun i  -> A.set a (k+3+i) 0.5) is
  let face l i j a k =
    let k = 6 * k in
    let c = c () in
    List.iteri (fun v is -> vec c is a @@ k + v)
    [ l; i::l ; [j] @ l ; [i;j] @ l ; [j] @ l ; [i] @ l ]

  let faces = [face [] 0 1; face [] 0 2; face [] 1 2;
               face [1] 2 0; face [0] 1 2; face [2] 0 1]
  let nfaces = List.length faces
  let input =
    let a = A.make Ctypes.float (36 * nfaces) ~initial:0. in
    List.iteri (fun i f -> f a i) faces;
    a

  let pp_input ppf a=
    for k = 0 to A.length a/ 6 - 1 do
      Format.fprintf ppf "@[";
        for i = 0 to 2 do
          Format.fprintf ppf "%f " @@ A.get a (6 * k + i)
        done;
      Format.fprintf ppf "|";
      for i = 0 to 2 do
        Format.fprintf ppf " %f" @@ A.get a (6*k + i + 3)
      done;
        Format.fprintf ppf "@]@."
    done

  ;; debug "Input:@;%a" pp_input input
  let fsize = Ctypes.(sizeof float)

  let vertex_binding =
    Vkt.Vertex_input_binding_description.make
      ~binding:0
      ~stride:(6 * fsize)
      ~input_rate:Vkt.Vertex_input_rate.Vertex

  let attributes = A.make Vkt.vertex_input_attribute_description 2

  let geom_attribute =
    Vkt.Vertex_input_attribute_description.make
      ~location:0 ~binding:0 ~format:Vkt.Format.R32g32b_32_sfloat
      ~offset:(3 * fsize)

  let color_attribute =
    Vkt.Vertex_input_attribute_description.make
      ~location:1 ~binding:0 ~format:Vkt.Format.R32g32b_32_sfloat
      ~offset:0

  ;; A.set attributes 0 !geom_attribute
  ;; A.set attributes 1 !color_attribute

  let bindings = A.from_ptr vertex_binding 1

  let mem_size = Vkt.Device_size.of_int @@ fsize * A.length input

  let pp_mem ppf m = let open Vkt.Physical_device_memory_properties in
    A.iter (fun mt ->
        pp_opt Vkt.Memory_property_flags.pp ppf
          mt#.Vkt.Memory_type.property_flags;
        Format.print_cut ()
      ) m#.memory_types

  let create_buffer flag mem_size =
    let buffer_info =
      Vkt.Buffer_create_info.make
        ~size: mem_size
        ~usage:Vkt.Buffer_usage_flags.(singleton flag)
        ~sharing_mode:Vkt.Sharing_mode.Exclusive
        () in
    let buffer = Vkc.create_buffer device buffer_info () <!> "Buffer creation" in

    let memory_rqr = Vkc.get_buffer_memory_requirements ~device ~buffer in
    let phymem = Vkc.get_physical_device_memory_properties Device.phy in
    debug "memory flags, %a" pp_mem phymem;

    let alloc_info =
      Vkt.Memory_allocate_info.make
        ~allocation_size:memory_rqr#.Vkt.Memory_requirements.size
        ~memory_type_index:0
        () in

    let buffer_memory = Vkc.allocate_memory device alloc_info ()
                        <!> "Buffer memory allocation" in
    let () =
      Vkc.bind_buffer_memory device buffer buffer_memory zero_offset
      <!!> "Bind buffer to buffer datatypes" in
    buffer, buffer_memory

  let buffer, buffer_memory =
    create_buffer Vkt.Buffer_usage_flags.vertex_buffer mem_size

  let () =
    let len = A.length input in
    let mapped_mem =
      Vkc.map_memory device buffer_memory zero_offset mem_size ()
      <!> "Memory mapped" in
    let a = A.from_ptr Ctypes.(coerce (ptr void) (ptr float) mapped_mem) len in
    for i = 0 to len - 1 do
      A.set a i (A.get input i)
    done;
    Vkc.unmap_memory device buffer_memory



module Uniform = struct

  let binding =
    Vkt.Descriptor_set_layout_binding.make
      ~binding:0  ~descriptor_count:1
      ~descriptor_type:Vkt.Descriptor_type.Uniform_buffer
      ~stage_flags:Vkt.Shader_stage_flags.(singleton vertex) ()

  let bindings =
    A.from_ptr binding 1

  let layout_info =
    Vkt.Descriptor_set_layout_create_info.make ~bindings ()

  let layout = Vkc.create_descriptor_set_layout device layout_info ()
               <!> "Descriptor layout creation"

  let layouts = A.of_list Vkt.descriptor_set_layout [layout]

  let size = Vkt.Device_size.of_int ( 4 * 4 * fsize )
  let buffer, memory =
    create_buffer Vkt.Buffer_usage_flags.uniform_buffer size

  let pool_sizes = let open Vkt.Descriptor_pool_size in
    A.from_ptr (make Vkt.Descriptor_type.Uniform_buffer 1) 1

  let pool_info =
    Vkt.Descriptor_pool_create_info.make ~max_sets:1 ~pool_sizes ()

  let pool =
    Vkc.create_descriptor_pool device pool_info () <!> "Descriptor pool"

  let alloc =
    Vkt.Descriptor_set_allocate_info.make pool layouts ()

  let descriptor_sets=
    Vkc.allocate_descriptor_sets device alloc <!> "Descriptor set"

  ;; debug "Allocated %d descriptor sets" (A.length descriptor_sets)

  let buffer_info = Vkt.Descriptor_buffer_info.make buffer zero_offset size
  let write_info =
    A.from_ptr begin
      Vkt.Write_descriptor_set.make
        ~dst_set:(A.get descriptor_sets 0) ~dst_binding:0 ~descriptor_count:1
        ~dst_array_element:0
      ~descriptor_type:Vkt.Descriptor_type.Uniform_buffer
      (* TODO: the three fields belows corresponds to the
         constructors of a sum type:
         descriptor_type is the sum flag, and only on of  *)
      ~buffer_info
      ~texel_buffer_view:(nullptr Vkt.buffer_view)
      ~image_info:(nullptr Vkt.descriptor_image_info) ()
      end 1

    let transfer matrix =
    let m = Vkc.map_memory device memory zero_offset size () <*> "map memory" in
    let typed = Ctypes.(coerce (ptr void) (ptr float) m) in
    let a = A.from_ptr typed (4*4)  in
    Vec.blit_to ~from:matrix ~to':a;
    Vkc.unmap_memory device memory

end
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
      ~max_depth: 1.

  let scissor =
    Vkt.Rect_2d.make
      ~offset: Vkt.Offset_2d.(!(make ~x:0l ~y:0l))
      ~extent:Image.extent

  let viewports = A.from_ptr viewport 1
  let scissors = A.from_ptr scissor 1

  let viewport_state =
    Vkt.Pipeline_viewport_state_create_info.make
      ~viewports ~scissors ()

  let rasterizer =
    Vkt.Pipeline_rasterization_state_create_info.make
      ~depth_clamp_enable: false
      ~rasterizer_discard_enable: false
      ~polygon_mode: Vkt.Polygon_mode.Fill
      ~cull_mode: Vkt.Cull_mode_flags.(singleton front)
      ~front_face: Vkt.Front_face.Clockwise
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
     ~color_write_mask: Vkt.Color_component_flags.(of_list[r;g;b;a])
     ~src_color_blend_factor: Vkt.Blend_factor.One
     ~dst_color_blend_factor: Vkt.Blend_factor.Zero
     ~color_blend_op: Vkt.Blend_op.Add
     ~src_alpha_blend_factor: Vkt.Blend_factor.One
     ~dst_alpha_blend_factor: Vkt.Blend_factor.Zero
     ~alpha_blend_op: Vkt.Blend_op.Add
     ()

  let depth_info =
    let st = Ctypes.make Vkt.stencil_op_state in
    Vkt.Pipeline_depth_stencil_state_create_info.make
      ~depth_test_enable:true
      ~depth_write_enable:true
      ~depth_compare_op:Vkt.Compare_op.Less
      (* Fixme vv Option group vv *)
      ~depth_bounds_test_enable:false
      ~min_depth_bounds:0.
      ~max_depth_bounds:1.
      (* Fixme vv Option group vv *)
      ~stencil_test_enable:false
      ~front:st
      ~back:st
      ()

  let blend_state_info =
    let attachments =
      A.of_list Vkt.pipeline_color_blend_attachment_state [!no_blend] in
    let consts = snd @@ from_array Ctypes.float [| 0.; 0.; 0.; 0. |] in
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
    <!> "Creating pipeline layout"

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

  let dependencies = A.from_ptr begin
    let stage = Vkt.Pipeline_stage_flags.(singleton color_attachment_output) in
    Vkt.Subpass_dependency.make
      ~src_subpass:(Unsigned.UInt.to_int Vk.Const.subpass_external)
      ~dst_subpass:0
      ~src_stage_mask:stage
      ~dst_stage_mask:stage
      ~dst_access_mask:
        Vkt.Access_flags.(of_list [color_attachment_read;
                                   color_attachment_write])
      () end 1

  let subpass =
    let color = A.from_ptr color_attachment 1 in
    Vkt.Subpass_description.make
      ~pipeline_bind_point: Vkt.Pipeline_bind_point.Graphics
      ~color_attachments: color
      ~depth_stencil_attachment
      ()

  let render_pass_info =
    let attachments = A.of_list Vkt.attachment_description
        [!color_description; !depth_description] in
    let subpasses = A.from_ptr subpass 1 in
    Vkt.Render_pass_create_info.make
      ~attachments ~subpasses ~dependencies ()

  let simple_render_pass =
    Vkc.create_render_pass device render_pass_info () <!> "Creating render pass"

  let pipeline_info =
    let stages = A.of_list Vkt.pipeline_shader_stage_create_info
        Shaders.[ !vert_stage; !frag_stage ] in
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
      ~base_pipeline_index: 0l
      ()

  let pipeline_infos = A.from_ptr pipeline_info 1

  let pipelines =
    Vkc.create_graphics_pipelines device pipeline_infos ()
    <!> "Graphics pipeline creation"

  let x = A.get pipelines 0

end


module Cmd = struct

  ;; Vkc.update_descriptor_sets ~device
    ~descriptor_writes:Pipeline.Uniform.write_info ()


  let images =
    Array.init 2 ( fun i ->
        A.of_list Vkt.image_view [A.get Image.views i; Depth.view])
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
    <!> "Framebuffer creation"

  let queue =
    Vkc.get_device_queue device Device.queue_family 0

  let command_pool_info =
    Vkt.Command_pool_create_info.make
      ~queue_family_index: Device.queue_family
      ()

  let command_pool =
    Vkc.create_command_pool device command_pool_info ()
    <!> "Command pool creation"


  let () = (* initialize depth buffer *)
    Depth.one_time queue command_pool Depth.transition
  let framebuffers =
    A.of_list Vkt.framebuffer [framebuffer 0; framebuffer 1]

  let n_cmd_buffers =  (A.length framebuffers)
  let buffer_allocate_info =
    Vkt.Command_buffer_allocate_info.make
      ~command_pool
      ~level: Vkt.Command_buffer_level.Primary
      ~command_buffer_count: n_cmd_buffers
      ()

  let cmd_buffers =
    Vkc.allocate_command_buffers device buffer_allocate_info
    <!> "Command buffers allocation"

  ;;debug "Created %d cmd buffers" n_cmd_buffers

  let cmd_begin_info =
    Vkt.Command_buffer_begin_info.make
      ~flags: Vkt.Command_buffer_usage_flags.(singleton simultaneous_use)
      ()

  let clear_colors =
    let open Vkt.Clear_value in
    let x = Ctypes.make t in
    let c = Ctypes.make Vkt.clear_color_value in
    let a = A.of_list Ctypes.float [ 0.;1.;0.; 1.] in
    set c Vkt.Clear_color_value.float_32 @@ A.start a;
    set x color c;
    x

  let clear_depths =
    let open Vkt.Clear_value in
    let x = Ctypes.make t in
    let f = Vkt.Clear_depth_stencil_value.make ~depth:1. ~stencil:0 in
    set x depth_stencil !f;
    x

  let clear_values =
    A.of_list Vkt.clear_value [clear_colors;clear_depths; clear_depths]

  let render_pass_info fmb =
    Vkt.Render_pass_begin_info.make
      ~render_pass: Pipeline.simple_render_pass
      ~framebuffer: fmb
      ~render_area: !Pipeline.scissor
      ~clear_values
      ()

  let render_pass_infos = A.map (Ctypes.ptr Vkt.render_pass_begin_info)
      render_pass_info framebuffers
  let vertex_buffers = A.of_list Vkt.buffer [Pipeline.buffer]
  let offsets = A.of_list Vkt.device_size Vkt.Device_size.[of_int 0]
  let cmd b ifmb =
    Vkc.begin_command_buffer b cmd_begin_info <!!> "Begin command buffer";
    Vkc.cmd_begin_render_pass b (A.get render_pass_infos ifmb)
      Vkt.Subpass_contents.Inline;
    Vkc.cmd_bind_pipeline b Vkt.Pipeline_bind_point.Graphics Pipeline.x;
    Vkc.cmd_bind_vertex_buffers b 0 vertex_buffers (A.start offsets);
    Vkc.cmd_bind_descriptor_sets ~command_buffer:b
      ~pipeline_bind_point:Vkt.Pipeline_bind_point.Graphics
      ~layout:Pipeline.layout ~first_set:0
      ~descriptor_sets: Pipeline.Uniform.descriptor_sets ();
    Vkc.cmd_draw b (6 * Pipeline.nfaces) 1 0 0;
    Vkc.cmd_end_render_pass b;
    Vkc.end_command_buffer b <!!> "Command buffer recorded"

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
    Vkc.create_semaphore device semaphore_info () <!> "Created semaphore"

  let im_semaphore = create_semaphore ()
  let render_semaphore = create_semaphore ()

  let wait_sems = A.of_list Vkt.semaphore [im_semaphore]
  let sign_sems = A.of_list Vkt.semaphore [render_semaphore]


  let wait_stage = let open Vkt.Pipeline_stage_flags in
    Ctypes.allocate view @@ singleton color_attachment_output

  let submit_info _index (* CHECK-ME *) =
    A.from_ptr (
    Vkt.Submit_info.make
      ~wait_semaphores: wait_sems
      ~wait_dst_stage_mask: wait_stage
      ~command_buffers: Cmd.cmd_buffers
      ~signal_semaphores: sign_sems ()
      ) 1

  let swapchains = A.of_list Vkt.swapchain_khr [Image.swap_chain]

  let present_indices = Ctypes.allocate Vkt.uint_32_t 0
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
            <!> "Acquire image" in
    present_indices <-@ n;
    debug "Image %d acquired" n;
    let () = Pipeline.Uniform.transfer Vec.id in
    Vkc.queue_submit ~queue:Cmd.queue ~submits:(submit_info n) ()
    <!!> "Submitting command to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <??> "Image presented"

  let rec acquire_next () =
    match  Swapchain.acquire_next_image_khr ~device
             ~swapchain:Image.swap_chain
               ~timeout:Unsigned.UInt64.max_int ~semaphore:im_semaphore () with
      | Ok ((`Success|`Suboptimal_khr), n) -> n
      | Ok ((`Timeout|`Not_ready), _ ) -> acquire_next ()
      | Error x ->
        (Format.eprintf "Error %a in acquire_next" Vkt.Result.pp x; exit 2)


  let rotz = Vec.axis_rotation 0 1
  let rotx = Vec.axis_rotation 1 2
  let rot x y = Vec.( rotz y * rotx x)
  let u f = f *. (Random.float 2. -. 1.)
  let draw (speedx, anglex, speedz, anglez) =
    present_indices <-@ acquire_next ();
    let speedx = speedx +. u 0.001 in
    let anglex = speedx +. anglex in
    let speedz = speedz +. u 0.001 in
    let anglez = speedz +. anglez in
    let () = Pipeline.Uniform.transfer (rot anglex anglez) in
    Vkc.queue_submit ~queue:Cmd.queue ~submits:(submit_info !present_indices) ()
    <!!> "Submit to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <??> "Present to queue";
    (speedx,anglex,speedz,anglez)

end

;; Render.(debug_draw(); debug_draw ())
;; Sdl.(event_loop Render.draw (0., 0., 0., 0.)  e)
;; debug "End"
