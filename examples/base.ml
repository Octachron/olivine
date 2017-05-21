module A = Ctypes.CArray
module Vkt = Vk.Types
module Vkc = Vk.Core


module Utils = struct
  (** Ctype utility functions *)
  let get, set = Ctypes.(getf,setf)
  let null = Ctypes.null

  let pp_opt pp ppf = function
    | None -> ()
    | Some x -> pp ppf x

  let ($=) field value str= set str field value
  let ( #. ) = get

  let make typ updates =
    let str=Ctypes.make typ in
    List.iter (fun f -> f str) updates;
    str

  let mk_ptr typ updates = Ctypes.addr @@ make typ updates


  let debug fmt = Format.printf ("Debug: " ^^ fmt ^^ "@.")

  let (<?>) x s = match x with
    | Ok _ -> Format.printf "Success: %s@." s
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (!) = Ctypes.(!@)
  let (~:) = Unsigned.UInt32.of_int
  let to_int = Unsigned.UInt32.to_int
(*
  module Bool = Vkt.Bool_3_2
  let bool32 = Bool.ctype
  let true' = Bool.make ~: Vk.Const.true'
  let false' = Bool.make ~: Vk.Const.false'
*)

  let (+@) = Ctypes.(+@)
  let ( <-@ ) = Ctypes.( <-@ )

  let from_array typ a =
    let n = Array.length a in
    let a' = Ctypes.allocate_n ~count:n typ in
    Array.iteri (fun i x -> (a' +@ i) <-@ x ) a;
    n, a'

  let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)
  let to_array n p = Array.init n (fun i -> !(p +@ i) )
  let to_string carray =
    String.init (A.length carray)
      (fun n -> A.get carray n)

  let get_array msg elt f =
    let n = Ctypes.allocate Vkt.uint32_t_opt None in
    msg "count" @@ f n None;
    let count = match !n with
      | None -> 0 | Some n ->  n  in
    let e =
      Ctypes.allocate_n ~count elt in
    msg "allocation" (f n @@ Some e);
    to_array count e

  let msg name minor r =
    r <?> name ^ ":" ^ minor

  let silent _minor _r = ()

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
  let rec event_loop idle e =
    idle ();
    let open Sdl.Event in
    if
      Sdl.poll_event @@ Some e
      && get e typ = key_down
      && get e keyboard_keycode = Sdl.K.escape
    then
      exit 0
    else event_loop idle e

end

  let print_extension_property e =
    let open Vkt.Extension_properties in
    Format.printf "%s\n" (to_string e#.extension_name)

module Instance = struct
  (** Creating a vulkan instance *)

  let layers = A.of_list Ctypes.string []

  let extensions =
    A.of_list Ctypes.string ["VK_KHR_surface"; "VK_KHR_xlib_surface" ]

  let info =
    Vkt.Instance_create_info.make
      ~s_type: Vkt.Structure_type.Instance_create_info
      ~p_next: null
      ~flags: Vkt.Instance_create_flags.empty
      ~pp_enabled_layer_names: layers
      ~pp_enabled_extension_names: extensions
      ()

  ;; debug "Info created"

  let x =
    let x = Ctypes.allocate_n Vkt.instance 1 in
    debug "Instance pointer allocated";
    Vkc.create_instance info None x
    <?> "instance";
    !x

  let extension_properties =
    get_array (msg "Extension properties") Vkt.extension_properties
    @@ Vkc.enumerate_instance_extension_properties None

  ;; Array.iter print_extension_property extension_properties
end
let instance = Instance.x

(** Once an instance has been created, load the KHR extensions *)
module Surface = Vk.Khr.Surface(Instance)


module Device = struct
  let phy_devices =
    let d =
      get_array (msg "physical device") Vkt.physical_device
      @@ Vkc.enumerate_physical_devices instance in
    debug "Number of devices: %d \n" (Array.length d);
    d

  let property device =
    let p = Ctypes.make Vkt.physical_device_properties in
    debug "Device properties acquisition";
    Vkc.get_physical_device_properties device (Ctypes.addr p);
    debug "Device properties acquired";
    p

  let print_property device =
    let p = property device in
    debug "Device: %s\n"
      (to_string p#.Vkt.Physical_device_properties.device_name)

  ;; Array.iter print_property phy_devices

  let phy = phy_devices.(0)

  let queue_family_properties =
    get_array silent Vkt.queue_family_properties
    @@ Vkc.get_physical_device_queue_family_properties phy

  let print_queue_property ppf property =
    Format.fprintf ppf "Queue flags: %a \n" (pp_opt Vkt.Queue_flags.pp)
      property#.Vkt.Queue_family_properties.queue_flags

  ;; Array.iter (print_queue_property Format.std_formatter)
    queue_family_properties

  let queue_family = 0
  let priorities = A.of_list Ctypes.float [ 1.]

  let queue_create_info =
    Vkt.Device_queue_create_info.make
      ~s_type: Vkt.Structure_type.Device_queue_create_info
      ~p_next: null
      ~queue_family_index:queue_family
      ~p_queue_priorities: priorities
      ()

  let device_extensions =
    get_array (msg "device extensions")
      Vkt.extension_properties
      (Vkc.enumerate_device_extension_properties phy None)

  ;; Format.printf "Device extensions:\n@[<v 2>"
  ;; Array.iter print_extension_property device_extensions
  ;; Format.printf "@]@."

  let surface_khr =
    let s = Ctypes.allocate_n ~count:1 Vkt.surface_khr in
    Vk__sdl.create_surface instance Sdl.w None s
    <?> "Obtaining surface";
    !s

  let capabilities =
    let x = Ctypes.allocate_n Vkt.surface_capabilities_khr 1 in
    Surface.get_physical_device_surface_capabilities_khr phy surface_khr x
    <?> "Surface capabilities";
    !x

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
    get_array (msg "Supported surface format") Vkt.surface_format_khr @@
    Surface.get_physical_device_surface_formats_khr phy
      surface_khr

  let pp_sformat ppf sformat = let open Vkt.Surface_format_khr in
    Format.fprintf ppf "surface format @[{@ format=@[%a@];@ color_space=@[%a@]}"
      Vkt.Format.pp sformat#.format Vkt.Color_space_khr.pp sformat#.color_space

  ;; Array.iter (debug "%a" pp_sformat) supported_formats

  let present_modes =
    get_array (msg "Surface present modes") Vkt.present_mode_khr @@
    Surface.get_physical_device_surface_present_modes_khr phy surface_khr

  ;; Array.iter (debug "%a" Vkt.Present_mode_khr.pp) present_modes

  let support =
    let x = Ctypes.allocate Vkt.bool false in
    Surface.get_physical_device_surface_support_khr phy queue_family
      surface_khr x <?> "Compatibility surface/device";
    assert (!x = true )

  let x =
    let d = Ctypes.allocate_n Vkt.Device.t 1 in
    let layers = A.of_list Ctypes.string [] in
    let exts = A.of_list Ctypes.string ["VK_KHR_swapchain"] in
    let info =
      let queue_create_infos = A.from_ptr queue_create_info 1 in
      Vkt.Device_create_info.make
        ~s_type: Vkt.Structure_type.Device_create_info
        ~p_next: null
        ~p_queue_create_infos: queue_create_infos
        ~pp_enabled_layer_names: layers
        ~pp_enabled_extension_names: exts
      ()
      in
    Vkc.create_device phy info None d
    <?> "Create logical device";
    !d
end
let device = Device.x
let surface_khr = Device.surface_khr

module Swapchain = Vk.Khr.Swapchain(Device)

module Image = struct

  let surface_format = Device.supported_formats.(0)

  let format, colorspace =
    let open Vkt.Surface_format_khr in
    surface_format#.format, surface_format#.color_space

  let image_count, extent = let open Vkt.Surface_capabilities_khr in
    Device.capabilities#.min_image_count,
    Device.capabilities#. current_extent

  let swap_chain_info =
    let qfi = A.of_list Vkt.uint32_t [0] in
    Vkt.Swapchain_create_info_khr.make
      ~s_type: Vkt.Structure_type.Swapchain_create_info_khr
      ~p_next: null
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
      ~p_queue_family_indices: qfi
      ~pre_transform:
        Vkt.Surface_transform_flags_khr.identity
      ~composite_alpha:
        Vkt.Composite_alpha_flags_khr.opaque
      ~present_mode: Vkt.Present_mode_khr.Fifo
      ~clipped: true
      ~old_swapchain:Vkt.Swapchain_khr.null
      ()

  let swap_chain =
    let s = Ctypes.allocate_n ~count:1 Vkt.swapchain_khr in
    Swapchain.create_swapchain_khr device swap_chain_info None s
    <?> "swap_chain";
    !s

  let images =
    get_array (msg "Swapchain images") Vk.Types.image
    @@ Swapchain.get_swapchain_images_khr device swap_chain

  ;; debug "Swapchain: %d images" (Array.length images)

  let component_mapping =
    let id = Vkt.Component_swizzle.Identity in
    Vkt.Component_mapping.make ~r:id ~g:id ~b:id ~a:id

  let subresource_range =
    !(Vkt.Image_subresource_range.make
    ~aspect_mask:Vkt.Image_aspect_flags.(singleton color)
    ~base_mip_level: 0
    ~level_count: 1
    ~base_array_layer: 0
    ~layer_count: 1)

  let image_view_info im =
    Vkt.Image_view_create_info.make
      ~s_type: Vkt.Structure_type.Image_view_create_info
      ~p_next: null
      ~image: im
      ~view_type: Vkt.Image_view_type.N2d
      ~format
      ~subresource_range
      ~components:!(component_mapping)
      ()

  let views =
    let create im =
      let v = Ctypes.allocate_n Vkt.image_view 1 in
      Vkc.create_image_view device (image_view_info im) None v
      <?> "Creating image view";
      !v in
    Array.map create images

end

module Pipeline = struct
  (** Create a graphical pipeline *)

  module Shaders = struct

    let frag = read_spirv "shaders/frag.spv"
    let vert = read_spirv "shaders/vert.spv"

    let shader_module_info s =
      let len = String.length s in
      let c = A.make Vkt.uint32_t (len / Ctypes.(sizeof uint32_t)) in
      let c' =
        A.from_ptr Ctypes.(coerce (ptr Vkt.uint32_t) (ptr char) @@ A.start c) len in
      String.iteri (fun n x -> A.set c' n x) s;
      Vkt.Shader_module_create_info.make
        ~s_type: Vkt.Structure_type.Shader_module_create_info
        ~p_next: null
        ~code_size:(Unsigned.Size_t.of_int len)
        ~p_code:(A.start c)
        ()

    let create_shader name s =
      let info = shader_module_info s in
      let x = Ctypes.allocate_n Vkt.shader_module 1 in
      Vkc.create_shader_module device info None x
      <?> "Shader creation :" ^ name;
      !x

    let frag_shader = create_shader "fragment" frag
    let vert_shader = create_shader "vertex" vert

    let make_stage stage module' =
      Vkt.Pipeline_shader_stage_create_info.make
        ~s_type: Vkt.Structure_type.Pipeline_shader_stage_create_info
        ~p_next: null
        ~flags: Vkt.Pipeline_shader_stage_create_flags.empty
        ~stage
        ~module'
        ~p_name: "main"
        ()

    let frag_stage = make_stage Vkt.Shader_stage_flags.fragment frag_shader
    let vert_stage = make_stage Vkt.Shader_stage_flags.vertex vert_shader
  end

  let null_input =
    let vbd = A.of_list Vkt.vertex_input_binding_description [] in
    let vd = A.of_list Vkt.vertex_input_attribute_description [] in
    Vkt.Pipeline_vertex_input_state_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_vertex_input_state_create_info
      ~p_next: null
      ~p_vertex_binding_descriptions: vbd
      ~p_vertex_attribute_descriptions: vd
      ()

  let input_assembly =
    Vkt.Pipeline_input_assembly_state_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_input_assembly_state_create_info
      ~p_next: null
      ~flags: Vkt.Pipeline_input_assembly_state_create_flags.empty
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


  let viewport_state =
    Vkt.Pipeline_viewport_state_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_viewport_state_create_info
      ~p_next: null
      ~viewport_count: (1)
      ~scissor_count: (1)
      ~p_viewports: viewport
      ~p_scissors: scissor
      ()

  let rasterizer =
    Vkt.Pipeline_rasterization_state_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_rasterization_state_create_info
      ~p_next: null
      ~depth_clamp_enable: false
      ~rasterizer_discard_enable: false
      ~polygon_mode: Vkt.Polygon_mode.Fill
      ~cull_mode: Vkt.Cull_mode_flags.(singleton back)
      ~front_face: Vkt.Front_face.Clockwise
      ~depth_bias_enable: false
      ~depth_bias_constant_factor: 0.
      ~depth_bias_clamp: 0.
      ~depth_bias_slope_factor: 0.
      ~line_width: 1.
      ()

  let no_multisampling =
    Vkt.Pipeline_multisample_state_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_multisample_state_create_info
      ~p_next: null
      ~flags: Vkt.Pipeline_multisample_state_create_flags.empty
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


  let blend_state_info =
    let attachs =
      A.of_list Vkt.pipeline_color_blend_attachment_state [!no_blend] in
    let consts = snd @@ from_array Ctypes.float [| 0.; 0.; 0.; 0. |] in
    Vkt.Pipeline_color_blend_state_create_info.make
     ~s_type: Vkt.Structure_type.Pipeline_color_blend_state_create_info
     ~p_next: null
     ~logic_op_enable: false
     ~logic_op: Vkt.Logic_op.Copy
     ~p_attachments: attachs
     ~blend_constants: consts
     ()


  let no_uniform =
    let layouts = A.of_list Vkt.descriptor_set_layout [] in
    let pcr = A.of_list Vkt.push_constant_range [] in
    Vkt.Pipeline_layout_create_info.make
      ~s_type: Vkt.Structure_type.Pipeline_layout_create_info
      ~p_next: null
      ~p_set_layouts: layouts
      ~p_push_constant_ranges: pcr
      ()

  let simple_layout =
    let x = Ctypes.allocate_n Vkt.pipeline_layout 1 in
    Vkc.create_pipeline_layout device no_uniform None x
    <?> "Creating pipeline layout";
    !x

  let color_attachment =
    Vkt.Attachment_description.make
      ~format: Image.format
      ~samples: Vkt.Sample_count_flags.n1
      ~load_op: Vkt.Attachment_load_op.Clear
      ~store_op: Vkt.Attachment_store_op.Store
      ~stencil_load_op: Vkt.Attachment_load_op.Dont_care
      ~stencil_store_op: Vkt.Attachment_store_op.Dont_care
      ~initial_layout: Vkt.Image_layout.Undefined
      ~final_layout: Vkt.Image_layout.Present_src_khr
      ()

  let attachment =
    Vkt.Attachment_reference.make
      ~attachment: 0
      ~layout: Vkt.Image_layout.Color_attachment_optimal

  let subpass =
    let color = A.from_ptr attachment 1 in
    let input = A.of_list Vkt.attachment_reference [] in
    let preserve = A.of_list Vkt.uint32_t [] in
    Vkt.Subpass_description.make
      ~pipeline_bind_point: Vkt.Pipeline_bind_point.Graphics
      ~p_color_attachments: color
      ~p_input_attachments: input
      ~p_preserve_attachments: preserve
      ()

  let render_pass_info =
    let attachments = A.from_ptr color_attachment 1 in
    let subpasses = A.from_ptr subpass 1 in
    let deps = A.of_list Vkt.subpass_dependency [] in
    Vkt.Render_pass_create_info.make
      ~s_type: Vkt.Structure_type.Render_pass_create_info
      ~p_next: null
      ~p_attachments: attachments
      ~p_subpasses: subpasses
      ~p_dependencies: deps
      ()

  let simple_render_pass =
    let x = Ctypes.allocate_n Vkt.render_pass 1 in
    Vkc.create_render_pass device render_pass_info None x
    <?> "Creating render pass";
    !x

  let pipeline_info =
    let stages = A.of_list Vkt.pipeline_shader_stage_create_info
        Shaders.[ !vert_stage; !frag_stage ] in
    Vkt.Graphics_pipeline_create_info.make
      ~s_type: Vkt.Structure_type.Graphics_pipeline_create_info
      ~p_next: null
      ~p_stages: stages
      ~p_vertex_input_state: null_input
      ~p_input_assembly_state: input_assembly
      ~p_viewport_state: viewport_state
      ~p_rasterization_state: rasterizer
      ~p_multisample_state: no_multisampling
      ~p_color_blend_state: blend_state_info
      ~layout: simple_layout
      ~render_pass: simple_render_pass
      ~subpass: 0
      ~base_pipeline_index: 0l
      ()


  let x =
    debug "Pipeline creation";
    let x = Ctypes.allocate_n Vkt.pipeline 1 in
    Vkc.create_graphics_pipelines device None 1
      pipeline_info None x
    <?> "Graphics pipeline creation";
    !x

end

module Cmd = struct

  let framebuffer_info image =
    let images = A.of_list Vkt.image_view [image] in
    Vkt.Framebuffer_create_info.make
      ~s_type: Vkt.Structure_type.Framebuffer_create_info
      ~p_next: null
      ~render_pass: Pipeline.simple_render_pass
      ~p_attachments: images
      ~width: Image.extent#.Vkt.Extent_2d.width
      ~height: Image.extent#.Vkt.Extent_2d.height
      ~layers:  1
      ()

  let framebuffer index =
    let x  = Ctypes.allocate_n Vkt.framebuffer 1 in
    Vkc.create_framebuffer device (framebuffer_info index) None
      x <?> "Framebuffer creation";
    !x

  let framebuffers = A.of_list Vkt.framebuffer @@ Array.to_list
    @@ Array.map framebuffer Image.views

  let my_fmb = framebuffer

  let queue =
    let x = Ctypes.allocate_n Vkt.queue 1 in
    Vkc.get_device_queue device Device.queue_family 0 x;
    !x

  let command_pool_info =
    Vkt.Command_pool_create_info.make
      ~s_type: Vkt.Structure_type.Command_pool_create_info
      ~p_next: null
      ~queue_family_index: Device.queue_family
      ()

  let command_pool =
    let x  = Ctypes.allocate_n Vkt.Command_pool.t 1 in
    Vkc.create_command_pool device command_pool_info None x
    <?> "Command pool creation";
    !x

  let my_cmd_pool = command_pool
  let n_cmd_buffers =  (A.length framebuffers)
  let buffer_allocate_info =
    Vkt.Command_buffer_allocate_info.make
      ~s_type: Vkt.Structure_type.Command_buffer_allocate_info
      ~p_next: null
      ~command_pool: my_cmd_pool
      ~level: Vkt.Command_buffer_level.Primary
      ~command_buffer_count: n_cmd_buffers

  let cmd_buffers =
    let n = n_cmd_buffers in
    let x = A.make Vkt.command_buffer n in
    Vkc.allocate_command_buffers device buffer_allocate_info @@ A.start x
    <?> "Command buffers allocation";
    x

  ;;debug "Created %d cmd buffers" n_cmd_buffers

  let cmd_begin_info =
    Vkt.Command_buffer_begin_info.make
      ~s_type: Vkt.Structure_type.Command_buffer_begin_info
      ~p_next: null
      ~flags: Vkt.Command_buffer_usage_flags.(singleton simultaneous_use)
      ()

  let clear_values =
    let open Vkt.Clear_value in
    let x = Ctypes.make t in
    let c = Ctypes.make Vkt.clear_color_value in
    let a = A.of_list Ctypes.float [ 0.;0.;0.; 1.] in
    set c Vkt.Clear_color_value.float_3_2 @@ A.start a;
    set x color c;
    x

  let render_pass_info fmb =
    let clear_values = A.of_list Vkt.clear_value [clear_values] in
    Vkt.Render_pass_begin_info.make
      ~s_type: Vkt.Structure_type.Render_pass_begin_info
      ~p_next: null
      ~render_pass: Pipeline.simple_render_pass
      ~framebuffer: fmb
      ~render_area: !Pipeline.scissor
      ~p_clear_values: clear_values

  let cmd b fmb =
    Vkc.begin_command_buffer b cmd_begin_info <?> "Begin command buffer";
    Vkc.cmd_begin_render_pass b (render_pass_info fmb)
      Vkt.Subpass_contents.Inline;
    Vkc.cmd_bind_pipeline b Vkt.Pipeline_bind_point.Graphics Pipeline.x;
    Vkc.cmd_draw b 3 1 0 0;
    Vkc.cmd_end_render_pass b;
    Vkc.end_command_buffer b <?> "Command buffer recorded"

  let iter2 f a b =
    for i=0 to min (A.length a) (A.length b) - 1 do
      f (A.get a i) (A.get b i)
    done

  let () = iter2 cmd cmd_buffers framebuffers
end

module Render = struct

  let semaphore_info =
    Vkt.Semaphore_create_info.make
      ~s_type: Vkt.Structure_type.Semaphore_create_info
      ~p_next: null
      ()

  let create_semaphore () =
    let x = Ctypes.allocate_n Vkt.semaphore 1 in
    Vkc.create_semaphore device semaphore_info None x
    <?> "Created semaphore";
    x

  let im_semaphore = create_semaphore ()
  let render_semaphore = create_semaphore ()

  let wait_stage = let open Vkt.Pipeline_stage_flags in
    Ctypes.allocate view @@ singleton top_of_pipe

  let submit_info index =
    let wait_sems = A.from_ptr im_semaphore 1 in
    let sign_sems = A.from_ptr render_semaphore 1 in
    Vkt.Submit_info.make
      ~s_type: Vkt.Structure_type.Submit_info
      ~p_next: null
      ~p_wait_semaphores: wait_sems
      ~p_wait_dst_stage_mask: wait_stage
      ~p_command_buffers: Cmd.cmd_buffers
      ~p_signal_semaphores: sign_sems

  let swapchains = A.of_list Vkt.swapchain_khr [Image.swap_chain]
  let present_info index =
    Vkt.Present_info_khr.make
      ~s_type: Vkt.Structure_type.Present_info_khr
      ~p_next: null
      ~wait_semaphore_count: 1
      ~p_wait_semaphores: render_semaphore
      ~p_swapchains: swapchains
      ~p_image_indices: index
      ()

  let debug_draw () =
    let n = Ctypes.allocate Vkt.uint32_t 0 in
    Swapchain.acquire_next_image_khr device Image.swap_chain
      Unsigned.UInt64.max_int (Some !im_semaphore) None n
    <?> "Acquire image";
    debug "Image %d acquired" !n;
    Vkc.queue_submit Cmd.queue (Some 1) (submit_info !n) None
    <?> "Submitting command to queue";
    Swapchain.queue_present_khr Cmd.queue (present_info n)
    <?> "Image presented"

  let draw () =
        let n = Ctypes.allocate Vkt.uint32_t 0 in
    Swapchain.acquire_next_image_khr device Image.swap_chain
      Unsigned.UInt64.max_int (Some !im_semaphore) None n
    |> ignore;
    Vkc.queue_submit Cmd.queue (Some 1) (submit_info !n) None
    |> ignore;
    Swapchain.queue_present_khr Cmd.queue (present_info n)
    |> ignore

end

;; Render.(debug_draw(); debug_draw ())
;; Sdl.(event_loop Render.draw e)
;; debug "End"
