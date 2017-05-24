module A = Ctypes.CArray
module Vkt = Vk.Types
module Vkc = Vk.Core
module Vkr = Vk.Raw
module Vkw = Vk__sdl

module Utils = struct
  (** Ctype utility functions *)
  let get, set = Ctypes.(getf,setf)
  let null = Ctypes.null

  let pp_opt pp ppf = function
    | None -> ()
    | Some x -> pp ppf x

  let ( #. ) = get

  let debug fmt = Format.printf ("Debug: " ^^ fmt ^^ "@.")

  let (<?>) x s = match x with
    | Ok x -> Format.printf "%a: %s@." Vkt.Result.pp x s
    | Error k ->
      Format.eprintf "Error %a: %s @."
        Vkt.Result.pp k s; exit 1

  let (<!>) x s = match x with
    | Ok (r, x) -> Format.printf "%a: %s@." Vkt.Result.pp r s; x
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

  let extensions =
    A.of_list Ctypes.string ["VK_KHR_surface"; "VK_KHR_xlib_surface" ]

  let info =
    Vkt.Instance_create_info.make
      ~flags: Vkt.Instance_create_flags.empty
      ~pp_enabled_extension_names: extensions
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
      ~p_queue_priorities: priorities
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
        ~p_queue_create_infos: queue_create_infos
        (*  ~pp_enabled_layer_names: layers *)
        ~pp_enabled_extension_names: exts
      ()
      in
    Vkc.create_device phy info () <!> "Create logical device"

end
let device = Device.x
let surface_khr = Device.surface_khr

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
    Swapchain.create_swapchain_khr device swap_chain_info ()
    <!> "swap chain creation"

  let images =
    Swapchain.get_swapchain_images_khr device swap_chain
    <!> "Swapchain images"

  ;; debug "Swapchain: %d images" (A.length images)

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
      ~image: im
      ~view_type: Vkt.Image_view_type.N2d
      ~format
      ~subresource_range
      ~components:!(component_mapping)
      ()

  let views =
    let create im =
      Vkc.create_image_view device (image_view_info im) ()
      <!> "Creating image view" in
    A.map Vkt.image_view create images

end

module Pipeline = struct
  (** Create a graphical pipeline *)

  module Shaders = struct

    let frag = read_spirv "shaders/frag.spv"
    let vert = read_spirv "shaders/vert.spv"

    let shader_module_info s =
      let len = String.length s in
      let c = A.make Vkt.uint_32_t (len / Ctypes.(sizeof uint32_t)) in
      let c' =
        A.from_ptr Ctypes.(coerce (ptr Vkt.uint_32_t) (ptr char) @@ A.start c) len in
      String.iteri (fun n x -> A.set c' n x) s;
      Vkt.Shader_module_create_info.make
        ~code_size:(Unsigned.Size_t.of_int len)
        ~p_code:(A.start c)
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
        ~p_name: "main"
        ()

    let frag_stage = make_stage Vkt.Shader_stage_flags.fragment frag_shader
    let vert_stage = make_stage Vkt.Shader_stage_flags.vertex vert_shader
  end

  let null_input =
    Vkt.Pipeline_vertex_input_state_create_info.make ()

  let input_assembly =
    Vkt.Pipeline_input_assembly_state_create_info.make
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

  let viewports = A.from_ptr viewport 1
  let scissors = A.from_ptr scissor 1

  let viewport_state =
    Vkt.Pipeline_viewport_state_create_info.make
      ~p_viewports: viewports
      ~p_scissors: scissors
      ()

  let rasterizer =
    Vkt.Pipeline_rasterization_state_create_info.make
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
     ~logic_op_enable: false
     ~logic_op: Vkt.Logic_op.Copy
     ~p_attachments: attachs
     ~blend_constants: consts
     ()


  let no_uniform =
    Vkt.Pipeline_layout_create_info.make ()

  let simple_layout =
    Vkc.create_pipeline_layout device no_uniform ()
    <!> "Creating pipeline layout"

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
    Vkt.Subpass_description.make
      ~pipeline_bind_point: Vkt.Pipeline_bind_point.Graphics
      ~p_color_attachments: color
      ()

  let render_pass_info =
    let attachments = A.from_ptr color_attachment 1 in
    let subpasses = A.from_ptr subpass 1 in
    Vkt.Render_pass_create_info.make
      ~p_attachments: attachments
      ~p_subpasses: subpasses
      ()

  let simple_render_pass =
    Vkc.create_render_pass device render_pass_info () <!> "Creating render pass"

  let pipeline_info =
    let stages = A.of_list Vkt.pipeline_shader_stage_create_info
        Shaders.[ !vert_stage; !frag_stage ] in
    Vkt.Graphics_pipeline_create_info.make
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

  let pipeline_infos = A.from_ptr pipeline_info 1

  let pipelines =
    Vkc.create_graphics_pipelines device pipeline_infos ()
    <!> "Graphics pipeline creation"

  let x = A.get pipelines 0

end

module Cmd = struct

  let framebuffer_info image =
    let images = A.of_list Vkt.image_view [image] in
    Vkt.Framebuffer_create_info.make
      ~render_pass: Pipeline.simple_render_pass
      ~p_attachments: images
      ~width: Image.extent#.Vkt.Extent_2d.width
      ~height: Image.extent#.Vkt.Extent_2d.height
      ~layers:  1
      ()

  let framebuffer index =
    Vkc.create_framebuffer device (framebuffer_info index) ()
    <!> "Framebuffer creation"

  let framebuffers = A.map Vkt.framebuffer framebuffer Image.views

  let my_fmb = framebuffer

  let queue =
    Vkc.get_device_queue device Device.queue_family 0

  let command_pool_info =
    Vkt.Command_pool_create_info.make
      ~queue_family_index: Device.queue_family
      ()

  let command_pool =
    Vkc.create_command_pool device command_pool_info ()
    <!> "Command pool creation"

  let my_cmd_pool = command_pool
  let n_cmd_buffers =  (A.length framebuffers)
  let buffer_allocate_info =
    Vkt.Command_buffer_allocate_info.make
      ~command_pool: my_cmd_pool
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

  let clear_values =
    let open Vkt.Clear_value in
    let x = Ctypes.make t in
    let c = Ctypes.make Vkt.clear_color_value in
    let a = A.of_list Ctypes.float [ 0.;0.;0.; 1.] in
    set c Vkt.Clear_color_value.float_32 @@ A.start a;
    set x color c;
    x

  let render_pass_info fmb =
    let clear_values = A.of_list Vkt.clear_value [clear_values] in
    Vkt.Render_pass_begin_info.make
      ~render_pass: Pipeline.simple_render_pass
      ~framebuffer: fmb
      ~render_area: !Pipeline.scissor
      ~p_clear_values: clear_values
      ()

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
    Vkt.Semaphore_create_info.make ()

  let create_semaphore () =
    Vkc.create_semaphore device semaphore_info () <!> "Created semaphore"

  let im_semaphore = create_semaphore ()
  let render_semaphore = create_semaphore ()

  let wait_sems = A.of_list Vkt.semaphore [im_semaphore]
  let sign_sems = A.of_list Vkt.semaphore [render_semaphore]


  let wait_stage = let open Vkt.Pipeline_stage_flags in
    Ctypes.allocate view @@ singleton top_of_pipe

  let submit_info _index (* CHECK-ME *) =
    A.from_ptr (
    Vkt.Submit_info.make
      ~p_wait_semaphores: wait_sems
      ~p_wait_dst_stage_mask: wait_stage
      ~p_command_buffers: Cmd.cmd_buffers
      ~p_signal_semaphores: sign_sems ()
      ) 1

  let swapchains = A.of_list Vkt.swapchain_khr [Image.swap_chain]

  let present_indices = Ctypes.allocate Vkt.uint_32_t 0
  (* Warning need to be alive as long as present_info can be used! *)

  let present_info =
    Vkt.Present_info_khr.make
      ~p_wait_semaphores: sign_sems
      ~p_swapchains: swapchains
      ~p_image_indices: present_indices
      ()

  let debug_draw () =
    let n = Swapchain.acquire_next_image_khr ~device ~swapchain:Image.swap_chain
      ~timeout:Unsigned.UInt64.max_int ~semaphore:im_semaphore ()
            <!> "Acquire image" in
    present_indices <-@ n;
    debug "Image %d acquired" n;
    Vkc.queue_submit ~queue:Cmd.queue ~p_submits:(submit_info n) ()
    <!!> "Submitting command to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <?> "Image presented"

  let rec acquire_next () =
      match  Swapchain.acquire_next_image_khr ~device ~swapchain:Image.swap_chain
               ~timeout:Unsigned.UInt64.max_int ~semaphore:im_semaphore () with
      | Ok ((`Success|`Suboptimal_khr), n) -> n
      | Ok ((`Timeout|`Not_ready), _ ) -> acquire_next ()
      | Error x ->
        (Format.eprintf "Error %a in acquire_next" Vkt.Result.pp x; exit 2)

  let draw () =
    present_indices <-@ acquire_next ();
    Vkc.queue_submit ~queue:Cmd.queue ~p_submits:(submit_info !present_indices) ()
    <!!> "Submit to queue";
    Swapchain.queue_present_khr Cmd.queue present_info
    <??> "Present to queue"

end

;; Render.(debug_draw(); debug_draw ())
;; Sdl.(event_loop Render.draw e)
;; debug "End"
