module A = Ctypes.CArray
module Vkt = Vk.Types
module Vkc = Vk.Core

module Utils = struct
  (** Ctype utility functions *)
  let get, set = Ctypes.(getf,setf)
  let null = Ctypes.null

  let ($=) field value str= set str field value
  let ( % ) = get

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

  let (+@) = Ctypes.(+@)
  let ( <-@ ) = Ctypes.( <-@ )

  let from_array typ a =
    let n = Array.length a in
    let a' = Ctypes.allocate_n ~count:n typ in
    Array.iteri (fun i x -> (a' +@ i) <-@ x ) a;
    ~: n, a'

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
  let rec event_loop e =
    let open Sdl.Event in
    if
      Sdl.poll_event @@ Some e
      && get e typ = key_down
      && get e keyboard_keycode = Sdl.K.escape
    then
      exit 0
    else event_loop e

end

  let print_extension_property e =
    let open Vkt.Extension_properties in
    Format.printf "%s\n" (to_string @@ e % extension_name)

module Instance = struct
  (** Creating a vulkan instance *)

  let nl, layers = from_array Ctypes.string [| |]

  let n_ext, extensions =
    from_array Ctypes.string
      [|"VK_KHR_surface"; "VK_KHR_xlib_surface" |]

  let info =
    make Vkt.instance_create_info
      Vkt.Instance_create_info.[
        s_type $= Vkt.Structure_type.Instance_create_info;
        p_next $= null;
        flags $= Vkt.Instance_create_flags.empty;
        p_application_info $= None;
        enabled_layer_count $= nl;
        pp_enabled_layer_names $= layers;
        enabled_extension_count $= n_ext ;
        pp_enabled_extension_names $= extensions;
      ]

  ;; debug "Info created"

  let instance =
    let x = Ctypes.allocate_n Vkt.instance 1 in
    debug "Instance pointer allocated";
    Vkc.create_instance (Ctypes.addr info) None x
    <?> "instance";
    !x

  let extension_properties =
    get_array (msg "Extension properties") Vkt.extension_properties
    @@ Vkc.enumerate_instance_extension_properties ""

  ;; Array.iter print_extension_property extension_properties
end
let instance = Instance.instance

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
      (to_string @@ p % Vkt.Physical_device_properties.device_name)

  ;; Array.iter print_property phy_devices

  let phy = phy_devices.(0)

  let queue_family_properties =
    get_array silent Vkt.queue_family_properties
    @@ Vkc.get_physical_device_queue_family_properties phy

  let print_queue_property ppf property =
    Format.fprintf ppf "Queue flags: %a \n" Vkt.Queue_flags.pp
      (property % Vkt.Queue_family_properties.queue_flags)

  ;; Array.iter (print_queue_property Format.std_formatter)
    queue_family_properties

  let queue_create_info =
    let open Vkt.Device_queue_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Device_queue_create_info;
      p_next $= null;
      flags $= Vkt.Device_queue_create_flags.empty;
      queue_family_index $= ~:0;
      queue_count $= ~:1;
      p_queue_priorities $= Ctypes.(allocate float) 1.
    ]

  let device_extensions =
    get_array (msg "device extensions")
      Vkt.extension_properties
      (Vkc.enumerate_device_extension_properties phy "")

  ;; Format.printf "Device extensions:\n@[<v 2>"
  ;; Array.iter print_extension_property device_extensions
  ;; Format.printf "@]@."


  let x =
    let d = Ctypes.allocate_n Vkt.Device.t 1 in
    let nl, layers = from_array Ctypes.string [| |] in
    let n_ext, exts = from_array Ctypes.string [|"VK_KHR_swapchain"|] in
    let info =
      let open Vkt.Device_create_info in
      mk_ptr t [
        s_type $= Vkt.Structure_type.Device_create_info;
        p_next $= null;
        flags $= Vkt.Device_create_flags.empty;
        queue_create_info_count $= ~: 1;
        p_queue_create_infos $= queue_create_info;
        enabled_layer_count $= nl;
        pp_enabled_layer_names $= layers;
        enabled_extension_count $= n_ext ;
        pp_enabled_extension_names $= exts;
        p_enabled_features $= None
      ] in
    Vkc.create_device phy info None d
    <?> "Create logical device";
    !d
end
let device = Device.x


(** Once a device has been created, load the KHR extensions *)
module Khr = Vk.Khr.Make(Device)

module Image = struct
  let surface_khr =
    let s = Ctypes.allocate_n ~count:1 Vkt.surface_khr in
    Vk__sdl.create_surface instance Sdl.w None s
    <?> "Obtaining surface";
    !s

  let capabilities =
    let x = Ctypes.allocate_n Vkt.surface_capabilities_khr 1 in
    Khr.get_physical_device_surface_capabilities_khr Device.phy surface_khr x
    <?> "Surface capabilities";
    !x

  let supported_formats =
    get_array (msg "Supported surface format") Vkt.surface_format_khr @@
    Khr.get_physical_device_surface_formats_khr Device.phy
      surface_khr

  let pp_sformat ppf sformat = let open Vkt.Surface_format_khr in
    Format.fprintf ppf "surface format @[{@ format=@[%a@];@ color_space=@[%a@]}"
      Vkt.Format.pp (sformat%format) Vkt.Color_space_khr.pp (sformat%color_space)

  ;; Array.iter (debug "%a" pp_sformat) supported_formats

  let im_format = Vkt.Format.R32_sfloat

  let extent = Vkt.Extent_2d.(
        make t [ width $= ~:512; height $= ~:512 ]
      )
  let swap_chain_info =
    let open Vkt.Swapchain_create_info_khr in
    make t [
      s_type $= Vkt.Structure_type.Swapchain_create_info_khr;
      p_next $= null;
      flags $= Vkt.Swapchain_create_flags_khr.empty;
      surface $= surface_khr;
      min_image_count $= ~:1;
      image_format $= im_format;
      image_color_space $= Vkt.Color_space_khr.Extended_srgb_linear_ext;
      image_extent $= extent ;
      image_array_layers $= ~: 1;
      image_usage $= Vkt.Image_usage_flags.(
          of_list [
            color_attachment;
            sampled;
            depth_stencil_attachment
          ]);
      image_sharing_mode $= Vkt.Sharing_mode.Exclusive;
      queue_family_index_count $= ~: 1;
      p_queue_family_indices $= Ctypes.(allocate uint32_t) ~:0;
      pre_transform $=
      Vkt.Surface_transform_flags_khr.identity;
      composite_alpha $=
      Vkt.Composite_alpha_flags_khr.opaque;
      present_mode $= Vkt.Present_mode_khr.Fifo_relaxed;
      clipped $= ~: Vk.Consts.true';
    ]

  let swap_chain =
    let s = Ctypes.allocate_n ~count:1 Vkt.swapchain_khr in
    Khr.create_swapchain_khr device (Ctypes.addr swap_chain_info) None s
    <?> "swap_chain";
    !s


  let images =
    get_array (msg "Swapchain images") Vk.Types.image
    @@ Khr.get_swapchain_images_khr device swap_chain


  let component_mapping =
    let open Vkt.Component_mapping in
    let id = Vkt.Component_swizzle.Identity in
    make t [ r $= id; g $= id; b $= id; a $= id ]

  let a_subresource_range =
    let open Vkt.Image_subresource_range in
    make t [
      aspect_mask $= Vkt.Image_aspect_flags.(singleton color);
      base_mip_level $= ~:0;
      level_count $= ~:1;
      base_array_layer $= ~:0;
      layer_count $= ~:1;
    ]

  let image_view_info im =
    let open Vkt.Image_view_create_info in
    make t [
      s_type $= Vkt.Structure_type.Image_view_create_info;
      p_next $= null;
      flags $= Vkt.Image_view_create_flags.empty;
      image $= im;
      view_type $= Vkt.Image_view_type.N2d;
      format $= im_format;
      subresource_range $= a_subresource_range;
    ]

  let views =
    let create im =
      let v = Ctypes.allocate_n Vkt.image_view 1 in
      Vkc.create_image_view device (Ctypes.addr @@ image_view_info im) None v
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
      let c = A.make Ctypes.uint32_t (len / Ctypes.(sizeof uint32_t)) in
      let c' =
        A.from_ptr Ctypes.(coerce (ptr uint32_t) (ptr char) @@ A.start c) len in
      String.iteri (fun n x -> A.set c' n x) s;
      let open Vkt.Shader_module_create_info in
      make t [
        s_type $= Vkt.Structure_type.Shader_module_create_info;
        p_next $= null;
        flags $= Vkt.Shader_module_create_flags.empty;
        code_size $= Unsigned.Size_t.of_int len ;
        p_code $= A.start c
      ]

    let create_shader name s =
      let info = shader_module_info s in
      let x = Ctypes.allocate_n Vkt.shader_module 1 in
      Vkc.create_shader_module device (Ctypes.addr info) None x
      <?> "Shader creation :" ^ name;
      !x

    let frag_shader = create_shader "fragment" frag
    let vert_shader = create_shader "vertex" vert

    let make_stage stage_ module_ =
      let open Vkt.Pipeline_shader_stage_create_info in
      make t [
        s_type $= Vkt.Structure_type.Pipeline_shader_stage_create_info;
        p_next $= null;
        flags $= Vkt.Pipeline_shader_stage_create_flags.empty;
        stage $= stage_;
        module' $= module_;
        p_name $= "main";
        p_specialization_info $= None
      ]

    let frag_stage = make_stage Vkt.Shader_stage_flags.fragment frag_shader
    let vert_stage = make_stage Vkt.Shader_stage_flags.vertex vert_shader
  end

  let null_input = let open Vkt.Pipeline_vertex_input_state_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_vertex_input_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_vertex_input_state_create_flags.empty;
      vertex_binding_description_count $= ~: 0;
      p_vertex_binding_descriptions $=
      nullptr Vkt.vertex_input_binding_description;
      vertex_attribute_description_count $= ~: 0;
      p_vertex_attribute_descriptions $=
      nullptr Vkt.vertex_input_attribute_description
    ]

  let input_assembly = let open Vkt.Pipeline_input_assembly_state_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_input_assembly_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_input_assembly_state_create_flags.empty;
      topology $= Vkt.Primitive_topology.Triangle_strip;
      primitive_restart_enable $= ~:Vk.Consts.false'
    ]

  let to_float x = float @@ Unsigned.UInt32.to_int x
  let viewport = let open Vkt.Viewport in
    make t [
      x $= 0.;
      y $= 0.;
      width $= to_float @@ Image.extent%(Vkt.Extent_2d.width);
      height $= to_float @@ Image.extent%(Vkt.Extent_2d.height);
      min_depth $= 0.;
      max_depth $= 1.
    ]

  let scissor = let open Vkt.Rect_2d in
    make t [
      offset $= Vkt.Offset_2d.(make t [ x $= 0l; y $= 0l ] );
      extent  $= Image.extent
    ]

  let viewport_state = let open Vkt.Pipeline_viewport_state_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_viewport_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_viewport_state_create_flags.empty;
      viewport_count $= ~: 1;
      p_viewports $= Ctypes.addr viewport;
      p_scissors $= Ctypes.addr scissor
    ]

  let rasterizer = let open Vkt.Pipeline_rasterization_state_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_rasterization_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_rasterization_state_create_flags.empty;
      depth_clamp_enable $= ~: Vk.Consts.false';
      rasterizer_discard_enable $= ~: Vk.Consts.false';
      polygon_mode $= Vkt.Polygon_mode.Fill;
      cull_mode $= Vkt.Cull_mode_flags.(singleton back);
      front_face $= Vkt.Front_face.Clockwise;
      depth_bias_enable $=  ~: Vk.Consts.false';
      depth_bias_constant_factor $= 0.;
      depth_bias_clamp $= 0.;
      depth_bias_slope_factor $= 0.;
      line_width $= 1.
    ]

  let no_multisampling = let open Vkt.Pipeline_multisample_state_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_multisample_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_multisample_state_create_flags.empty;
      rasterization_samples $= Vkt.Sample_count_flags.n1;
      sample_shading_enable $= ~: Vk.Consts.false';
      min_sample_shading $= 1.;
      p_sample_mask $= nullptr Ctypes.uint32_t;
      alpha_to_coverage_enable $= ~: Vk.Consts.false';
      alpha_to_one_enable $= ~: Vk.Consts.false'
    ]

  let no_blend = let open Vkt.Pipeline_color_blend_attachment_state in
    mk_ptr t [
      blend_enable $= ~: Vk.Consts.false';
      src_color_blend_factor $= Vkt.Blend_factor.One;
      dst_color_blend_factor $= Vkt.Blend_factor.Zero;
      color_blend_op $= Vkt.Blend_op.Add;
      src_alpha_blend_factor $= Vkt.Blend_factor.One;
      dst_alpha_blend_factor $= Vkt.Blend_factor.Zero;
      alpha_blend_op $= Vkt.Blend_op.Add;
    ]

  let blend_state_info = let open Vkt.Pipeline_color_blend_state_create_info in
    let consts = snd @@ from_array Ctypes.float [| 0.; 0.; 0.; 0. |] in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_color_blend_state_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_color_blend_state_create_flags.empty;
      logic_op_enable $= ~: Vk.Consts.false';
      logic_op $= Vkt.Logic_op.Copy;
      attachment_count $= ~: 1;
      p_attachments $= no_blend;
      blend_constants $= consts
    ]

  let no_uniform = let open Vkt.Pipeline_layout_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Pipeline_layout_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_layout_create_flags.empty;
      set_layout_count $= ~:0;
      p_set_layouts $= nullptr Vkt.descriptor_set_layout;
      push_constant_range_count $= ~: 0;
      p_push_constant_ranges $= nullptr Vkt.push_constant_range
    ]

  let simple_layout =
    let x = Ctypes.allocate_n Vkt.pipeline_layout 1 in
    Vkc.create_pipeline_layout device no_uniform None x
    <?> "Creating pipeline layout";
    !x

  let color_attachment =
    let open Vkt.Attachment_description in
    mk_ptr t [
      flags $= Vkt.Attachment_description_flags.empty;
      format $= Image.im_format;
      samples $= Vkt.Sample_count_flags.n1;
      load_op $= Vkt.Attachment_load_op.Clear;
      store_op $= Vkt.Attachment_store_op.Store;
      stencil_load_op $= Vkt.Attachment_load_op.Dont_care;
      stencil_store_op $= Vkt.Attachment_store_op.Dont_care;
      initial_layout $= Vkt.Image_layout.Undefined;
      final_layout $= Vkt.Image_layout.Present_src_khr
    ]

  let attachment =
    let open Vkt.Attachment_reference in
    mk_ptr t [
      attachment $= ~: 0;
      layout $= Vkt.Image_layout.Color_attachment_optimal;
    ]

  let my_subpass =
    let null = nullptr Vkt.Attachment_reference.t in
    let open Vkt.Subpass_description in
    mk_ptr t [
      flags $= Vkt.Subpass_description_flags.empty;
      pipeline_bind_point $= Vkt.Pipeline_bind_point.Graphics;
      color_attachment_count $= ~: 1;
      p_color_attachments $= attachment;
      input_attachment_count $= ~:0;
      p_input_attachments $= null;
      preserve_attachment_count $= ~:0;
      p_preserve_attachments $= nullptr Ctypes.uint32_t;
      p_resolve_attachments $= null;
      p_depth_stencil_attachment $= None;
    ]

  let render_pass_info = let open Vkt.Render_pass_create_info in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Render_pass_create_info;
      p_next $= null;
      flags $= Vkt.Render_pass_create_flags.empty;
      attachment_count $= ~: 1;
      p_attachments $= color_attachment;
      subpass_count $= ~: 1;
      p_subpasses $= my_subpass;
      dependency_count $= ~: 0;
      p_dependencies $= nullptr Vkt.subpass_dependency
    ]

  let simple_render_pass =
    let x = Ctypes.allocate_n Vkt.render_pass 1 in
    Vkc.create_render_pass device render_pass_info None x
    <?> "Creating render pass";
    !x

  let pipeline_info = let open Vkt.Graphics_pipeline_create_info in
    let nstages, stages = from_array Vkt.pipeline_shader_stage_create_info
        Shaders.[| vert_stage; frag_stage |] in
    mk_ptr t [
      s_type $= Vkt.Structure_type.Graphics_pipeline_create_info;
      p_next $= null;
      flags $= Vkt.Pipeline_create_flags.empty;
      stage_count $= nstages;
      p_stages $= stages;
      p_vertex_input_state $= null_input;
      p_input_assembly_state $= input_assembly;
      p_tessellation_state $= None;
      p_viewport_state $= Some viewport_state;
      p_rasterization_state $= rasterizer;
      p_multisample_state $= Some no_multisampling;
      p_depth_stencil_state $= None;
      p_color_blend_state $= Some blend_state_info;
      p_dynamic_state $= None;
      layout $= simple_layout;
      render_pass $= simple_render_pass;
      subpass $= ~:0;
      base_pipeline_handle $= Vkt.Pipeline.null;
      base_pipeline_index $= 0l;
    ]

  let pipeline =
    debug "Pipeline creation";
    let x = Ctypes.allocate_n Vkt.pipeline 1 in
    Vkc.create_graphics_pipelines device Vkt.Pipeline_cache.null ~:1
      pipeline_info None x
    <?> "Graphics pipeline creation";
    !x

end


;; Sdl.(event_loop e)
;; debug "End"
