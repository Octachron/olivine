open Vk__types
module type device = sig val x: device end
module type instance = sig val x: instance end

module Foreign_device(X:device) = struct
  let foreign name typ=
    let name = "vk"^name in
    let open Ctypes in
    coerce (ptr void) (Foreign.funptr typ) @@
    Vk__core.get_device_proc_addr X.x name
end

module Foreign_instance(X:instance) = struct
  let foreign name typ=
    let name = "vk"^name in
    let open Ctypes in
    coerce (ptr void) (Foreign.funptr typ) @@
    Vk__core.get_instance_proc_addr (Some X.x) name
end



