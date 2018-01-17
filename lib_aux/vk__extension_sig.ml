module type Device = sig val x: Vk__Types__Device.t end
module type Instance = sig val x: Vk__Types__Instance.t end

module type extension = sig
  val foreign: string -> ('a -> 'b) Ctypes.fn -> 'a -> 'b
end

module Foreign_device(X:Device): extension = struct
  let foreign name typ=
    let name = name in
    let open Ctypes in
    coerce (ptr void) (Foreign.funptr typ) @@
    Vk__Core.get_device_proc_addr X.x name
end

module Foreign_instance(X:Instance): extension = struct
  let foreign name typ=
    let name = name in
    let open Ctypes in
    coerce (ptr void) (Foreign.funptr typ) @@
    Vk__Core.get_instance_proc_addr (Some X.x) name
end
