module type Device = sig val x: Vk__types.Device.t end

let get device name typ =
  let open Ctypes in
  coerce (ptr void) (Foreign.funptr typ) @@
  Vk__core.get_device_proc_addr device name
