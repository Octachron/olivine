open Ctypes

let vulkan = Vk.libvulkan
let lib =
  Dl.dlopen ~filename:"./libsdlvulkan.so" ~flags:Dl.[RTLD_NOW]

let window: Tsdl.Sdl.window typ =
  Ctypes.view ~read:Obj.magic ~write:Obj.magic (ptr void)

let result =
  let c = Vk.Result.(of_int, to_int) in
  Vk__result.view ~ok:c ~error:c

let create_surface =
  Foreign.foreign ~from:lib "create_surface"
    ( Vk.instance @-> window @-> ptr_opt Vk.allocation_callbacks
      @-> ptr Vk.surface_khr @-> returning result )

