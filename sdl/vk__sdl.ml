open Ctypes

let vulkan = Vk.Core.libvulkan
let lib =
  Dl.dlopen ~filename:"./libsdlvulkan.so" ~flags:Dl.[RTLD_NOW]

let window: Tsdl.Sdl.window typ =
  Ctypes.view ~read:Obj.magic ~write:Obj.magic (ptr void)

let result =
  let c = Vk.Types.Result.(of_int, to_int) in
  Vk__result.view ~ok:c ~error:c

let create_surface =
  Foreign.foreign ~from:lib "create_surface"
    ( Vk.Types.instance @-> window
      @-> ptr_opt Vk.Types.allocation_callbacks
      @-> ptr Vk.Types.surface_khr @-> returning result )
