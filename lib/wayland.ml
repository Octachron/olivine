type display
type surface

let wl_display: display Ctypes.structure Ctypes.typ =
  Ctypes.structure "wl_display"
let wl_surface: surface Ctypes.structure Ctypes.typ =
  Ctypes.structure "wl_surface"
