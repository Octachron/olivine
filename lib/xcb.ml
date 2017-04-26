type connection
type window

type 's typ = 's Ctypes.structure Ctypes.typ

let xcb_connection_t: connection typ =
  Ctypes.structure "xcb_connection_t"

let xcb_window_t: window typ =
  Ctypes.structure "xcb_window_t"
