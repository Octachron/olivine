type display
type window
type visual_id
type rr_output

type 's typ = 's Ctypes.structure Ctypes.typ
let display: display typ = Ctypes.structure "display"
let window: window typ = Ctypes.structure "window"
let visual_id: visual_id typ = Ctypes.structure "visual_id"
let rr_output: rr_output typ = Ctypes.structure "rr_output"
