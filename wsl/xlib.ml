type display
type window
type visual_id
type rr_output

module S = struct
  type 's t = 's Ctypes.structure Ctypes.typ
end
let display: display S.t = Ctypes.structure "display"
let window: window S.t = Ctypes.structure "window"
let visual_id: visual_id S.t = Ctypes.structure "visual_id"
let rr_output: rr_output S.t = Ctypes.structure "rr_output"
