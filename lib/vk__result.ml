let view ~ok ~error =
  let read n =
    if n < 0 then
      Error(fst error n)
    else
      Ok(fst ok n) in
  let write = function
    | Ok x -> snd ok x
    | Error x -> snd error x in
  Ctypes.view ~read ~write Ctypes.int

let pp raw ppf = function
  | Ok r -> Format.fprintf ppf "Ok %a" raw r
  | Error e -> Format.fprintf ppf "Error %a" raw e
