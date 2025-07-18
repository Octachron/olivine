let unwrap = function
  | Some x -> x
  | None -> raise (Invalid_argument "unwrap None")


let nullptr typ = Ctypes.(coerce (ptr void) (ptr typ) null)

let may f = function
  | None -> None
  | Some x -> Some(f x)

let maybe f = match f with
  | Some f -> may f
  | None -> fun _ -> None

let array_opt n t =
  let read = may (fun x -> Ctypes.CArray.from_ptr x n) in
  let write = may Ctypes.CArray.start in
  Ctypes.view read write (Ctypes.ptr_opt t)

let convert_string n s =
  let a = Ctypes.CArray.make Ctypes.char n ~initial:' ' in
  for i = 0 to min n (String.length s) -1 do
    Ctypes.CArray.set a i s.[i]
  done;
  a

module Pp = struct

  let opt pp ppf = function
    | None -> Format.fprintf ppf "None"
    | Some x -> Format.fprintf ppf "Some(%a)" pp x

  let array pp ppf a =
    let get n = Ctypes.CArray.get a n in
    let n = Ctypes.CArray.length a in
    Format.fprintf ppf "@[<hov 2>⟦";
    if n > 0 then begin
      pp ppf (get 0);
      for i = 1 to (n-1) do
        Format.fprintf ppf "@ ;%a" pp (get i)
      done;
      Format.fprintf ppf "⟧@]"
    end

  let ptr pp ppf x =
    Format.fprintf ppf "*%a" pp (Ctypes.(!@) x)

  let string ppf =
    Format.fprintf ppf "%s"

  let addr ppf v =
    Format.fprintf ppf "%s" @@ Nativeint.to_string
    @@ Ctypes.raw_address_of_ptr v

  let abstract ppf _ = Format.fprintf ppf "⟨abstr⟩"


end
