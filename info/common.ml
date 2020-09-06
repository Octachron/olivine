module StringMap = struct
  include Map.Make(String)
  let find_opt key m =  try Some(find key m) with Not_found -> None
end
module StringSet = Set.Make(String)

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let merge_exn x y = match x, y with
    | Some x, _ -> x
    | _, Some y -> y
    | _ -> assert false
end

module List = struct
  let rec filter_map f = function
    | [] -> []
    | a :: q ->
      match f a with
      | None -> filter_map f q
      | Some x -> x :: filter_map f q
end
