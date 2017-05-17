module StringMap = struct
  include Map.Make(String)
  let find_opt key m =  try Some(find key m) with Not_found -> None
end
module StringSet = Set.Make(String)
