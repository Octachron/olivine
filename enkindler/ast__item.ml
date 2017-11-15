type ('a,'b) item = { structure:'a; signature:'b }

let sg x =  x.signature
let str x =  x.structure
let item structure signature = {structure;signature}
let hidden s = { structure = [s]; signature = [] }

let ( ^:: ) it1 it2 = item (str it1 :: str it2) (sg it1 :: sg it2)
let (@*) it1 it2 = item (str it1 @ str it2) (sg it1 @ sg it2)
let nil = item [] []

let rec imap f = function
  | [] -> nil
  | a :: q -> f a ^:: imap f q

let rev s = { structure = List.rev s.structure;
              signature = List.rev s.signature
            }

let fold_map f x =
  rev
 @@ List.fold_left (fun acc x -> f x @* acc ) nil x

let map_both f x = item (f @@ str x) (f @@ sg x)

let fmap f x = item (str f @@ str x) (sg f @@ sg x)
