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
