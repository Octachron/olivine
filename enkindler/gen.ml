
let read filename =
  let spec = open_in filename in
  let source = Xmlm.(make_input @@ `Channel spec) in
  Typed.typecheck @@ Xml.normalize @@ snd @@ Xml.tree source

let () =
  let info = read Sys.argv.(1) in
  let output = Format.formatter_of_out_channel
    @@ open_out Sys.argv.(2) in
  Generator.make_all output info.entities
