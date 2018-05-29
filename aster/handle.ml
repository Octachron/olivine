open Item
open Utils
open Common
open Info.Linguistic

let inner = ~:"Inner"

let make ~dispatchable name =
    if dispatchable then
      include_genf inner "Vk__builtin__handle"
    else
      include_genf ~suffix:"_non_dispatchable" inner "Vk__builtin__handle"
