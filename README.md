
Olivine is a binding generator for Vulkan and OCaml.
It generates OCaml code from the xml specification of the Vulkan API and a modest
amount of a priori knowledge.
The bindings themselves use the Ocaml Ctype library.

Olivine aims to generate thin but well-typed bindings.

Currently, the generated bindings covers all vulkan api except for the WSL
extensions (i.e. the interface with the various windows systems) due to a lack
of OCaml libraries covering the corresponding window systems.

Interaction with windows system is delegated to graphical interface libraries.
A possible option is to use the patched tsdl library available at
https://github.com/Octachron/tsdl/tree/support_2.04-06 or wait for the
integration of this patch into tsdl proper.

# Generated binding naming conventions :

First, all names use a snake case convention from type names, enum names, function
names etc. Second the `vk` prefix is not used in the generated binding. Instead,
the binding provides a nested module hierarchy of modules starting with a top
module called `Vk`. If possible redundant prefix and suffix are eliminated.

# Module hierarchy

The current module hierarchy modules is

```
Vk
├———— Const for constants
│
├———— Types for type definitions
│       └ Each type definition defines its own submodule
│
│
├———— Core for functions
│
└———— $Extension_name for extensions
      └ Each extension is defined as an functor that takes as an argument
      an instance or device module depending on the scope of the extension
```

# Type mapping

Each type definition is mapped to a submodule with a type t.
Externally, this type `Type_name.t` is reexported as `type_name`
All type definitions comes with a pretty-printer definition and
a variety of helper functions.

## Enums

C Enums are mapped either to

  * Polymorphic variant for the `VkResult` type
  * Each subset of `VkStructureType` used as a record extension is mapped to an
open sum type
  * Regular variants otherwise

Vulkan Enum names are prefixed with the type name, olivine removes these prefixes.

## Handle

Handle are mapped to an abstract type

## Bitset

Bitset are mapped to a olivine built-in through the `Bitset.make` functor.
This type distinguish supports standard set operations and distinguish
between singleton and non-singleton values through a phantom type parameter

## Unions
  Unions are mapped to a Ctype union type.
  A constructor is generated for each field of the union type

## Records

Records are mapped to a Ctype record type.
Moreover getter functions and a labelled `make` functions are generated
for the convenience sake. Like for functions (see next section), olivine
try to reconstruct higher level types from the types of record fields:

   * Array types are reconstructed in many cases
   * Option types are identified and the corresponding fields are
     mapped to optional argument
   * Substructure argument are passed directly and not by reference.
   * The `sType`,`pNext` idiom used to extend records is mapped to a proper
   open sum types.

A array function (`t list -> t Ctypes.CArray) is also provided to ensure that
the GC does not collect the values living on the C side too soon.

## Function pointer

Ctypes view are generated for each function pointer typedef

# Function binding

Function binding are generated in three different modes: `raw`, `regular` or
`native`.

The raw mode maps directly to the C function.
The regular mode regularizes the format of structure elements.
The native mode maps low-level C types to higher level types:


* Array types are reconstructed in many cases
* Constant size  array of characters are transformed to strings
* Option types are identified and the corresponding fields are
  mapped to optional argument
* Output parameters that are the last argument of the function and of a pointer
  kind are identified and are added to the OCaml function output.
* `VkResult` output type are transformed to a Ocaml's result monad
   and combined with output parameters when relevant. Note the result type
   uses polymorphic variant to narrow its type to the effective return type.
* The Vulkan idiom `void f( size_t* n, ty array[])` where the first integer
  argument can be used to retrieve the length of the output array is identified
  and mapped to an array output.

In presence of optional argument, a last unit argument is added

As an illustration the `vkCreateInstance` function
```C
VkResult
  vkCreateInstance(
    const VkCreateInstanceInfo*,
    const VkAllocationCallbacks*, VkInstance*
  )
```

is mapped to

```OCaml
val create_info:
Vk.Types.Instance_create_info.t ->
?allocator:Vk.Types.Allocation_callbacks.t Ctypes_static.ptr ->
unit ->
([ `Success ] * Vk.Types.Instance.t,
 [ `Error_extension_not_present
 | `Error_incompatible_driver
 | `Error_initialization_failed
 | `Error_layer_not_present
 | `Error_out_of_device_memory
 | `Error_out_of_host_memory ])
result
```
