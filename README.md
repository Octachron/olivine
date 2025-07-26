
Olivine is a binding generator for Vulkan and OCaml.
It generates OCaml code from the xml specification of the Vulkan API and a modest
amount of a priori knowledge.
The bindings themselves use the OCaml Ctype library.

Olivine aims to generate thin but well-typed bindings.

Currently, the generated bindings covers all Vulkan APIs except for the WSL
extensions (i.e. the interface with the various windows systems) due to a lack
of OCaml libraries covering the corresponding window systems.

Consequently, interaction with windows system is delegated to graphical
interface libraries.
Recent version of the SDL libary (≥ 2.06) and the tsdl bindings (≥0.9.6)
comes with Vulkan suport.

# Installation

If you want to experiment with the current generated bindings, you can try:

```bash
  opam pin add olivine https://github.com/Octachron/olivine.git
```

# Running examples

The examples can be run with either

```bash
make test-triangle
```
and
```bash
make test-tesseract
```

or by calling the executable by hand. Using `make test-*` enable the LunarG standard
validation layer for a more verbose log.


# Generated binding naming conventions

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
│       └ Each type definition defines its own module
│
├———— Core for functions
│
└———— $Extension_name for extensions
      └ Each extension is defined as an functor that takes as an argument
      an instance or device module depending on the scope of the extension
```

# Type mapping

Each type definition is mapped to a module `Vk.Types.Type_name` which defines a main type `t`.
All type definitions comes with a pretty-printer definition and
a variety of helper functions.

## Enums

C Enums are mapped either to

  * Polymorphic variant for the `VkResult` type
  * Each subset of `VkStructureType` used as a record extension is mapped to an open sum type
  * Regular variants otherwise

Vulkan enum names are prefixed with the type name, olivine removes these prefixes.

## Handles

Handle are mapped to an abstract type.

## Bitsets

Bitset are mapped to a olivine built-in through the `Bitset.make` functor.
These types support standard set operations and distinguish
between singleton and non-singleton values through a phantom type parameter

## Unions

Unions are mapped to a Ctype union type.
A constructor is generated for each field of the union type.

## Records

Records are mapped to a Ctype record type.
Moreover getter functions and a labelled `make` function are generated
for convenience. Like for functions (see next section), olivine
try to reconstruct higher level types from the types of record fields:

   * Array types are reconstructed in many cases.
   * Option types are identified and the corresponding fields are
     mapped to optional arguments.
   * Substructure arguments are passed directly and not by reference.
   * The `sType`, `pNext` idiom used to extend records is mapped to a proper
   open sum types.

An array function (`t list -> t Ctypes.CArray`) is also provided to ensure that
the GC does not collect the values living on the C side too soon.

## Function pointers

Ctypes views are generated for each function pointer typedef.

# Function bindings

Function bindings are generated in three different modes: `raw`, `regular` or
`native`.

The raw mode maps directly to the C function.
The regular mode regularizes the format of structure elements.
The native mode maps low-level C types to higher level types:


* Array types are reconstructed in many cases.
* Constant size arrays of characters are transformed to strings.
* Option types are identified and the corresponding fields are
  mapped to optional arguments.
* Output parameters that are the last argument of the function and of a pointer
  kind are identified and are added to the OCaml function output.
* `VkResult` output types are transformed an OCaml `result` monad
   and combined with output parameters when relevant. Note the result type
   uses a polymorphic variant to narrow its type to the effective return type.
* The Vulkan idiom `void f(size_t* n, ty array[])` where the first integer
  argument can be used to retrieve the length of the output array is identified
  and mapped to an array output.

In the presence of optional arguments, a last unit argument is added.

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

# Internals

The code generator executable is in `generator/libgen.ml`.
It operates in several stages:

1. The Vulkan XML specification is loaded as an `Info.Xml.xml` tree.
2. `Info.Structured_spec.typecheck` converts this to an `Info.Structured_spec.spec`.
   This roughly matches the structure of the XML, but with richer types,
   and it merges entities from all enabled extensions into the main registry.
3. `Aster.Lib.generate` creates the tree of modules to be generated (`Aster.Lib.lib`).
   Each `Aster.Lib.module'` contains a list of items (functions, types, submodules, etc).
   Aliases in the spec get applied here.
   Aster uses `Info.Linguistic.name` for names, rather than `string`.
4. If `libgen` is called without an output directory, it just lists the modules to be generated
   (this is used by the dune build rules).
   Otherwise, it calls `Printer.lib` to write out the files.
5. For each module, the printer calls `item_to_ast` to output each item.
