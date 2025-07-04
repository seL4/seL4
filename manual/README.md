<!--
     Copyright 2021, seL4 Project a Series of LF Projects, LLC

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# seL4 Reference Manual

## Build

To build a PDF use

    make

You will need recent version of `LaTeX` and `doxygen` as well as the seL4 python
dependencies.

The main source file is `manual.tex`, most of the text is in `parts/`,
and most of the API reference is generated with `doxygen`.

## API Generation

The documentation of the seL4 API is automatically generated from
comments in the source code. This section documents the process.

### Types of API

seL4 has two types of API, system calls and object invocations.

- Top-level system calls are generally for message passing, for example `Call`,
  `Send`, and `Recv`. In addition there are also debugging and benchmarking
  system calls which can be enabled with a build flag.
- Object invocations are regular message-passing system calls, usually `Call`,
  where the recipient is effectively the kernel itself and the message encodes
  some operation on a kernel object. Some examples are `TCB_Resume` and
  `CNode_Copy`. Some kernel objects and object invocations are specific to a
  particular processor architecture, for instance `X86_Page_Map`, and
  `ARM_VCPU_InjectIrq`.

Almost all system calls, including `Call`, `Send`, and `Recv`, are in fact
object invocations under the hood, for instance on endpoint or notification
objects -- they just provide a top-level API for message passing. The only
"true" system calls are `Yield`, debug and benchmark operations.

The API documentation in the manual is divided into the following hierarchy:

- System Calls
  - General System Calls
  - Debugging System Calls
  - Benchmarking System Calls
- Architecture-Independent Object Methods
- x86-Specific Object Methods
  - General x86 Object Methods
  - IA32 Object Methods
  - x86_64 Object Methods
- ARM-Specific Object Methods
  - General ARM Object Methods
  - aarch32 Object Methods
  - aarch64 Object Methods
- RISC-V-Specific Object Methods

The process of generating API docs is different between System Calls and Object
Invocations, though each process has some parts in common.

### Common

#### Approach

Documentation for each seL4 API is written in the form of doxygen comments in C
header files. The rest of the seL4 manual is written in LaTeX. To generate API
documentation for the manual, we generate LaTeX files from doxygen comments
which are then included in the manual.

Rather than using doxygen's LaTeX output directly, we use doxygen to generate
XML files. A custom script then parses the XML and produces the final LaTeX
output. This is because we already have an established style for API
documentation in our manual, and it was easier to generate LaTeX in this style
ourselves from some simple (ie. easy to parse) intermediate format (ie. XML)
rather than try to coerce doxygen into generating perfectly-styled LaTeX.

The script that translates doxygen-generated xml into LaTeX is in:
[manual/tools/parse_doxygen_xml.py](https://github.com/seL4/seL4/blob/master/manual/tools/parse_doxygen_xml.py).

#### Custom Notation in Doxygen Comments

Some parts of the API documentation reference other parts of the manual.
Additionally, there are some custom formatting rules we'd like to apply to the
API docs in the manual that aren't understood by doxygen. To achieve both these
goals, we introduce some additional XML tags which we explicitly add to doxygen
comments inside @xmlonly ... @endxmlonly blocks.

Here's a description of all the custom tags:

```xml
<!--
Introduces documentation for a new function.
The title of the section documenting the function will be NAME.
Other parts of the manual can refer to this function's documentation with \autoref{sec:LABEL}
-->
<manual name="NAME" label="LABEL"/>

<!-- Translated to the latex \autoref{sec:SEC} -->
<autoref sec="SEC"/>

<!-- Translated to the latex \ref{sec:SEC} -->
<shortref sec="SEC"/>

<!-- Translated to the latex \errorenumdesc, a custom command defined in manual/parts/api.tex <https://github.com/seL4/seL4/blob/master/manual/parts/api.tex> -->
<errorenumdesc/>

<!-- Translated to the latex \obj{NAME}, a custom command defined in manual/manual.tex <https://github.com/seL4/seL4/blob/master/manual/manual.tex> -->
<obj name="NAME"/>

<!-- Translated to the latex \texttt{TEXT} -->
<texttt text="TEXT"/>
```

Note that these must appear within `@xmlonly` ... `@endxmlonly` blocks in
order to function.

#### Required Documentation

Each function in the API must have the following documentation:

- a `@xmlonly <manual name="..." label=".../> @endxmlonly` tag with the `name`
  for use in the manual's text, and a `label` for creating references within the
  manual
- a `@brief` description
- a detailed description
- a `@param` description of each argument
- a `@return` description of the return value, unless the function is `void`

#### Detecting Missing Documentation

If a required part of a function's documentation is empty, the translation
script will insert the LaTeX command `\todo`, defined in
[manual/parts/api.tex](https://github.com/seL4/seL4/blob/master/manual/parts/api.tex).
It generates the text "TODO" to help readers of the manual identify which parts
of the API are undocumented.

#### Generated LaTeX Files

The LaTeX files generated by the translation script are placed in
`manual/generated`. Doxygen-generated XML files are placed in
`manual/doxygen-output/xml`.

Each API is documented in a separate file. The separation of APIs is implemented
using doxygen's `@defgroup` directive. Specifically, each API is defined in a
separate named group. In the doxygen-generated XML, each group's documentation
is in a separate file. The XML-to-LaTeX script then transforms each relevant XML
file into the corresponding LaTeX file.

#### Doxygen Configuration

The correct behaviour of the manual build system depends on a specific doxygen
configuration. A `Doxyfile` containing this configuration is checked into the
kernel repo at
[manual/Doxyfile](https://github.com/seL4/seL4/blob/master/manual/Doxyfile)

### System Calls

A prototype for each system call is declared in
[libsel4/include/sel4/syscalls.h](https://github.com/seL4/seL4/blob/master/libsel4/include/sel4/syscalls.h).
Each function is documented with a comment of the form described above.

### Object Invocations

#### Stub Generation

These are more complicated, as the C source code implementing the user-level
object invocations functions is generated from interface descriptions in XML.
The following XML files contain object invocation interface descriptions:

- [libsel4/include/interfaces/object-api.xml](https://github.com/seL4/seL4/blob/master/libsel4/include/interfaces/object-api.xml)
- [libsel4/arch_include/x86/interfaces/object-api-arch.xml](https://github.com/seL4/seL4/blob/master/libsel4/arch_include/x86/interfaces/object-api-arch.xml)
- [libsel4/arch_include/arm/interfaces/object-api-arch.xml.xml](https://github.com/seL4/seL4/blob/master/libsel4/arch_include/arm/interfaces/object-api-arch.xml)
- [libsel4/sel4_arch_include/ia32/interfaces/object-api-sel4-arch.xml](https://github.com/seL4/seL4/blob/master/libsel4/sel4_arch_include/ia32/interfaces/object-api-sel4-arch.xml)
- [libsel4/sel4_arch_include/x86_64/interfaces/object-api-sel4-arch.xml](https://github.com/seL4/seL4/blob/master/libsel4/sel4_arch_include/x86_64/interfaces/object-api-sel4-arch.xml)
- [libsel4/sel4_arch_include/aarch32/interfaces/object-api-sel4-arch.xml](https://github.com/seL4/seL4/blob/master/libsel4/sel4_arch_include/aarch32/interfaces/object-api-sel4-arch.xml)
- [libsel4/sel4_arch_include/aarch64/interfaces/object-api-sel4-arch.xml](https://github.com/seL4/seL4/blob/master/libsel4/sel4_arch_include/aarch64/interfaces/object-api-sel4-arch.xml)

There is a script in the sel4 repo for generating C header files from a given
interface description:
[libsel4/tools/syscall_stub_gen.py](https://github.com/seL4/seL4/blob/master/libsel4/tools/syscall_stub_gen.py).
Note that despite its name, the script generates object invocation stubs - not
syscall stubs.

#### Documentation

Object invocation API documentation is located inline with the interface
descriptions in the XML files listed above. The XML language with which the
interfaces are described contains tags for documenting functions. For the
purpose of validation, the XML schema defining this language is in:
[libsel4/tools/sel4_idl.xsd](https://github.com/seL4/seL4/blob/master/libsel4/tools/sel4_idl.xsd).
This is a superset of the XML tags used in doxygen comments. The doxygen comment
tags described above have the same meaning in the interface description files.
Additional tags are used for the description and documentation of object
invocation interfaces.

When C code is generated from an interface description file, the documentation
inlined in that file is converted into doxygen comments of the form described
above.
