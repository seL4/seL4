<!--
  Copyright 2023 Proofcraft Pty Ltd
  SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Bitfield Generator Manual

- [Introduction](#introduction)
- [Bit Field Generator by Example](#bit-field-generator-by-example)
  - [Blocks](#blocks)
  - [Field High](#field-high)
  - [Visible Field Order](#visible-field-order)
  - [Tagged Unions](#tagged-unions)
  - [Advanced Tag Fields](#advanced-tag-fields)
    - [Tag Slices](#tag-slices)
    - [Masks](#masks)
  - [Raw Value Access](#raw-value-access)
  - [Architecture Parameters and Canonical Pointers](#architecture-parameters-and-canonical-pointers)
- [Syntax Reference](#syntax-reference)
  - [Lexical Structure](#lexical-structure)
  - [Syntactic Structure](#syntactic-structure)
  - [Restrictions](#restrictions)
- [Correctness](#correctness)
- [Limitations](#limitations)
- [Command Line](#command-line)
  - [Usage](#usage)
  - [Options](#options)
  - [seL4 build](#sel4-build)


## Introduction

The tool `bitfield_gen.py` in the `seL4/tools/` directory generates bitfield
accessor code in C with formal specifications and proofs in Isabelle/HOL.

It takes specifications of bitfield layouts in memory such as the following

```bf_gen
block VMFault {
    field     address           32

    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     15
    field     seL4_FaultType    4
}
```

and generates a C struct with constructor and getter/setter functions with
call-by-value and call-by-pointer semantics. It also, separately, generates a
corresponding data type definition in Isabelle/HOL with corresponding functions
in HOL for that data type, as well as proofs that the generated C code correctly
implements these functions. This includes absence of undefined behaviour in C.

The purpose of the tool is to provide bit-level access to structures in memory
with precise, deterministic and reliable layout semantics, with proofs of
correctness, and without the need for manual masking and shifting operations.
The C standard leaves C bitfields too underspecified to be fit for that purpose.

In addition to [basic blocks](#blocks) of bitfields such as above, the tool
supports [tagged unions](#tagged-unions) and various layout options for these
blocks.

## Bit Field Generator by Example

This section walks through the features of the bitfield specification languages
by example. The [full syntax](#syntax-reference) is described in the next
section for reference.

### Blocks

The basic concepts of the specification languages are blocks which contain a
list of fields. The example used in the introduction declares a block with
name `VMFault`, which contains four fields and two padding entries:

```bf_gen
block VMFault {
    field     address           32

    field     FSR               5
    padding                     7
    field     instructionFault  1
    padding                     15
    field     seL4_FaultType    4
}
```

This will generate a type `VMFault_t` in C. Assuming that the word size of the
underlying architecture is 32 bits, this `VMFault_t` type will have a size of
two machine words. The sizes of the content and padding fields in a block must
add up to a multiple of the machine word size.

The bits in the block are laid out in memory in the order they are mentioned in
the specification. That is, the left-most 32 bits (i.e. bits 63 to 32) of
`VMFault` will correspond to the `address` field, the next 5 bits to `FSR`, the
next 7 bits are inaccessible padding which is set to 0 on creation of the
bitfield, then one bit for `instructionFault`, 15 bits of padding, and finally 4
bits (bits 0 to 3 of the entire type) for `seL4_FaultType`.

The following diagram corresponds to the block specification above.

```bf_dia
   │------------------------  address  ----------------------------│
   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
   │                                                               │ ..
   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
   63                                                             32

                     instruction fault                  seL4_FaultType
   │-- FSR --│             │-|                             │-------|
   ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
.. │         │             │ │                             │       │
   └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
    31      27              19                              3     0
```

For creating new values of the type `VMFault`, the generator will provide a
function `VMFault` that takes an argument of the word size of the machine for
the value of each field -- for the example above, this is `uint32_t`

```c
VMFault_t VMFault(uint32_t address, uint32_t FSR, uint32_t instructionFault,
                  uint32_t seL4_FaultType);
```

Values that do not fit in the bit size of the target field will be cut off
accordingly, i.e., as if using `val & mask(size)`.

To read the value of each field, the generator will provide the following
functions:

```c
uint32_t VMFault_get_address(VMFault_t vm_fault);
uint32_t VMFault_get_FSR(VMFault_t vm_fault);
uint32_t VMFault_get_instructionFault(VMFault_t vm_fault);
uint32_t VMFault_get_seL4_FaultType(VMFault_t vm_fault);
```

For setting one of the fields in an existing value of type `VMFault_t`:

```c
VMFault_t VMFault_set_address(VMFault_t vm_fault, uint32_t address);
VMFault_t VMFault_set_FSR(VMFault_t vm_fault, uint32_t FSR);
VMFault_t VMFault_set_instructionFault(VMFault_t vm_fault, uint32_t instructionFault);
VMFault_t VMFault_set_seL4_FaultType(VMFault_t vm_fault, uint32_t seL4_FaultType);
```

All of the above (constructor, getter, setter) are pure functions without side
effects or dependency on global state. They do contain `assert` statements that
are in effect in debug configurations. The generator declares all of them as
`static inline CONST` to enable corresponding compiler optimisations.

Often we expect such bitfields to be accessed via pointers, e.g. as part of a
buffer or a larger struct in memory or for instance a page table. Therefore the
generator also produces getter/setter functions with pointer access:

```c
uint32_t VMFault_ptr_get_address(VMFault_t *vm_fault);
uint32_t VMFault_ptr_get_FSR(VMFault_t *vm_fault);
uint32_t VMFault_ptr_get_instructionFault(VMFault_t *vm_fault);
uint32_t VMFault_ptr_get_seL4_FaultType(VMFault_t *vm_fault);
```

These are declared `PURE`, but not `CONST`, since they depend on global state.

The pointer set functions are:

```c
void VMFault_set_address(VMFault_t *vm_fault, uint32_t address);
void VMFault_set_FSR(VMFault_t *vm_fault, uint32_t FSR);
void VMFault_set_instructionFault(VMFault_t *vm_fault, uint32_t instructionFault);
void VMFault_set_seL4_FaultType(VMFault_t *vm_fault, uint32_t seL4_FaultType);
```

These are neither `PURE` nor `CONST`, but still declared as `static inline`.

> Note that the generator has a command line option `--prune` to skip functions
that are not used, and the seL4 build system makes use of that option. That
means, not necessarily all of the combinations above will appear in the
generated files for seL4, only the ones that are actually used in the rest of
the code.

### Field High

The usual access patterns for fields is that if we provide e.g. a value of 5 to
a setter function, we expect a corresponding 8-bit field to be set to the bit
pattern `0000 0101`.

In general, on a 32-bit architecture, we are providing a 32-bit value to store
in an n-bit field. One way of looking at this is that we know the top 32-n bits
of the value are always 0.

Sometimes we instead know that the *bottom* n bits of the value we provide are
0. This can happen for instance when we want to store an address that is always
aligned to a page boundary, as it would in a page table entry. This means we are
interested in the high bits of the address. The `field_high` tag allows us to
store only the significant high bits of such addresses.

For instance the base pointer stored in a page table cap in seL4 for 32-bit
RISC-V is such a case:

```bf_gen
block page_table_cap {
    field       capPTMappedASID     9
    field_high  capPTBasePtr        20
    padding                         3

    padding                         7
    field       capPTIsMapped       1
    field_high  capPTMappedAddress  20
    field       capType             4
}
```

The generated access methods of the `field_high capPTBasePtr` have the same type
as the ones for `field`, but only the top 20 bits of the provided value will be
stored in the setter methods as opposed to the bottom 20 bits for normal fields.

The getter methods will automatically correctly return a value that has the top
20 bits set to the stored value and the bottom 12 bits set to 0. That is, getter
and setter remain inverses of each other as long as the values provided to the
setter have the bottom 12 bits set to 0.

On most 64-bit architectures, the top bits of pointers are also determined,
because not all 64 bits are available (e.g. only 48 of them on x64). The
generator assumes `field_high` values to be used for pointers and to need to store
only the significant bits of the pointer, along with potential sign extension.
See the [Architecture Parameters][] section below for details.

For instance, if the platform is declared to have base 64 and 48 significant
bits, and we have a pointer that we know is aligned to 12 bits, we need to store
only 48-12 = 36 bits to recreate the correct original pointer value. That means
if we declare

```bf_gen
block store_more {
    field       some_info  16
    field_high  ptr        36
    field       other_info 12
}
```

we can store a 12-aligned pointer with `sm = store_more_set_ptr(sm, p)` and
extract it again with `p = store_more_get_ptr(sm)` without losing information.
This means, we can store 28 bits of additional information in the space that
would be taken up by a single pointer. For this it does not matter where in the
`store_more` block the `ptr` field is declared, just that it is a `field_high`
that stores 36 bits (on a 64 bit base platform with 48 significant bits).

[Architecture Parameters]: #architecture-parameters-and-canonical-pointers

### Visible Field Order

Sometimes it is useful for the constructor to take its arguments in a particular
order that is different from the field layout order in memory. It is possible to
specify such an order, enumerating the constructor field order in parentheses
after the block name:


```bf_gen
block page_table_cap(capPTBasePtr, capPTMappedASID) {
    field       capPTMappedASID     9
    field_high  capPTBasePtr        20
    padding                         3
}
```

All fields must be enumerated, including the tag field in tagged unions
described in the [next section](#tagged-unions).

Only the constructor is affected, the memory layout as well as getters and
setters remain the same.

### Tagged Unions

Multiple blocks that share a type field can be aggregated as a type in C into a
tagged union. For example to express an algebraic data type such as the
following

```isabelle
datatype cap = NullCap | ObjectCap (ref : "20 word")
```

in C, one might specify the following:

```bf_gen
block null_cap {
    padding    28
    cap_type    4
}

block object_cap {
    field ref  20
    padding     8
    cap_type    4
}

tagged_union cap cap_type {
    tag null_cap     0
    tag object_cap   1
}
```

> Note that both of the block declarations for `null_cap` and `object_cap` contain
a field with the name `cap_type` at the same bit position with the same size,
and that all blocks have the same size. We will later relax the size of the tag
field, but the tool does require all blocks in a tagged union to be of the same
size and the tag field to always be at the same position.

The fragment `tagged_union cap cap_type` means that we want the generator to
produce a C type `cap_t` that is a tagged union, and that we want to use
`cap_type` as the tag field that distinguishes which variant of the union we are
operating on.

Listing `tag null_cap 0` means that we want the block `null_cap` to be part of
the tagged union and that the value `0` in field `cap_type` will indicate that
the rest of the fields are `null_cap` fields. Similarly `tag object_cap 1`
means that a value of `1` in the tag field makes the fields of the block
`object_cap` available.

This mapping must be unique, i.e., it is an error for a block to be mentioned in
more than one tagged union.

If a block is mentioned in a tagged union, its memory layout stays the same as
before, but the code that is generated for it changes types to represent the
tagged union instead. All getters and setters for the block will be prefixed
with the name of the union, and instead of the block type, they will take and
return the union type instead (e.g. `cap_t` instead of `object_cap_t` or
`null_cap_t`). The tag field will be omitted from the constructor, because it is
determined by the constructor name, and no setter methods for the tag field will
be generated. Instead there will be a generic getter function for the tag field
which does not need to know which variant of the type it is presented with. It
returns an enum whose names correspond to the block names and whose values
correspond to the associated tag values.

For instance for the specification above, we will get:

```c
// variant tags:
enum cap_tag {
    cap_null_cap = 0,
    cap_object_cap = 1
};
typedef enum cap_tag cap_tag_t;

// discrimator:
cap_tag_t cap_get_cap_type(cap_t cap);

// constructors:
cap_t cap_null_cap_new(void);
cap_t cap_object_cap_new(uint64_t ref);

// getters and setters for object_cap variant:
uint64_t cap_object_cap_get_ref(cap_t cap);
cap_t cap_object_cap_set_ref(cap_t cap, uint64_t ref);

uint64_t cap_object_cap_ptr_get_ref(cap_t *cap);
void cap_object_cap_ptr_set_ref(cap_t *cap, uint64_t ref);
```

The getter names are constructed as `<union>_<block>_get_<field>`. Setters and
pointer versions have names with `set`, `ptr_get`, and `ptr_set` respectively.
All functions for the tagged union `cap` will start with `cap_`.

> It is a runtime error to use a getter or setter function on the wrong block
variant. That means it is the caller's responsibility to know or check which
variant a value of `cap_t` represents before using e.g.
`cap_object_cap_get_ref()` on it. The tool generates `assert` statements in the
getters and setters. Common idioms are to check the tag in a `switch` or `if`
statement and work on the contents accordingly.

### Advanced Tag Fields

#### Tag Slices

Occasionally the tag value for a tagged union cannot be encoded in a single
field of a block, for instance when the hardware prescribes using e.g. bits 0
and 5..6 for the type of a structure and bits 1..4 for something else.

An example would be the following

```bf_gen
block null {
    padding     25
    field typ_h  2
    padding      4
    field typ_l  1
}

block content {
    padding     25
    field typ_h  2
    field some   4
    field typ_l  1
}

tagged_union example ex_type(typ_h, typ_l) {
    null    (0,0)
    content (2,0)
}
```

Instead of a single tag field, the `example` union has two tag fields. We also
say that the tag `ex_type` has two *slices* `typ_h` and `typ_l`. The same
restrictions as for normal tags apply: they must fit the tag values and all tag
slice fields must be mentioned in all blocks of the union.

The fragment `ex_type(typ_h, typ_l)` means that we want the discriminator
function to be called `example_get_ex_type` and that the tag is made up of the
two fields `(typ_h, typ_l)`. Any positive number of fields is allowed. The
generated enumeration will still be called `example_tag` as it would be for a
single tag, and it will still contain a name for each alternative bound to a
value unique for that variant. (The values consist of the concatenation of the
bit pattern of the tag slice values).

Similarly to a single tag field, the tool will not generate getters and setters
for any of the tag fields mentioned in the union specification.

The values in the `null (0,0)` and `content (2,0)` are positional, i.e., the
example assigns the bit pattern `0b10` in field `typ_h` and `0` in field `typ_l`
to mean the `content` block and `0` in both to mean the `null` block.

#### Masks

The restriction that all tag fields have to have the same size can be relaxed
slightly. One can declare multiple possible tag sizes together with a bit mask
each that determines which of these sizes applies.

Say for instance that we have more than 16 different `cap` types, and that some
of the `cap` alternatives only have 4 bits of storage left to store the cap tag,
but others have further bits available, e.g. 8. We can declare a bit mask that
determines if the tag size is 4 or 8 and use the size 4 tags in the blocks that
have fewer bits left.

The following example achieves this:

```bf_gen
block null_cap {
    padding    24
    cap_type    8
}

block object_cap {
    field ref  28
    cap_type    4
}

tagged_union cap cap_type {
    mask 4 0x0e
    mask 8 0x0e
    tag null_cap    15
    tag object_cap   1
}
```

The block `null_cap` has an 8-bit tag field and the block `object_cap` has a
4-bit tag field of the same name. Note that both still must start at the same
bit positions.

The declarations `mask 4 0x0e` and `mask 8 0x0e` are read from the smallest to
the largest mask size. `mask 4 0x0e` declares that the default tag size is 4
bits wide, but if the bits `0b1110` (0x0e) are set, i.e. if `tag_value & 0x0e ==
0x0e`, then the next larger mask will be used. This means values 0 to 13 are
using 4 bit tags, 14 and 15 are using 8 bit tags, and all 8 bit tag values must
have at least the bits 1, 2, and 3 set.

Since there is no larger mask size after 8 in the example, the mask value there
is not important. However, the generator expects the mask of each larger level
to have at least all bits set that the masks of all smaller sizes have set, so
we chose the same value `0x0e` in the example. If we had another level, e.g. 16
bits wide afterwards, we might have chosen `0b11110` (0x1e) for the mask value
at 8 bit.

The sub mask inclusion means that the generator can check mask sizes
consecutively. I.e. if `tag_value & 0xe != 0xe`, we always know it is a 4 bit
tag, no matter how many other tag sizes are declared.

The tag mask value declared for each alternative in the tagged union must fit
the mask for the size of the tag field in the block. Remember that with `mask 4
 0x0e` the `0x0e` bit pattern means that size 4 does *not* apply, and instead
the next larger size applies. So in the example, because `object_cap` has a
4-bit tag field, `object_cap` must have a tag value where the bits `0x0e` are
not all set (e.g. 1 is a legal value). Conversely, `null_cap` has an 8-bit tag,
and so at least the bits `0x0e` must all be set in the tag value (e.g. 14 or 15
works, as does 255, but not 1).

Masks and tag slices are mutually exclusive, i.e. only one of these can be used
for any particular tagged union.

### Raw Value Access

The C representation of blocks, whether in a tagged union or not, is a struct
containing an array of the architecture word type (`uint32` or `uint64`) of the
length computed from the number and width of fields in the block. Given that the
memory layout is fully specified it is possible to operate on these raw values
directly without breaking the semantics of the generated functions.

This is usually neither recommended nor necessary, and for code undergoing
formal verification strongly discouraged, because it introduces additional
verification overhead.

However, one special case is occasionally convenient: when initialising an area
of memory with 0, and reinterpreting that memory as a bitfield structure, we are
guaranteed to get a value where all fields are 0. Similarly, returning an all-0
struct from a function instead of calling the constructor with only 0 parameters
may be acceptable, although one should check the generated assembly if the
compiler was not able to optimise the constructor away -- if it does then the
constructor form is clearer and easier to use in proofs.

The tool will not access padding fields, but is not guaranteed to preserve them
when copying. Writing/reading padding fields is not guaranteed to be stable.

### Architecture Parameters and Canonical Pointers

The tool supports architectures with 32 and 64 bit word sizes. The command

```bf_gen
base 32
```

selects a 32 bit native word size. The declaration covers all subsequent blocks
and unions in the specification. It is possible to give multiple declarations,
each covering the subsequent blocks and unions, but it rarely makes sense to
pick any other value than the word size of the architecture.

Many 64 bit architectures have the concept of canonical pointers where not all
64 bits of the available space are usable for pointers, but only for example 48
bits. In this setting a pointer value would be canonical if and only if all top
bits are set when bit 47 (counting from 0) is set. This is equivalent to sign
extension, i.e. treating bit 47 as the sign bit and extending that sign bit up
to bit 63.

A potential use for this is for kernel pointers to always have bit 47 set (and
all bits above) and for user pointers to always have bit 47 unset (and all bits
above).

The bitfield generator allows specifying this canonical bit and whether the top
bits for pointers should be sign extended or not like this:

```bf_gen
base 64(48,1)
```

This means: word size is 64, pointers are 48 bits `[0..47]`, and bit 48 and
above should be set to 1 if bit 47 is set.

This only affects `field_high` fields which are assumed to be pointers.

```bf_gen
base 64(48,0)
```

means word size is 64, pointers are at most 48 bits, and no sign extension. This
is currently equivalent to just `base 64`.

It may make sense to use the `base` command multiple times for the sign
extension case. For instance, if the intention is to usually store pointers on
x64, one would declare `base 64(48,1)`, but if there is a specific block that
wants to not store a pointer, but some other value as `field_high`, one could
declare `base 64` just before and `base 64(48,1)` again just after that block.


## Syntax Reference

### Lexical Structure

The generator distinguishes identifiers, integer literals, comments, white
space, keywords, and operators. Keywords and operators are shown inline in the
grammar in the next section. The rest are defined below.

A multi-character ident must start with a letter or underscore, followed by
letters, underscores or numbers. A one-character ident must consist of one
letter. Letters are Latin letters `[a-z]` and `[A-Z]`, digits are `[0-9]`.

```bnf
IDENT ::= [A-Za-z_][a-zA-Z0-9_]+ | [A-Za-z]
```

In addition to the regular expression above, identifiers that consist only of
underscores are invalid. Valid examples are `a`, `a_20`, `_a_20_b`. Invalid
examples are `0`, `_`, `0a` or `0_a`.

Integer literals can be decimal, octal, hex, or binary, followed by an optional
`l` or `L` specifier.

```bnf
INTLIT ::= (  [1-9][0-9]*
            | 0[oO]?[0-7]+
            | 0[xX][0-9a-fA-F]+
            | 0[bB][01]+
            | 0 ) [lL]?'
```

Valid examples are `15`, `017`, `0o17`, `0x0F`, `0xF`, `0XF`, `0b1111`, `0`,
`0l`, and `15L`. Invalid examples are `10A0`, `0xFG0`, `0b20`, `090`.

Comments start with `--` or with `#` and go until the end of the line. There
are no block comments.

```bf_gen
-- this is a valid comment
## as is this
```

Whitespace are spaces and tab characters. They delimit other tokens. Consecutive
whitespace is ignored.

### Syntactic Structure

```ebnf
spec :== ( base | block | tagged_union )*

base ::= "base" ("32"|"64") canonical_spec?
canonical_spec ::= "(" INTLIT "," ("0"|"1") ")"

block ::= "block" IDENT visible_order_spec_opt? "{" fields "}"
visible_order_spec_opt ::= "(" visible_order_spec? ")"
visible_order_spec ::= IDENT ("," IDENT)*
fields ::= ( "field" IDENT INTLIT | "field_high" IDENT INTLIT | "padding" INTLIT )*

tagged_union ::= "tagged_union" IDENT IDENT tag_slices? "{" masks tags "}"

tag_slices ::= "(" IDENT ("," IDENT)* ")"
masks ::= ( "mask" INTLIT INTLIT )*
tags ::= ( "tag" IDENT tag_value )*
tag_value ::= INTLIT | "(" INTLIT ( "," INTLIT )* ")"
```

Forward references to names that are declared later in the file are allowed.

### Restrictions

The following restrictions are not represented by the grammar in the previous
section, but will be checked by the generator.

- the canonical bit must be smaller than the `base` size
- field sizes must not be larger than the `base` size
- the field sizes in a block must add up to multiples of the `base` size
- all identifiers mentioned as tags in a union must be blocks
- all blocks in a tagged union must have the same size
- all blocks in a tagged union must mention the tag field(s)
- all blocks in a tagged union must have the tag field(s) at the same bit
  position
- all blocks in a tagged union must have tag fields of one of the sizes declared
  in the union, or if no mask size is declared, the tag fields must all be of
  the same size.
- each block can be used in at most one tagged union
- visible_order_spec must enumerate all fields if used
- tag values must fit into the size of the tag fields
- tag values must not be larger than an `int` of the underlying platform,
  because they will be used in an `enum` in C.

## Correctness

Since the tool generates both the implementation and the specification, the
question arises what kind of correctness property we gain. Even if the generated
proofs are checked for correctness by Isabelle/HOL, they could still be
expressing properties that are not useful or specify behaviour that we do not
want the code to have. What we do know after Isabelle/HOL has checked the proofs
is that the code implements the generated specification and that the generated C
code is free from undefined behaviour.

To gain confidence that the generated specification is meaningful and itself has
the right properties it needs human inspection or it needs to be used in further
proofs. In the seL4 verification both are the case. In particular, the generated
specification together with the generated implementation proofs are the
interface that the C verification uses when it encounters bitfield functions. If
the generated specifications are trivial, not useful, wrong, or do not express
the behaviour of the generated C (e.g. if the generated correctness proof was
trivial), these proofs could not succeed, because the specification would then
not provide the needed semantics of the code (e.g. that loading a value after
storing it does not lose information). If the specification provides strong
enough properties to make the rest of the C verification succeed, we know that
the specification is strong enough for our needs, even though it might still be
missing additional facts or properties that the generated C code might have.
This is sufficient for strong functional correctness.

## Limitations

The tool does not currently support proofs for all combinations of pointer
access functions for variants, but it does support all those that are used in
seL4. If proof support for new combinations is needed, the tool could be
extended relatively easily.

## Command Line

### Usage

The tool can be invoked from the command line as follows

```sh
bitfield_gen.py [options] <input.bf> <outfile>
```

where `<input.bf>` is a bitfield specification file as described in this manual.
If no file is given, the tool reads from `stdin`, and if no output file is given,
the tool prints to `stdout`.

By default, the output is generated C code. See options below for how to
additionally produce Isabelle/HOL theory files for specifications and proofs.

### Options

`-h, --help`:\
    Show help message and exit.

`--environment={seL4,libseL4}`:\
    Whether to produce code for the `seL4` or `libseL4` environment. This
    influences which header files are included, what statement to use for
    `assert`, what type names `uint32` etc have, and how a function is declared
    inline. The values are determined by the `INCLUDES`, `ASSERTS`, `INLINE`,
    and `TYPES` dictionaries in the generator source.

`--hol_defs`:\
    Produce Isabelle/HOL theory files with specifications and definitions.

`--hol_proofs`:\
    Produce Isabelle/HOL theory files with proofs, assuming specification
    and definition files exist. Requires the `--umm_types` option.

`--sorry_lemmas`:\
    When producing proofs, only state lemmas, but do not prove them. This
    requires explicitly setting `quick_and_dirty` in Isabelle and
    is useful for reducing processing time during other proof development.

`--prune=<file>`:\
    Append `<file>` to the list of files with pruning information, which contain
    the names of bitfield functions that are used in the client code base. The
    option can be used multiple times. If this option is used, the generator
    will only produce the functions that are listed in one of these files. This
    is useful to reduce code size, processing time, and especially proof
    processing time drastically. It does make it harder to discover which
    functions are generated by the tool, so if that is important, consider
    leaving this option off until proofs are needed.

`--toplevel=<type>`:\
    Append Isabelle/HOL `<type>` name to the list of top-level heap types. The
    proofs will generate appropriate frame conditions for all of these top-level
    heap types. The option can be used multiple times.

`--umm_types=<file>`:\
    Path to the file `umm_types.txt` which contains dependency information
    between the top-level heap types (e.g. which structs contain which other
    structs or type fields). The file can be generated using
    `spec/cspec/mk_umm_types.py` in the [l4v] repository. (UMM = unified memory
    model).

`--cspec-dir=<dir>`:\
    Location of the `cspec` directory containing the theory file
    `KernelState_C`. This is specific to the seL4 proof setup.

`--thy-output-path=<path>`:\
    Where to put the generated theory files.

`--skip_modifies`:\
    Do not emit "modifies" proofs. These proofs state and show which global
    state is modified by each generated function. They are weaker versions
    of the full specifications and may not be needed if the full specifications
    are always used instead.

`--showclasses`:\
    Print the parsed classes and fields.

`--debug`:\
    Print debug information during code and proof generation.

`--from_file=<FROM_FILE>`:\
    Original source file before preprocessing. Use this to produce a
    `Generated from FROM_FILE` comment in the output.

[l4v]: https://github.com/seL4/l4v

### seL4 build

In the seL4 build system, `.bf` files are usually first passed through the C
preprocessor to process `#include` and `#ifdef` statements, as well as perform
macro expansion, which are currently not part of the bitfield specification
language.

The source files containing C preprocessor macros and includes are called `.bf`.
There are the following main files in seL4:

- `include/structures_32.bf` and `include/structures_64.bf` contain generic
  kernel objects and capabilities for 32 and 64 bit systems
- `include/arch/**/object/structures.bf` contain architecture dependent object
  and capability definitions.
- some `plat/**` directories contain `hardware.bf` files for hardware structures.
- `libseL4` contains some bitfield definitions that are shared with the kernel in
  `libsel4/sel4_arch_include/*/sel4/sel4_arch/types.bf`,
  `libsel4/arch_include/*/sel4/arch/shared_types.bf`, and
  `libsel4/mode_include/*32*/sel4/shared_types.bf`.

The generated C code contains a comment which `.bf` include hierarchy the
definitions are coming from.

The generated code can be found in a build directory under the `generated`
directory, where e.g. `structures_gen.h` will have been generated from
`structures.bf.pbf`, which in turn is the preprocessed source of
`structures.bf`.

Note that the seL4 build uses the `--prune` option, which means that the
generator will only produce functions that are mentioned in the rest of the seL4
sources. To generate a function that is not yet mentioned, simply use a
supported function name pattern in the source (e.g. `<block>_get_<field>`) and
the generator will supply it. To discover which functions can be generated,
either use the name template descriptions in this manual or consider temporarily
removing the `--prune` option in the `cmake` build to look at the generated
sources. This should only be done when no proofs are required, because it will
drastically increase proof processing time.

<!--
TODO:

- semantics (Isabelle?)
-->
