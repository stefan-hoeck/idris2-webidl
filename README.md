# [ WIP ] idris2-webidl: A Generator for Idris2 Web Bindings

Parsers and (eventually) Idris code generator for WebIDL documents

## Progress

- [x] WebIDL parser
  - [x] Parse WebIDL toplevel specs into a syntax tree
        (types can be found in the `Text.WebIDL.Types` submodules).
  - [x] Encode syntax trees back to valid WebIDL
  - [x] Property test for parsers and encoders
- [ ] Idris2 code generator
  - [ ] Foreign function implementations
    - [x] Attribute getters
    - [x] Attribute setters
    - [x] Regular function implementations
    - [x] Interface constructors
    - [x] Dictionary constructors
    - [x] Static functions
    - [ ] Function to callback converters
    - [ ] Iterables
    - [ ] Namespace members
    - [x] Setters and Getters
    - [x] Stringifiers
    - [ ] Setlikes (no instance in spec)
    - [ ] Maplikes (no instance in spec)
  - [ ] API function implementations
    - [x] Attribute getters
    - [x] Attribute setters
    - [ ] Optional attribute unsetters
    - [ ] Optional attribute getters supporting a default return value
    - [x] Regular functions (no optional arguments or variadic argument)
    - [x] Functions with optional arguments
          (including a second version with the optional arguments
          missing)
    - [ ] Functions with a variadic argument (not sure yet how
          to handle these).
    - [ ] Function to callback converters
    - [x] Static functions
    - [ ] Iterables
    - [ ] Namespace members
    - [x] Setters and Getters
    - [x] Stringifiers
    - [ ] Setlikes (no instance in spec)
    - [ ] Maplikes (no instance in spec)
  - [ ] Marshalling
    - [ ] from enum types to `String` and back
    - [ ] from `Maybe` to `Nullable` and back
    - [ ] from `Union` to `NS I` (from `Data.SOP.NS`) and back
    - [ ] from `UndefOr` to an appropriate Idris2 ADT
    - [ ] from Idris2 functions to callback types

## Codegen Notes

### Transferring WebIDL types to Idris2 types

WebIDL types can be found in module `Text.WebIDL.Types.Type`.
Here is an summary about how these types are being mapped to
Idris2 types by the code generator. Eventually, the list below
will also give additional information about functionality
required from these bindings, but in a first step

 * `Undefined`: This type actually has two Idris2 representations.
    As a function argument, it is mapped to an external
    `Undefined` Idris2 type. As an unwrapped return value, it
    is mapped to `()`.

 * Integral types: These are mapped to the following external types
   * `octet` : `UInt8`
   * `byte` : `Int8`
   * `unsigned short` : `UInt16`
   * `short` : `Int16`
   * `unsigned long` : `UInt32`
   * `long` : `Int32`
   * `unsigned long long` : `UInt64`
   * `long long` : `UInt64`

   Up to 32-bit integers, these all implement the usual numeric
   interfaces (including `Data.Bits`). The 64-bit types are
   still lacking an API, as they are represented by `Number`
   in the backend and this doesn't support integer operations
   at this precision. Will have to do some reading on how these
   are handled elsewhere (for instance in PureScript).

   Note that some of these bindings might change if Idris2 gets primitives
   like `Int32` and some of its `BitsXY` types are mapped to `Number`
   (right now, all integral primitives in Idris2 are mapped to `BigInt`).

 * Floating point types: These are mapped to `Double` for
   simplicity. Not yet sure whether it is worth to treat `float`
   and `unrestricted float` differently.

 * `bigint` : This is obviously mapped to `Integer`.

 * `boolean` : This gets its own external `Boolean` type with
    marshalling functions from and to Idris2 `Bool`.

 * String types: `DOMString` and `USVString` are ordinary Idris2
   strings, `ByteString` gets its own external type (API yet to be
   defined).

 * Buffer related types: These are mapped to external types
   of the same name. APIs are yet to be defined.

 * Nullable types: These get their own external type `Nullable`
   like in PureScript, with functionality to convert from
   and to `Maybe`.

 * `Any`: This is mapped to `AnyPtr`.

 * `Promise`: This is mapped to an external type of the same name.
   API yet to be defined (have a look at PureScript again).
   
 * `Object`: This is mapped to an external type of the same name.
   The API should at least provide an `IO` function for returning an
   empty object.

 * `Symbol`: This is mapped to an external type of the same name.
   API yet to be defined.

 * `Record`: Again, this gets its own parameterized external type.
    API yet to be defined. In Idris2, it should be possible to
    make sure that the key parameter can only be `String`
    or `ByteString`.

 * Union types : These are mapped to an external `Union` type
   parameterized by a list of types listing the choices.
   It should be straight forward to marshall from an n-ary sum
   (`Data.SOP.NS`) to a union type. Marshalling back requires some
   thinking, however.

 * Callbacks : These get their own external types. We should, however,
   provide a way to for each callback type marshall from a corresponding
   Idris2 function to the callback (as in PureScript, these should be
   `IO` actions, as they create new objects in the backend). I'm not
   sure whether it will be necessary to marshall from a callback
   back to an Idris2 function.

 * Enums: These are treated as `String` in FFI calls, but
   get their own Idris2 enum type with proper conversions
   from and to their `String` representation.

 * Interfaces : These git their own external types. Their API is generated
   by this code generator from the idl specs.
   API yet to be defined.

 * Dictionaries : These get their own external types. Their API is generated
   by this code generator from the idl specs. In addition, each dictionary
   should get one or two (if the dictionary has optional attributes)
   constructor.

 * Mixins : These just get their own external types without any
   constructor.

 * Constants : Right now, these are implemented as plain Idris2
   nullary functions. Eventually, I'd like define proper Idris2 types
   for these and define rules for the code generator, where to
   replace the primitive types by these Idris2 types. This is future work,
   however.

### Inheritance and Mixins

WebIDL specifies inheritance relations for interfaces and dictionaries.
The code generator generates implementations for a `JSType` interface,
listing a type's parent types and included mixins. The API supports
then safe upcasting from a type to one of its parent types or mixins.

Downcasting is trickier, as it requires a way to inspect the
type of a value at runtime. The web API should provide a `SafeCast` interface
for types, which can be checked at runtime. This includes
all Idris2 primitives, the external numeric types, `String`, `Object`,
`Symbol`, `Undefine`, `Boolean`, and all interfaces and dictionary types,
whose type can be inspected by traversing a value's prototype chain.
