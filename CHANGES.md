# cl-protobufs Release Notes

## Version 3.0

This release is largely for an overhaul of the public API (hence a major version
change) and for internal code quality improvements.

*   The `protobufs` package nickname has been removed. Use the full package
    name, `cl-protobufs`, or use the `:local-nicknames` option to
    `cl:defpackage` which is widely available.

*   The reflection APIs have been renamed to more clearly distinguish them from
    non-reflection APIs by adding the word "descriptor". `find-message`,
    `find-enum` etc. are now called `find-message-descriptor` etc.

*   The descriptor types are now exported from `cl-protobufs`:
    `message-descriptor`, `service-descriptor`, `enum-descriptor`, etc.

*   `push`, `length`, and `nth` functions have been added for repeated fields.
    Both generic and generated code versions are available. See README.md for
    details.

*   `base-message` was renamed to `message`. This is the base type of any
    generated message struct.

*   The enum accessors have been renamed for clarity, to prevent confusion about
    whether the "values" of an enum were keywords or numbers:

    -   `numeral->enum` is now `enum-int-to-keyword`
    -   `enum->numeral` is now `enum-keyword-to-int`
    -   `enum-values` is now `enum-keywords`
    -   And similar renamings for the generated code versions. See README.md for
        details.

*   The word "object" has been removed from the serialization/deserialization
    functions:

    -   `serialize-object` is now `serialize`
    -   `serialize-object-to-bytes` is now `serialize-to-bytes`
    -   `serialize-object-to-stream` is now `serialize-to-stream`

    Also `serialize-object-to-file` and `deserialize-object-from-file` have been
    removed. These functions didn't give access to all file-opening options and
    are trivial to implement by calling `with-open-file` and
    `serialize-to-stream`.

*   These definitions previously exported from `cl-protobufs` have been removed:

    -   `merge-from-array`
    -   `merge-from-message`
    -   `undefined-field-type`
    -   `undefined-input-type`
    -   `undefined-output-type`
    -   `undefined-stream-type`
    -   `undefined-type`
    -   `unknown-enum-error`
    -   `error-type-name`
    -   `error-field`
    -   `error-method`

*   The descriptor defining macros, `define-enum`, `define-message`, etc. have
    been moved to the implementation package. These macros are only intended to
    be called by the generated code.

*   The condition hierarchy has been simplified. A new type, `protobuf-error` is
    exported from `cl-protobufs`, along with two subtypes: `unknown-type` and
    `unknown-field-type`. The latter are signaled during
    serialization/deserialization, parsing, and printing when a non-protobuf
    type is encountered. For example if you accidentally store a non-protobuf
    type in a repeated field and `cl-protobufs` can't figure out how to
    serialize it.

*   Type declarations have been added to field accessor functions to improve
    speed.

*   If no `package` is specified in a `.proto` file, protoc_gen_lisp now sets it
    to `cl-protobufs.filename-without-extension` instead of `cl-protobufs-user`.

*   Support for extensions has been added to the generated serialization code.

## Version 2.0

This version has many significant changes over the version that was originally
open-sourced. It reflects a change in direction from Lisp-centric to being more
like the implementations for other languages and is feature complete, including
`proto3`. A high level list of changes follows.

*   It is no longer supported to write protocol buffer message definitions in
    Lisp. Generate the Lisp code with the `protoc` plugin instead. One of the
    primary benefits of protocol buffers is that it provides
    language-independent way to interoperate with other processes, and not using
    `.proto` files as the message schema prevents that.

*   For better performance, protocol buffer messages are now represented as
    structs instead of CLOS classes and the generated field accessors do not use
    generic dispatch. (It is still possible to use the generic field accessor
    functions, if performance isn't a primary concern.)

*   `proto3` features were added: `map`, `oneof`

*   Support for JSON printing and parsing.

*   Support for most of the protocol buffers
    [well-known types](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf).

*   ABCL and CCL are now fully supported and tested via GitHub CI. (SBCL has
    long been supported.)
