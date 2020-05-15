# Description: Common Lisp Protocol Buffers library Tests.

load("//lisp/devtools/bazel:rules.bzl", "lisp_test")
load("//lisp/devtools/proto:clpb.bzl", "cl_protobufs_library")

licenses(["notice"])

exports_files(["LICENSE"])

# Files intentionally not included in this build file:
# stability-tests.lisp
#     We use protoc to parse proto schemas so we don't care about this.
# pkgdcl.lisp
# cl-protobufs-tests.asd
#     We use the Google build and test frameworks instead.

proto_library(
    name = "case-preservation",
    srcs = ["case-preservation.proto"],
)

cl_protobufs_library(
    name = "case-preservation-clpb",
    deps = [":case-preservation"],
)

proto_library(
    name = "extend-test-base",
    srcs = ["extend-test-base.proto"],
)

proto_library(
    name = "extend-test-proto",
    srcs = ["extend-test.proto"],
    deps = [":extend-test-base"],
)

cl_protobufs_library(
    name = "extend-test-clpb",
    deps = [":extend-test-proto"],
)

lisp_test(
    name = "extend-test",
    size = "small",
    srcs = ["extend-test.lisp"],
    main = "cl-protobufs.test.extend-test:run",
    deps = [
        ":extend-test-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "nested_nested_proto",
    srcs = ["nested_nested.proto"],
)

cl_protobufs_library(
    name = "nested_nested_clpb",
    deps = [":nested_nested_proto"],
)

proto_library(
    name = "forward_reference",
    srcs = ["forward_reference.proto"],
    deps = ["//third_party/lisp/cl_protobufs:proto2_descriptor_extensions"],
)

cl_protobufs_library(
    name = "forward_reference_clpb",
    deps = [":forward_reference"],
)

proto_library(
    name = "package_test1",
    srcs = ["package_test1.proto"],
    deps = [":package_test2"],
)

cl_protobufs_library(
    name = "package_test1_clpb",
    deps = [":package_test1"],
)

proto_library(
    name = "package_test2",
    srcs = ["package_test2.proto"],
)

proto_library(
    name = "testproto1",
    srcs = ["testproto1.proto"],
)

cl_protobufs_library(
    name = "testproto1-clpb",
    visibility = ["//visibility:public"],
    deps = [":testproto1"],
)

proto_library(
    name = "testproto2",
    srcs = ["testproto2.proto"],
)

cl_protobufs_library(
    name = "testproto2-clpb",
    deps = [":testproto2"],
)

proto_library(
    name = "unittest_import",
    srcs = ["unittest_import.proto"],
)

cl_protobufs_library(
    name = "unittest_import_clpb",
    deps = [":unittest_import"],
)

proto_library(
    name = "unittest",
    srcs = ["unittest.proto"],
    deps = [":unittest_import"],
)

cl_protobufs_library(
    name = "unittest_clpb",
    deps = [":unittest"],
)

proto_library(
    name = "service-test-proto",
    srcs = ["service-test.proto"],
)

cl_protobufs_library(
    name = "service-test-clpb",
    deps = [":service-test-proto"],
)

lisp_test(
    name = "case-preservation-test",
    size = "small",
    srcs = ["case-preservation-test.lisp"],
    main = "cl-protobufs.test.case-preservation-test:run",
    deps = [
        ":case-preservation-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "custom-methods",
    size = "small",
    srcs = ["custom-methods.lisp"],
    main = "cl-protobufs.test.custom-proto-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "lisp-reference-tests",
    size = "small",
    srcs = ["lisp-reference-tests.lisp"],
    main = "cl-protobufs.test.reference-test:run",
    deps = [
        ":forward_reference_clpb",
        ":package_test1_clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "lisp-service-test",
    size = "small",
    srcs = ["lisp-service-test.lisp"],
    main = "cl-protobufs.test.service-test:run",
    deps = [
        ":forward_reference_clpb",
        ":package_test1_clpb",
        ":service-test-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "packed-test",
    size = "small",
    srcs = ["packed-test.lisp"],
    main = "cl-protobufs.test.packed-test:run",
    deps = [
        ":unittest_clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

filegroup(
    name = "golden_data",
    srcs = [
        "golden_message.data",
        "golden_packed_message.data",
    ],
)

lisp_test(
    name = "full-tests",
    size = "small",
    srcs = ["full-tests.lisp"],
    data = [":golden_data"],
    main = "cl-protobufs.test.full-test:run",
    deps = [
        ":testproto1-clpb",
        ":unittest_clpb",
        "//third_party/lisp/babel",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "quick-tests",
    size = "small",
    srcs = ["quick-tests.lisp"],
    data = [":golden_data"],
    main = "cl-protobufs.test.quick-test:run",
    deps = [
        ":testproto1-clpb",
        ":testproto2-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "wire-tests",
    size = "small",
    srcs = ["wire-tests.lisp"],
    main = "cl-protobufs.test.wire-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "zigzag-test",
    size = "small",
    srcs = ["zigzag-test.lisp"],
    main = "cl-protobufs.test.zigzag-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "lisp-alias-test-proto",
    srcs = ["lisp-alias-test.proto"],
    deps = ["//third_party/lisp/cl_protobufs:proto2_descriptor_extensions"],
)

cl_protobufs_library(
    name = "lisp-alias-test-proto-clpb",
    deps = [":lisp-alias-test-proto"],
)

lisp_test(
    name = "lisp-alias-test",
    size = "small",
    srcs = ["lisp-alias-test.lisp"],
    main = "cl-protobufs.test.alias-test:run",
    deps = [
        ":lisp-alias-test-proto-clpb",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "deserialize-object-to-bytes-test",
    size = "small",
    srcs = ["deserialize-object-to-bytes-test.lisp"],
    main = "cl-protobufs.test.deserialize-test:run",
    deps = [
        ":lisp-alias-test-proto-clpb",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "text-format-test-proto",
    srcs = ["text-format-test.proto"],
    deps = ["//third_party/lisp/cl_protobufs:proto2_descriptor_extensions"],
)

cl_protobufs_library(
    name = "text-format-test-proto-clpb",
    deps = [":text-format-test-proto"],
)

lisp_test(
    name = "text-format-test",
    size = "small",
    srcs = ["text-format-test.lisp"],
    main = "cl-protobufs.test.text-format-test:run",
    deps = [
        ":text-format-test-proto-clpb",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "import-test-import-1-proto",
    srcs = [
        "import-test-import-1.proto",
    ],
)

proto_library(
    name = "import-test-import-2-proto",
    srcs = [
        "import-test-import-2.proto",
    ],
)

proto_library(
    name = "import-test-proto",
    srcs = [
        "import-test.proto",
    ],
    deps = [
        ":import-test-import-1-proto",
        ":import-test-import-2-proto",
    ],
)

cl_protobufs_library(
    name = "import-test-proto-clpb",
    deps = [":import-test-proto"],
)

lisp_test(
    name = "import-test",
    size = "small",
    srcs = ["import-test.lisp"],
    main = "cl-protobufs.test.import-test:run",
    deps = [
        ":import-test-proto-clpb",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "lazy-test-proto",
    srcs = ["lazy-test.proto"],
)

cl_protobufs_library(
    name = "lazy-test-proto-clpb",
    deps = [":lazy-test-proto"],
)

lisp_test(
    name = "lazy-test",
    size = "small",
    srcs = ["lazy-test.lisp"],
    main = "cl-protobufs.test.lazy-test:run",
    deps = [
        ":lazy-test-proto-clpb",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "lazy-structure-test",
    size = "small",
    srcs = ["lazy-structure-test.lisp"],
    main = "cl-protobufs.test.lazy-structure-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "varint-test",
    size = "small",
    srcs = ["varint-tests.lisp"],
    main = "cl-protobufs.test.varint-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "symbol-import-test",
    size = "small",
    srcs = ["symbol-import-tests.lisp"],
    main = "cl-protobufs.test.symbol-import-test:run",
    deps = [
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

lisp_test(
    name = "serialization-tests",
    size = "small",
    srcs = ["serialization-tests.lisp"],
    main = "cl-protobufs.test.serialization-test:run",
    nowarn = [
        # The define-schema macro generates methods with no corresponding defgeneric.
        "implicit-generic",
    ],
    order = "multipass",
    deps = [
        ":serialization-test-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "enum-mapping-test-proto",
    srcs = ["enum-mapping-test.proto"],
)

cl_protobufs_library(
    name = "enum-mapping-test-clpb",
    deps = [":enum-mapping-test-proto"],
)

lisp_test(
    name = "enum-mapping-test",
    srcs = ["enum-mapping-test.lisp"],
    main = "cl-protobufs.test.enum-mapping-test:run",
    deps = [
        ":enum-mapping-test-clpb",
        "//third_party/lisp/clunit",
    ],
)

proto_library(
    name = "serialization-test-proto",
    srcs = ["serialization-test.proto"],
)

cl_protobufs_library(
    name = "serialization-test-clpb",
    deps = [":serialization-test-proto"],
)

lisp_test(
    name = "serialize-object-to-bytes-tests",
    size = "small",
    srcs = ["serialize-object-to-bytes.lisp"],
    main = "cl-protobufs.test.serialize-test:run",
    deps = [
        ":serialization-test-clpb",
        "//third_party/lisp/cl_protobufs",
        "//third_party/lisp/clunit",
    ],
)
