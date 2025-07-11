# Description: protoc plugin for cl-protobufs

load("//security/scudo/blaze:cc_hardened_binary.bzl", "HARDENING_ENABLE_SCUDO_ALLOC", "HARDENING_ENABLE_STACK_AUTO_INIT", "HARDENING_ENABLE_WX", "cc_hardened_binary")
load("//third_party/bazel_rules/rules_cc/cc:cc_library.bzl", "cc_library")

package(default_applicable_licenses = ["//third_party/lisp/cl_protobufs:license"])

licenses(["notice"])

cc_hardened_binary(
    name = "protoc-gen-cl-pb",
    srcs = ["main.cc"],
    calls_initgoogle = True,
    hardening_mode = [
        HARDENING_ENABLE_SCUDO_ALLOC,
        HARDENING_ENABLE_WX,
        HARDENING_ENABLE_STACK_AUTO_INIT,
    ],
    third_party = True,
    visibility = ["//visibility:public"],
    deps = [
        ":lib",
        "//net/proto2/compiler/public:plugin",
    ],
)

cc_library(
    name = "lib",
    srcs = [
        "enum.cc",
        "field.cc",
        "file.cc",
        "generator.cc",
        "literals.cc",
        "message.cc",
        "names.cc",
        "service.cc",
    ],
    hdrs = [
        "enum.h",
        "field.h",
        "file.h",
        "generator.h",
        "literals.h",
        "message.h",
        "names.h",
        "service.h",
    ],
    deps = [
        "//base",
        "//net/proto2/compiler/public:code_generator",
        "//net/proto2/proto:descriptor_cc_proto",
        "//net/proto2/public:proto2",
        "//strings",
        "//third_party/absl/strings",
        "//third_party/lisp/cl_protobufs:proto2_descriptor_extensions_cc_proto",
        "//third_party/protobuf/io",
        "//third_party/protobuf/io:printer",
    ],
)
