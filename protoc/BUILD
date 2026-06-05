# Description: protoc plugin for cl-protobufs

load("//security/hardening/blaze:cc_binary.bzl", "HARDENING_ENABLE_SCUDO_ALLOC", "HARDENING_ENABLE_STACK_AUTO_INIT", "HARDENING_ENABLE_WX", "cc_binary")
load("//third_party/bazel_rules/rules_cc/cc:cc_library.bzl", "cc_library")

package(default_applicable_licenses = ["//:license"])

licenses(["notice"])

cc_binary(
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
        "@com_google_protobuf//:protoc_lib",
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
        "@com_google_absl//absl/log:absl_check",
        "@com_google_absl//absl/log:absl_log",
        "@com_google_absl//absl/strings",
        "//:proto2_descriptor_extensions_cc_proto",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:protobuf_lite",
        "@com_google_protobuf//:protoc_lib",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:protobuf",
    ],
)
