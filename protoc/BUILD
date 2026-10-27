# Description: protoc plugin for cl-protobufs

load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")

package(default_applicable_licenses = ["//:license"])

licenses(["notice"])

cc_binary(
    name = "protoc-gen-cl-pb",
    srcs = ["main.cc"],
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
        "//third_party/absl/container:flat_hash_set",
        "@com_google_absl//absl/log:absl_check",
        "@com_google_absl//absl/log:absl_log",
        "@com_google_absl//absl/strings",
        "//:proto2_descriptor_extensions_cc_proto",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:protobuf",
        "//third_party/protobuf:json_enumvalue_options_cc_proto",
        "@com_google_protobuf//:protobuf_lite",
        "@com_google_protobuf//:protoc_lib",
        "@com_google_protobuf//:protobuf",
        "@com_google_protobuf//:protobuf",
    ],
)
