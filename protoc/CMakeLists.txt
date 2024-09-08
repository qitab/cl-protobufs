cmake_minimum_required(VERSION 3.10)

project(protoc-gen-cl-pb)

# Abseil requires C++17
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Process Abseil's CMake build system
find_package(absl REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

include_directories(${PROTOBUF_INCLUDE_DIR})

find_library(PROTOC_LIBRARY protoc HINTS ${PROTOBUF_LIB_DIR} REQUIRED)

add_library(proto-objects OBJECT "${CMAKE_CURRENT_LIST_DIR}/../google/protobuf/proto2-descriptor-extensions.proto")

target_link_libraries(proto-objects PUBLIC protobuf::libprotobuf protobuf::libprotoc)

set(PROTO_BINARY_DIR ${CMAKE_CURRENT_LIST_DIR})

target_include_directories(proto-objects PUBLIC "$<BUILD_INTERFACE:${PROTO_BINARY_DIR}>")

protobuf_generate(
    TARGET proto-objects
    IMPORT_DIRS "${CMAKE_CURRENT_LIST_DIR}/../google/protobuf")

add_executable(protoc-gen-cl-pb enum.cc field.cc file.cc generator.cc literals.cc message.cc names.cc service.cc  main.cc)

# Declare dependency on the absl::strings library
target_link_libraries(protoc-gen-cl-pb PUBLIC absl::strings absl::log absl::base proto-objects ${PROTOC_LIBRARY} protobuf::libprotobuf protobuf::libprotoc)

install(TARGETS protoc-gen-cl-pb DESTINATION bin)
