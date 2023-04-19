// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_MESSAGE_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_MESSAGE_H__

#include <memory>
#include <set>
#include <string>
#include <vector>

#include <cstdint>
#include "enum.h"
#include "google/protobuf/descriptor.h"
#include "google/protobuf/io/printer.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

class MessageGenerator {
 public:
  explicit MessageGenerator(const Descriptor* descriptor);
  ~MessageGenerator();

  // Generate the Lisp source code for the message and nested types.
  void Generate(io::Printer* printer);

  // Add symbols to be exported.
  void AddExports(std::vector<std::string>* exports);

  // Add packages from Lisp type aliases to the set.
  void AddPackages(std::set<std::string>* packages);

 private:
  const Descriptor* descriptor_;
  const std::string lisp_name_;

  std::vector<std::unique_ptr<MessageGenerator>> nested_;
  std::vector<std::unique_ptr<EnumGenerator>> enums_;

  void GenerateSource(io::Printer* printer, const std::string& lisp_name,
                      const int32 number, const FieldDescriptor::Label label);

  MessageGenerator(const MessageGenerator&) = delete;
  MessageGenerator& operator=(const MessageGenerator&) = delete;
};

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_MESSAGE_H__
