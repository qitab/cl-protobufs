// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_ENUM_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_ENUM_H__

#include <string>
#include <vector>

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

class EnumGenerator {
 public:
  explicit EnumGenerator(const EnumDescriptor* descriptor);
  ~EnumGenerator();

  void Generate(io::Printer* printer);

  void AddExports(std::vector<std::string>* exports);

 private:
  const EnumDescriptor* descriptor_;
  const std::string lisp_name_;

  EnumGenerator(const EnumGenerator&) = delete;
  EnumGenerator& operator=(const EnumGenerator&) = delete;
};

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google



#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_ENUM_H__
