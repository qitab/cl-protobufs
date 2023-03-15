// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_FILE_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_FILE_H__

#include <memory>
#include <string>
#include <vector>

#include "enum.h"
#include "message.h"
#include "service.h"
#include "google/protobuf/descriptor.h"
#include "google/protobuf/descriptor.pb.h"
#include "google/protobuf/io/printer.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

class FileGenerator {
 public:
  explicit FileGenerator(const FileDescriptor* file);
  ~FileGenerator();

  void GenerateSource(io::Printer* printer);

 private:
  const FileDescriptor* file_;
  std::string lisp_package_name_;
  std::string schema_name_;
  std::string syntax_;

  std::vector<std::unique_ptr<EnumGenerator>> enums_;
  std::vector<std::unique_ptr<MessageGenerator>> messages_;
  std::vector<std::unique_ptr<ServiceGenerator>> services_;

  FileGenerator(const FileGenerator&) = delete;
  FileGenerator& operator=(const FileGenerator&) = delete;
};

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_FILE_H__
