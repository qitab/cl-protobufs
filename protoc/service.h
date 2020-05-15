// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_SERVICE_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_SERVICE_H__

#include <string>
#include <vector>

#include <google/protobuf/io/printer.h>
#include <google/protobuf/descriptor.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

class ServiceGenerator {
 public:
  explicit ServiceGenerator(const ServiceDescriptor* descriptor);
  ~ServiceGenerator();

  void Generate(io::Printer* printer);

  void AddExports(std::vector<std::string>* exports);
  void AddRpcExports(std::vector<std::string>* exports);

 private:
  const ServiceDescriptor* descriptor_;

  ServiceGenerator(const ServiceGenerator&) = delete;
  ServiceGenerator& operator=(const ServiceGenerator&) = delete;
};

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google



#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_SERVICE_H__
