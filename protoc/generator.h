// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

// Generates Common Lisp code for a given .proto file.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_GENERATOR_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_GENERATOR_H__

#include <string>
#include <vector>

#include "google/protobuf/compiler/code_generator.h"

namespace google {
class FileDescriptor;
}  // namespace google


namespace google {
namespace protobuf {
namespace cl_protobufs {

// compiler::CodeGenerator implementation that generates a Common Lisp source file.

class LispGenerator : public compiler::CodeGenerator {
 public:
  LispGenerator();
  ~LispGenerator();

  bool Generate(const FileDescriptor* file, const std::string& parameter,
                compiler::OutputDirectory* output_directory,
                std::string* error) const override;

  bool GenerateAll(const std::vector<const FileDescriptor*>& files,
                   const std::string& parameter,
                   compiler::GeneratorContext* generator_context,
                   std::string* error) const override;

  // cl-protobufs supports proto3 optional fields.
  uint64_t GetSupportedFeatures() const override {
    return FEATURE_PROTO3_OPTIONAL;
  }

 private:
  LispGenerator(const LispGenerator&) = delete;
  LispGenerator& operator=(const LispGenerator&) = delete;
};

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google


#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_GENERATOR_H__
