// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef GOOGLE_CL_PROTOBUFS_COMPILER_LISP_FIELD_H__
#define GOOGLE_CL_PROTOBUFS_COMPILER_LISP_FIELD_H__

#include <memory>
#include <string>

#include <google/protobuf/descriptor.h>
#include <google/protobuf/io/printer.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

// Returns the short Lisp slot name for a message field.
// Uses the lisp_slot extension if given.
const std::string FieldLispName(const FieldDescriptor* field);

void GenerateField(io::Printer* printer, const FieldDescriptor* field);

void GenerateExtension(io::Printer* printer,
                       const FieldDescriptor* extension,
                       const FileDescriptor* file);

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_CL_PROTOBUFS_COMPILER_LISP_FIELD_H__
