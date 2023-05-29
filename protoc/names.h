// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

// Utilities dealing with lisp names.

#ifndef GOOGLE_CL_PROTOBUF_COMPILER_LISP_NAMES_H__
#define GOOGLE_CL_PROTOBUF_COMPILER_LISP_NAMES_H__

#include <string>

#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/descriptor.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

const std::string NonDestructiveStrToLower(std::string s);

const void StrToLower(std::string* s);

// Removes camel-case, and puts the name in lower case.
const std::string ToLispName(const std::string& name);

// Provides a name for the Enum described by the descriptor.
const std::string EnumLispName(const EnumDescriptor* descriptor);
// Provides a name for the Lisp Enum Value that turns into a keyword.
const std::string ToLispEnumValue(const std::string& name);

// Provides a name for the Message described by the descriptor.
// Uses lisp_name extension.
const std::string MessageLispName(const Descriptor* descriptor);

// Fully qualified versions of above.
// If descriptor->file() == file, the package name is omitted.
const std::string QualifiedEnumLispName(const EnumDescriptor* descriptor,
                                        const FileDescriptor* file);

const std::string QualifiedMessageLispName(const Descriptor* descriptor,
                                           const FileDescriptor* file);

const std::string FileLispPackage(const FileDescriptor* descriptor);

const std::string GetSchemaName(std::string filename);

const std::string ToCamelCase(const std::string& name);

// True if the conversion to Lisp name and back differs from original.
bool CamelIsSpitting(const std::string& name);

const std::string ToLispAliasSymbolName(const std::string& symbol_name);

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google

#endif  // GOOGLE_CL_PROTOBUF_COMPILER_LISP_NAMES_H__
