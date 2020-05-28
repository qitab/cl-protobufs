// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "service.h"

#include <cstdint>
#include <google/protobuf/stubs/logging.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/message.h>
#include <google/protobuf/stubs/strutil.h>
#include "literals.h"
#include "names.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

ServiceGenerator::ServiceGenerator(const ServiceDescriptor* descriptor) :
    descriptor_(descriptor) {}

ServiceGenerator::~ServiceGenerator() {}

namespace {

void PrintType(io::Printer* printer,
               const Descriptor* type,
               const FileDescriptor* file,
               bool streaming) {
  const std::string qualified_lisp_name = QualifiedMessageLispName(type, file);

  // Try to imitate the CL-PROTOBUFS scope/naming rules.
  // E.g.:  (proto:define-service quux-service
  //          (:name "QUUXService")
  //          (quux-method (
  //            (quux-request :name "QUUXRequest") =>
  //            (quux-response :name "QUUXResponse"))
  //            :name "QUUXMethod")))
  // It is unclear if the "QUUXRequest" name should be prefixed with a package.
  //
  if (type->file() == file && !CamelIsSpitting(type->name()) && !streaming) {
    printer->Print("$type$", "type", qualified_lisp_name);
  } else {
    printer->Print("($type$ :name \"$name$\"$streaming$)",
                   "type", qualified_lisp_name,
                   "name", type->full_name(),
                   "streaming", streaming ? " :stream cl:t" : "");
  }
}

std::string MethodOptionValue(const MethodOptions& method_options,
                              const FieldDescriptor* field) {
  const Reflection* reflection = method_options.GetReflection();
  switch (field->cpp_type()) {
    case FieldDescriptor::CPPTYPE_INT32:
      return StrCat(reflection->GetInt32(method_options, field));
    case FieldDescriptor::CPPTYPE_INT64:
      return StrCat(reflection->GetInt64(method_options, field));
    case FieldDescriptor::CPPTYPE_UINT32:
      return StrCat(reflection->GetUInt32(method_options, field));
    case FieldDescriptor::CPPTYPE_UINT64:
      return StrCat(reflection->GetUInt64(method_options, field));
    case FieldDescriptor::CPPTYPE_DOUBLE:
      return LispSimpleDtoa(reflection->GetDouble(method_options, field));
    case FieldDescriptor::CPPTYPE_FLOAT:
      return LispSimpleFtoa(reflection->GetFloat(method_options, field));
    case FieldDescriptor::CPPTYPE_BOOL:
      return LispBool(reflection->GetBool(method_options, field));
    case FieldDescriptor::CPPTYPE_ENUM: {
      const EnumValueDescriptor* value =
          reflection->GetEnum(method_options, field);
      return StrCat(":", ToLispName(value->name()));
    }
    case FieldDescriptor::CPPTYPE_STRING:
      return LispEscapeString(reflection->GetString(method_options, field));
    case FieldDescriptor::CPPTYPE_MESSAGE:
      GOOGLE_LOG(FATAL) << "Unsupported method option: " << field->name();
  }
  return "";
}

void GenerateMethodOptions(io::Printer* printer,
                           const MethodOptions& method_options) {
  std::vector<std::string> options;
  const Descriptor* desc = MethodOptions::descriptor();
  for (int i = 0; i < desc->field_count(); ++i) {
    const FieldDescriptor* field = desc->field(i);
    // Custom method options are not supported (yet).
    if (field->name() == "uninterpreted_option") {
      continue;
    }
    // Currently every non-custom method option is non-repeated.
    GOOGLE_CHECK(!field->is_repeated());
    if (method_options.GetReflection()->HasField(method_options, field)) {
      options.emplace_back(LispEscapeString(field->name()));
      options.emplace_back(MethodOptionValue(method_options, field));
    }
  }
  if (!options.empty()) {
    printer->Print("\n:options (");
    const char* sep = "";
    for (const std::string& o : options) {
      printer->Print(sep);
      sep = " ";
      printer->Print("$option$", "option", o);
    }
    printer->Print(")");
  }
}

}  // namespace


void ServiceGenerator::Generate(io::Printer* printer) {
  // Forward-declare the stub type.
  printer->Print("\n(proto:define-service $name$",
                 "name", ToLispName(descriptor_->name()));
  printer->Annotate("name", ToLispName(descriptor_->name()));

  printer->Indent();
  {
    printer->Indent();
    printer->Print("\n(:source-location (#P\"$file$\")",
                   "file", descriptor_->file()->name());
    if (CamelIsSpitting(descriptor_->name())) {
      printer->Print("\n :name \"$name$\"", "name", descriptor_->name());
      printer->Annotate("name", descriptor_->name());
    }
    printer->Print(")");
    printer->Outdent();
  }
  {
    const FileDescriptor* file = descriptor_->file();
    for (int i = 0; i < descriptor_->method_count(); ++i) {
      const MethodDescriptor* method = descriptor_->method(i);
      printer->Print("\n($name$ (\n", "name", ToLispName(method->name()));
      printer->Annotate("name", ToLispName(method->name()));
      {
        printer->Indent();
        PrintType(printer, method->input_type(), file,
                  method->client_streaming());
        printer->Print(" =>\n");
        PrintType(printer, method->output_type(), file,
                  method->server_streaming());
        printer->Print(")");
        if (CamelIsSpitting(method->name())) {
          printer->Print("\n:name \"$name$\"", "name", method->name());
          printer->Annotate("name", method->name());
        }
        GenerateMethodOptions(printer, method->options());
        printer->Outdent();
        printer->Print(")");
      }
    }
    printer->Print(")");
    printer->Outdent();
  }
}

void ServiceGenerator::AddExports(std::vector<std::string>* exports) {
  exports->push_back(ToLispName(descriptor_->name()));
}

void ServiceGenerator::AddRpcExports(std::vector<std::string>* exports) {
  for (int i = 0; i < descriptor_->method_count(); ++i) {
    const std::string name = ToLispName(descriptor_->method(i)->name());
    exports->push_back("call-" + name);
    exports->push_back(name + "-impl");
  }
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
