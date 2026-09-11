// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "enum.h"

#include <optional>
#include <string>

#include <absl/strings/str_cat.h>
#include "proto2-descriptor-extensions.pb.h"
#include "literals.h"
#include "names.h"
#include <google/protobuf/io/printer.h>
#include <google/protobuf/json_enumvalue_options.pb.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

namespace {

std::optional<std::string> GetCustomJsonName(const EnumValueDescriptor* value) {
  const auto& extension = value->options().GetExtension(pb::enumvalue::json);
  if (!extension.has_string())
    return std::nullopt;

  std::string json_name(extension.string());
  return json_name;
}

}  // namespace

EnumGenerator::EnumGenerator(const EnumDescriptor* descriptor) :
    descriptor_(descriptor),
    lisp_name_(EnumLispName(descriptor)) {}

EnumGenerator::~EnumGenerator() {}

void EnumGenerator::Generate(io::Printer* printer) {
  printer->Print("\n\n(pi:define-enum $name$", "name", lisp_name_);
  printer->Annotate("name", descriptor_);
  printer->Indent();

  // Options.
  printer->Indent();
  printer->Print("\n(");
  const char* sep = "";
  if (CamelIsSpitting(descriptor_->name())) {
    printer->Print(sep); sep = "\n ";
    printer->Print(":name \"$name$\"", "name", descriptor_->name());
    printer->Annotate("name", descriptor_);
  }
  // More options here.
  // Note(czak): skipped lisp_alias as it is a MessageOptions extension.
  printer->Print(")");
  printer->Outdent();

  for (int i = 0; i < descriptor_->value_count(); i++) {
    const EnumValueDescriptor* val = descriptor_->value(i);
    std::string json_name_str;
    std::optional<std::string> custom_json = GetCustomJsonName(val);
    if (custom_json.has_value()) {
      json_name_str =
          absl::StrCat(" :json-name ", LispEscapeString(*custom_json));
    }
    printer->Print("\n(:$name$ :index $number$$json-name$)", "name",
                   ToLispEnumValue(val->name()), "number",
                   absl::StrCat(val->number()), "json-name",
                   json_name_str);
    printer->Annotate("name", descriptor_);
  }
  printer->Print(")");
  printer->Outdent();
}

void EnumGenerator::AddExports(std::vector<std::string>* exports) {
  exports->push_back(lisp_name_);
  // enum keyword to integer and JSON conversion functions.
  exports->push_back(lisp_name_ + "-keyword-to-int");
  exports->push_back(lisp_name_ + "-int-to-keyword");
  exports->push_back(lisp_name_ + "-keyword-to-json");
  exports->push_back(lisp_name_ + "-json-to-keyword");
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
