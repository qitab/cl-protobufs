// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "enum.h"

#include <string>
#include <vector>

#include "absl/strings/str_cat.h"
#include "proto2-descriptor-extensions.pb.h"
#include "names.h"
#include <google/protobuf/io/printer.h>

namespace google {
namespace protobuf {
namespace cl_protobufs {

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
    printer->Print("\n(:$name$ :index $number$)", "name",
                   ToLispEnumValue(descriptor_->value(i)->name()), "number",
                   absl::StrCat(descriptor_->value(i)->number()));
    printer->Annotate("name", descriptor_);
  }
  printer->Print(")");
  printer->Outdent();
}

void EnumGenerator::AddExports(std::vector<std::string>* exports) {
  exports->push_back(lisp_name_);
  // enum keyword to integer conversion functions.
  exports->push_back(lisp_name_ + "-keyword-to-int");
  exports->push_back(lisp_name_ + "-int-to-keyword");
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
