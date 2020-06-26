// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "message.h"

#include <stddef.h>

#include <algorithm>
#include <memory>
#include <set>

#include <google/protobuf/stubs/logging.h>
#include <google/protobuf/io/printer.h>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/stubs/strutil.h>
#include "proto2-descriptor-extensions.pb.h"
#include "enum.h"
#include "field.h"
#include "names.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

// Maximum value that an extension number can be.
const int kMaxExtensionNumber = 0x1fffffff;

// ===================================================================

MessageGenerator::MessageGenerator(const Descriptor* descriptor) :
    descriptor_(descriptor),
    lisp_name_(MessageLispName(descriptor)),
    nested_(descriptor->nested_type_count()),
    enums_(descriptor->enum_type_count())  {
  for (int i = 0; i < descriptor->nested_type_count(); ++i) {
    nested_[i] = std::make_unique<MessageGenerator>(descriptor->nested_type(i));
  }

  for (int i = 0; i < descriptor->enum_type_count(); ++i) {
    enums_[i] = std::make_unique<EnumGenerator>(descriptor->enum_type(i));
  }
}

MessageGenerator::~MessageGenerator() {}

namespace {
// For the strange proto:define-group.
const std::string FieldKeywordLabel(const FieldDescriptor::Label label) {
  switch (label) {
    case FieldDescriptor::LABEL_OPTIONAL:
      return ":optional";
    case FieldDescriptor::LABEL_REQUIRED:
      return ":required";
    case FieldDescriptor::LABEL_REPEATED:
      return ":repeated";
    default:
      GOOGLE_LOG(FATAL) << "Unknown field label: " << label;
      return ":error";
  }
}
}  // namespace

void MessageGenerator::GenerateSource(io::Printer* printer,
                                      const std::string& lisp_name,
                                      const int tag,
                                      const FieldDescriptor::Label label) {
  // If descriptor_ describes a map entry message, ignore it. We do not use
  // this generated message for map types.
  if (descriptor_->options().map_entry()) {
    return;
  } else {
    const bool group = tag >= 0;
    printer->Print("\n");
    printer->Print((group ? "(proto:define-group $name$" :
                            "(proto:define-message $name$"),
                   "name", lisp_name);
    printer->Annotate("name", descriptor_);
    printer->Indent();

    // Message options.
    printer->Indent();
    printer->Print("\n(:conc-name \"\"");
    if (group) {
      printer->Print("\n :index $tag$", "tag", StrCat(tag));
      printer->Print("\n :label $label$", "label", FieldKeywordLabel(label));
    }
    if (CamelIsSpitting(descriptor_->name())) {
      printer->Print("\n :name \"$name$\"", "name", descriptor_->name());
      printer->Annotate("name", descriptor_->name());
    }
    if (descriptor_->options().HasExtension(lisp_alias)) {
      // The symbol most likely doesn't exist yet.  Use double-colon to create
      // the symbol if doesn't exist yet.
      const std::string& lasn =
          ToLispAliasSymbolName(descriptor_->options().GetExtension(lisp_alias));
      printer->Print("\n :alias-for $for$", "for", lasn);
      printer->Annotate("for", lasn);
    }
    // END message options.
    printer->Print(")");
    printer->Outdent();

    if (descriptor_->enum_type_count() > 0) {
      printer->Print("\n;; Nested enums.");
      for (int i = 0; i < descriptor_->enum_type_count(); ++i) {
        enums_[i]->Generate(printer);
      }
    }

    if (descriptor_->nested_type_count() > 0) {
      printer->Print("\n;; Nested messages.");
      int printed = 0;
      for (int n = 0; n < descriptor_->nested_type_count(); ++n) {
        // Strange handling of group fields.
        // Remove all groups. Those should be generated with the fields.
        bool skip = false;
        for (int f = 0; f < descriptor_->field_count(); ++f) {
          if (descriptor_->field(f)->type() == FieldDescriptor::TYPE_GROUP &&
              descriptor_->field(f)->message_type() ==
              descriptor_->nested_type(n)) {
            skip = true;
            break;
          }
        }
        if (!skip) {
          nested_[n]->Generate(printer);
          ++printed;
        }
      }
      if (!printed) printer->Print(".. are all groups.\n");
    }

    if (descriptor_->field_count() > 0) {
      printer->Print("\n;; Fields.");
      for (int i = 0; i < descriptor_->field_count(); ++i) {
        const FieldDescriptor* field = descriptor_->field(i);
        if (field->type() == FieldDescriptor::TYPE_GROUP) {
          // Strange way of handling group fields.
          MessageGenerator group(field->message_type());
          group.GenerateSource(printer,
                               ToLispName(field->message_type()->name()),
                               field->number(),
                               field->label());
        } else {
          GenerateField(printer, field);
        }
      }
    }

    if (descriptor_->extension_count() > 0) {
      printer->Print("\n;; Extensions.");
      for (int i = 0; i < descriptor_->extension_count(); ++i) {
        GenerateExtension(
            printer, descriptor_->extension(i), descriptor_->file());
      }
    }

    if (descriptor_->extension_range_count() > 0) {
      printer->Print("\n;; Extension ranges.");
      for (int i = 0; i < descriptor_->extension_range_count(); ++i) {
        const Descriptor::ExtensionRange* range = descriptor_->extension_range(i);
        printer->Print(
            "\n(proto:define-extension $start$ $end$)", "start",
            StrCat(range->start),
            // The end is inclusive in cl_protobufs.
            // For some reason, the extension number is generated as
            // 0x7ffffffe when specified as 'max', but the max must be
            // (2^29 - 1).
            "end", StrCat(std::min(kMaxExtensionNumber, range->end - 1)));
      }
    }

    printer->Print(")");
    printer->Outdent();
  }
}

void MessageGenerator::Generate(io::Printer* printer) {
  GenerateSource(printer, lisp_name_, -1, FieldDescriptor::MAX_LABEL);
}

void MessageGenerator::AddExports(std::vector<std::string>* exports) {
  for (int n = 0; n < descriptor_->nested_type_count(); ++n) {
    // Strange handling of group fields.
    // Remove all groups. Those should be exported with the fields.
    // The field accessors of a group are not exported because
    // we're skipping the whole group.  Fix it.
    bool skip = false;
    for (int f = 0; f < descriptor_->field_count(); ++f) {
      if (descriptor_->field(f)->type() == FieldDescriptor::TYPE_GROUP &&
          descriptor_->field(f)->message_type() ==
          descriptor_->nested_type(n)) {
        skip = true;
        break;
      }
    }
    if (!skip) {
      nested_[n]->AddExports(exports);
    }
  }

  for (int i = 0; i < descriptor_->enum_type_count(); ++i) {
    enums_[i]->AddExports(exports);
  }
  exports->push_back(lisp_name_);
  for (int i = 0; i < descriptor_->field_count(); ++i) {
    const FieldDescriptor* field = descriptor_->field(i);
    // For groups, the message type name must be used for the correct lisp name.
    // E.g. when a group is named `SomeGroup`, the field name is `somegroup`,
    // but we want `some-group`.
    std::string name = field->type() == FieldDescriptor::TYPE_GROUP
                           ? ToLispName(field->message_type()->name())
                           : FieldLispName(field);
    exports->push_back(name);
  }
  for (int i = 0; i < descriptor_->extension_count(); ++i) {
    exports->push_back(FieldLispName(descriptor_->extension(i)));
  }
}

void MessageGenerator::AddPackages(std::set<std::string>* packages) {
  if (descriptor_->options().HasExtension(lisp_alias)) {
    const std::string alias = descriptor_->options().GetExtension(lisp_alias);
    const size_t colon = alias.find(':');
    if (colon != std::string::npos && colon > 0) {
      packages->insert(ToUpper(alias.substr(0, colon)));
    }
  }
  for (int i = 0; i < descriptor_->nested_type_count(); ++i) {
    nested_[i]->AddPackages(packages);
  }
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
