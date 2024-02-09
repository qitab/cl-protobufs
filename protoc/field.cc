// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "field.h"

#include <map>

#include <cstdint>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/extension_set.h>
#include "proto2-descriptor-extensions.pb.h"
#include "literals.h"
#include "names.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

namespace {
const std::string FieldLispType(const FieldDescriptor* field) {
  std::string type;
  if (field->options().HasExtension(lisp_type)) {
    type = field->options().GetExtension(lisp_type);
  } else {
    switch (field->type()) {
      case FieldDescriptor::TYPE_DOUBLE:
        type = "cl:double-float";
        break;
      case FieldDescriptor::TYPE_FLOAT:
        type = "cl:float";
        break;
      case FieldDescriptor::TYPE_INT64:
        type = "cl-protobufs:int64";
        break;
      case FieldDescriptor::TYPE_UINT64:
        type = "cl-protobufs:uint64";
        break;
      case FieldDescriptor::TYPE_INT32:
        type = "cl-protobufs:int32";
        break;
      case FieldDescriptor::TYPE_FIXED64:
        type = "cl-protobufs:fixed64";
        break;
      case FieldDescriptor::TYPE_FIXED32:
        type = "cl-protobufs:fixed32";
        break;
      case FieldDescriptor::TYPE_BOOL:
        type = "cl:boolean";
        break;
      case FieldDescriptor::TYPE_STRING:
        type = "cl:string";
        break;
      case FieldDescriptor::TYPE_GROUP:
      case FieldDescriptor::TYPE_MESSAGE:
        type = QualifiedMessageLispName(field->message_type(), field->file());
        break;
      case FieldDescriptor::TYPE_BYTES:
        type = "cl-protobufs:byte-vector";
        break;
      case FieldDescriptor::TYPE_UINT32:
        type = "cl-protobufs:uint32";
        break;
      case FieldDescriptor::TYPE_ENUM:
        type = QualifiedEnumLispName(field->enum_type(), field->file());
        break;
      case FieldDescriptor::TYPE_SFIXED32:
        type = "cl-protobufs:sfixed32";
        break;
      case FieldDescriptor::TYPE_SFIXED64:
        type = "cl-protobufs:sfixed64";
        break;
      case FieldDescriptor::TYPE_SINT32:
        type = "cl-protobufs:sint32";
        break;
      case FieldDescriptor::TYPE_SINT64:
        type = "cl-protobufs:sint64";
        break;
      default:
        ABSL_LOG(FATAL) << "Unsupported FileDescriptorType: "
                   << field->DebugString();
        break;
    }
  }
  return type;
}

const std::string FieldLispKind(const FieldDescriptor* field) {
  std::string proto_kind;

  switch (field->type()) {
    case FieldDescriptor::TYPE_DOUBLE:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_FLOAT:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_INT64:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_UINT32:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_UINT64:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_INT32:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_FIXED64:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_FIXED32:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_BOOL:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_STRING:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_SFIXED32:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_SFIXED64:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_SINT32:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_SINT64:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_BYTES:
      proto_kind = ":scalar";
      break;
    case FieldDescriptor::TYPE_MESSAGE:
      proto_kind = ":message";
      break;
    case FieldDescriptor::TYPE_GROUP:
      proto_kind = ":group";
      break;
    case FieldDescriptor::TYPE_ENUM:
      proto_kind = ":enum";
      break;
    default:
      ABSL_LOG(FATAL) << "Unsupported FileDescriptorType: "
                 << field->DebugString();
      break;
  }
  return proto_kind;
}


// Return the "arity" of the field, i.e. whether it's required, optional, or
// repeated, and if repeated the type of repeated.
const std::string FieldLispLabel(const FieldDescriptor* field) {
  switch (field->label()) {
    case FieldDescriptor::Label::LABEL_REQUIRED:
      return "(:required)";
    case FieldDescriptor::Label::LABEL_OPTIONAL:
      return "(:optional)";
    case FieldDescriptor::Label::LABEL_REPEATED:
      if (field->options().HasExtension(lisp_container)) {
        switch (field->options().GetExtension(lisp_container)) {
          case LIST:
            return "(:repeated :list)";
          case VECTOR:
            return "(:repeated :vector)";
        }
      } else {
        return "(:repeated :list)";
      }
  }

  ABSL_LOG(FATAL) << "Error determining field arity: " << field->DebugString();
  return "(:error)";
}

const std::string FieldLispDefault(const FieldDescriptor* field) {
  switch (field->cpp_type()) {
    case FieldDescriptor::CPPTYPE_DOUBLE:
      return LispSimpleDtoa(field->default_value_double());
    case FieldDescriptor::CPPTYPE_FLOAT:
      return LispSimpleFtoa(field->default_value_float());
    case FieldDescriptor::CPPTYPE_BOOL:
      return LispBool(field->default_value_bool());
    case FieldDescriptor::CPPTYPE_ENUM: {
      const EnumValueDescriptor* value = field->default_value_enum();
      return absl::StrCat(":", ToLispName(value->name()));
    }
    case FieldDescriptor::CPPTYPE_INT32:
      return absl::StrCat(field->default_value_int32());
    case FieldDescriptor::CPPTYPE_UINT32:
      return absl::StrCat(field->default_value_uint32());
    case FieldDescriptor::CPPTYPE_INT64:
      return absl::StrCat(field->default_value_int64());
    case FieldDescriptor::CPPTYPE_UINT64:
      return absl::StrCat(field->default_value_uint64());
    case FieldDescriptor::CPPTYPE_STRING: {
      switch (field->type()) {
        case FieldDescriptor::TYPE_BYTES:
          return absl::StrCat(
              "(cl:make-array ", field->default_value_string().size(),
              " :element-type '(cl:unsigned-byte 8)", " :initial-contents '(",
              StringOctets(field->default_value_string()), "))");
        case FieldDescriptor::TYPE_STRING:
          return LispEscapeString(field->default_value_string());
        default: break;
      }
      break;
    }
    default: break;
  }
  // Unsupported by cl_protobufs.
  // case FieldDescriptor::CPPTYPE_MESSAGE:
  // Report errors as early as possible.
  ABSL_LOG(FATAL) << "Unsupported FileDescriptorType: " << field->DebugString();
  return "";
}
}  // namespace

const std::string FieldLispName(const FieldDescriptor* field) {
  if (field->options().HasExtension(lisp_slot)) {
    return field->options().GetExtension(lisp_slot);
  } else if (field->type() == FieldDescriptor::TYPE_GROUP) {
    // For groups the field name and message name are derived from the same
    // string. For "group FooBar {" the field name is "foobar" so we use the
    // message name instead, to get the correct hyphenation.
    return ToLispName(field->message_type()->name());
  } else {
    return ToLispName(field->name());
  }
}

void GenerateField(io::Printer* printer, const FieldDescriptor* field) {
  std::map<std::string, std::string> vars;
  vars["name"] = FieldLispName(field);
  vars["tag"] = absl::StrCat(field->number());
  vars["json-name"] = field->json_name();
  if (field->is_map()) {
    vars["key-type"] = FieldLispType(field->message_type()->field(0));
    vars["val-type"] = FieldLispType(field->message_type()->field(1));
    vars["val-kind"] = FieldLispKind(field->message_type()->field(1));
    vars["val-default"]
        = field->message_type()->field(1)->cpp_type()
        == FieldDescriptor::CPPTYPE_ENUM ?
        absl::StrCat("\n     :val-default ",
                     FieldLispDefault(field->message_type()->field(1))) : "";
    printer->Print(vars,
                   "\n(pi:define-map $name$\n"
                   "   :key-type $key-type$\n"
                   "   :value-type $val-type$\n"
                   "   :json-name \"$json-name$\"\n"
                   "   :value-kind $val-kind$\n"
                   "   :index $tag$$val-default$)");
  } else {
    vars["type"] = FieldLispType(field);
    vars["kind"] = FieldLispKind(field);
    vars["label"] = FieldLispLabel(field);
    vars["packed"] = field->is_packed() ? " :packed cl:t" : "";
    vars["lazy"] = field->options().lazy() ? " :lazy cl:t" : "";
    vars["default"] = field->has_default_value() ||
                      (field->cpp_type() == FieldDescriptor::CPPTYPE_ENUM &&
                       field->label() != FieldDescriptor::Label::LABEL_REPEATED)
                      ? absl::StrCat(" :default ", FieldLispDefault(field))
        : "";
    printer->Print(vars,
                   "\n($name$\n"
                   " :index $tag$ :type $type$ :kind $kind$ :label $label$"
                   " :json-name \"$json-name$\"$default$$packed$$lazy$)");
  }
  printer->Annotate("name", field);
}

void GenerateExtension(io::Printer* printer,
                       const FieldDescriptor* extension,
                       const FileDescriptor* file) {
  printer->Print(
      "\n(pi:define-extend $name$ ()",
      "name", QualifiedMessageLispName(extension->containing_type(), file));
  printer->Indent();
  GenerateField(printer, extension);
  printer->Print(")");
  printer->Outdent();
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
