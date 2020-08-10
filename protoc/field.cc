
// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "field.h"

#include <map>

#include <cstdint>
#include <google/protobuf/stubs/logging.h>
#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/stubs/strutil.h>
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
        type = "proto:int64";
        break;
      case FieldDescriptor::TYPE_UINT64:
        type = "proto:uint64";
        break;
      case FieldDescriptor::TYPE_INT32:
        type = "proto:int32";
        break;
      case FieldDescriptor::TYPE_FIXED64:
        type = "proto:fixed64";
        break;
      case FieldDescriptor::TYPE_FIXED32:
        type = "proto:fixed32";
        break;
      case FieldDescriptor::TYPE_BOOL:
        type = "cl:boolean";
        break;
      case FieldDescriptor::TYPE_STRING:
        type = "cl:string";
        break;
      case FieldDescriptor::TYPE_MESSAGE:
        type = QualifiedMessageLispName(field->message_type(), field->file());
        break;
      case FieldDescriptor::TYPE_BYTES:
        type = "proto:byte-vector";
        break;
      case FieldDescriptor::TYPE_UINT32:
        type = "proto:uint32";
        break;
      case FieldDescriptor::TYPE_ENUM:
        type = QualifiedEnumLispName(field->enum_type(), field->file());
        break;
      case FieldDescriptor::TYPE_SFIXED32:
        type = "proto:sfixed32";
        break;
      case FieldDescriptor::TYPE_SFIXED64:
        type = "proto:sfixed64";
        break;
      case FieldDescriptor::TYPE_SINT32:
        type = "proto:sint32";
        break;
      case FieldDescriptor::TYPE_SINT64:
        type = "proto:sint64";
        break;
      default:
        GOOGLE_LOG(FATAL) << "Unsupported FileDescriptorType: "
                   << field->DebugString();
        break;
    }
  }
  return type;
}

const std::string FieldLispClass(const FieldDescriptor* field) {
  std::string proto_class;

  switch (field->type()) {
    case FieldDescriptor::TYPE_DOUBLE:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_FLOAT:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_INT64:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_UINT32:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_UINT64:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_INT32:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_FIXED64:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_FIXED32:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_BOOL:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_STRING:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_SFIXED32:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_SFIXED64:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_SINT32:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_SINT64:
      proto_class = ":scalar";
      break;
   case FieldDescriptor::TYPE_BYTES:
      proto_class = ":scalar";
      break;
    case FieldDescriptor::TYPE_MESSAGE:
      proto_class = ":message";
      break;
    case FieldDescriptor::TYPE_GROUP:
      proto_class = ":group";
      break;
    case FieldDescriptor::TYPE_ENUM:
      proto_class = ":enum";
      break;
    default:
      GOOGLE_LOG(FATAL) << "Unsupported FileDescriptorType: "
                        << field->DebugString();
      break;
  }
  return proto_class;
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

  GOOGLE_LOG(FATAL) << "Error determining field arity: " << field->DebugString();
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
      return StrCat(":", ToLispName(value->name()));
    }
    case FieldDescriptor::CPPTYPE_INT32:
      return StrCat(field->default_value_int32());
    case FieldDescriptor::CPPTYPE_UINT32:
      return StrCat(field->default_value_uint32());
    case FieldDescriptor::CPPTYPE_INT64:
      return StrCat(field->default_value_int64());
    case FieldDescriptor::CPPTYPE_UINT64:
      return StrCat(field->default_value_uint64());
    case FieldDescriptor::CPPTYPE_STRING: {
      switch (field->type()) {
        case FieldDescriptor::TYPE_BYTES:
          return StrCat(
              "#.(cl:make-array ", field->default_value_string().size(),
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
  GOOGLE_LOG(FATAL) << "Unsupported FileDescriptorType: " << field->DebugString();
  return "";
}

const std::string FieldTypeName(const FieldDescriptor* field) {
  switch (field->type()) {
      case FieldDescriptor::TYPE_ENUM:
        return  (field->enum_type()->file() == field->file() &&
                 (field->enum_type()->containing_type() ==
                  field->containing_type() ||
                  field->enum_type()->containing_type() ==
                  field->containing_type()->containing_type())) ?
            field->enum_type()->name():
            field->enum_type()->full_name();
      case FieldDescriptor::TYPE_MESSAGE:
        return  (field->message_type()->file() == field->file() &&
                 (field->message_type()->containing_type() ==
                  field->containing_type() ||
                  field->message_type()->containing_type() ==
                  field->containing_type()->containing_type())) ?
            field->message_type()->name():
            field->message_type()->full_name();
      default:
        return field->type_name();
    }
}
}  // namespace

const std::string FieldLispName(const FieldDescriptor* field) {
  if (field->options().HasExtension(lisp_slot)) {
    return field->options().GetExtension(lisp_slot);
  } else {
    return ToLispName(field->name());
  }
}

void GenerateField(io::Printer* printer, const FieldDescriptor* field) {
  std::map<std::string, std::string> vars;
  vars["name"] = FieldLispName(field);
  vars["tag"] = StrCat(field->number());
  if (field->is_map()) {
    vars["key-type"] = FieldLispType(field->message_type()->field(0));
    vars["val-type"] = FieldLispType(field->message_type()->field(1));
    vars["val-class"] = FieldLispClass(field->message_type()->field(1));
    printer->Print(vars,
                    "\n(proto:define-map $name$\n"
                    "   :key-type $key-type$\n"
                    "   :val-type $val-type$\n"
                    "   :val-class $val-class$\n"
                    "   :index $tag$)");
  } else {
    vars["type"] = FieldLispType(field);
    vars["class"] = FieldLispClass(field);
    vars["label"] = FieldLispLabel(field);
    vars["packed"] = field->options().packed() ? " :packed cl:t" : "";
    vars["lazy"] = field->options().lazy() ? " :lazy cl:t" : "";
    vars["default"] = field->has_default_value()
        ? StrCat(" :default ", FieldLispDefault(field))
        : "";
    printer->Print(vars,
                   "\n($name$"
                   " :index $tag$ "
                   " :type $type$"
                   " :class $class$"
                   " :label $label$"
                   "$default$"
                   "$packed$"
                   "$lazy$)");
  }
  printer->Annotate("name", field);
}

void GenerateExtension(io::Printer* printer,
                       const FieldDescriptor* extension,
                       const FileDescriptor* file) {
      printer->Print(
          "\n(proto:define-extend $name$ ()",
          "name", QualifiedMessageLispName(extension->containing_type(), file));
      printer->Indent();
      GenerateField(printer, extension);
      printer->Print(")");
      printer->Outdent();
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
