// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

// Utilities dealing with camel-case.

#include "names.h"

#include <ctype.h>
#include <string>
#include <string_view>

#include <google/protobuf/descriptor.pb.h>
#include <google/protobuf/extension_set.h>
#include "absl/strings/ascii.h"
#include "absl/strings/str_join.h"
#include "absl/strings/str_replace.h"
#include "absl/strings/str_split.h"
#include "proto2-descriptor-extensions.pb.h"

// Copied from ABSL.
#if defined(__clang__) && defined(__has_warning)
#if __has_feature(cxx_attributes) && __has_warning("-Wimplicit-fallthrough")
#define PROTOBUF_FALLTHROUGH_INTENDED [[clang::fallthrough]]
#endif
#elif defined(__GNUC__) && __GNUC__ >= 7
#define PROTOBUF_FALLTHROUGH_INTENDED [[gnu::fallthrough]]
#elif true
#define PROTOBUF_FALLTHROUGH_INTENDED
#endif

namespace google {
namespace protobuf {
namespace cl_protobufs {

enum CharType { unknown, lower, upper, digit, separator };

CharType CharacterType(const char c) {
  if (isalpha(c) && islower(c)) {
    return lower;
  } else if (isalpha(c) && isupper(c)) {
    return upper;
  } else if (isdigit(c)) {
    return digit;
  } else if (c == '-' || c == '_') {
    return separator;
  } else {
    return unknown;
  }
}

const std::string NonDestructiveStrToLower(std::string s) {
  StrToLower(&s);
  return s;
}

const void StrToLower(std::string* s) {
  for (auto& ch : *s) {
    ch = tolower(ch);
  }
}

const std::string DeCamel(std::string_view name, const bool to_lower_case,
                          const bool to_upper_case, const char* sep) {
  // Needs to be kept in sync with class-name->proto.
  std::string result;
  CharType previous_type = unknown;
  for (int i = 0; i < name.size(); ++i) {
    char c = name[i];
    const CharType type = CharacterType(c);

    switch (type) {
      case lower:
        if (to_upper_case) c = toupper(c);
        result.append(1, c);
        break;
      case separator:
        result.append(sep);
        break;
      case upper: {
        if (to_lower_case) c = tolower(c);
        switch (previous_type) {
          case upper:
            if (i + 1 >= name.size() ||
                !isalpha(name[i + 1]) ||
                isupper(name[i + 1])) break;
            PROTOBUF_FALLTHROUGH_INTENDED;
          case lower:
          case digit:
            result.append(sep);
            break;
          default:
            break;
        }
        PROTOBUF_FALLTHROUGH_INTENDED;
      }
      default:
        result.append(1, c);
        break;
    }
    previous_type = type;
  }
  return result;
}

const std::string ToLispName(std::string_view name) {
  return DeCamel(name, true, false, "-");
}

const std::string GetSchemaName(std::string_view filename) {
  std::string schema_name(filename);
  const size_t slash = schema_name.find_last_of("\\/");
  if (std::string::npos != slash) {
    schema_name.erase(0, slash + 1);
  }
  const size_t period = schema_name.rfind('.');
  if (std::string::npos != period) {
    schema_name.erase(period);
  }
  StrToLower(&schema_name);
  return schema_name;
}

// Namespace prefix for all generated packages.
const char* const kClProtobufs = "CL-PROTOBUFS";

const std::string FileLispPackage(const FileDescriptor* file) {
  if (file->package().empty()) {
    return std::string(kClProtobufs) + "." +
        absl::AsciiStrToUpper(GetSchemaName(file->name()));
  } else {
    return std::string(kClProtobufs) + "." +
           absl::AsciiStrToUpper(ToLispName(file->package()));
  }
}

const std::string EnumLispName(const EnumDescriptor* descriptor) {
  const std::string lisp_name = ToLispName(descriptor->name());
  const Descriptor* parent = descriptor->containing_type();
  if (parent) {
    return MessageLispName(parent) + "." + lisp_name;
  } else {
    return lisp_name;
  }
}

const std::string ToLispEnumValue(std::string_view name) {
  // Enum values are usually uppercase separated by underscore.
  std::string v = absl::StrReplaceAll(name, {{"_", "-"}});
  absl::AsciiStrToLower(&v);
  return v;
}

const std::string QualifiedEnumLispName(const EnumDescriptor* descriptor,
                                        const FileDescriptor* file) {
  if (descriptor->file() != file) {
    return (NonDestructiveStrToLower(FileLispPackage(descriptor->file())) +
            "::" + EnumLispName(descriptor));
  } else {
    return EnumLispName(descriptor);
  }
}

const std::string MessageLispName(const Descriptor* descriptor) {
  if (descriptor->options().HasExtension(lisp_name)) {
    // Set it in lower case as the symbol name reads better.
    return NonDestructiveStrToLower(
        descriptor->options().GetExtension(lisp_name));
  } else {
    const std::string lisp_name = ToLispName(descriptor->name());
    const Descriptor* parent = descriptor->containing_type();
    if (parent) {
      return MessageLispName(parent) + "." + lisp_name;
    } else {
      return lisp_name;
    }
  }
}

const std::string QualifiedMessageLispName(const Descriptor* msg,
                                           const FileDescriptor* file) {
  if (msg->file() != file) {
    return (NonDestructiveStrToLower(FileLispPackage(msg->file())) +
            "::" + MessageLispName(msg));
  } else {
    return MessageLispName(msg);
  }
}

const std::string ToCamelCase(std::string_view name) {
  // Needs to be kept in sync with the Lisp function proto->class-name.
  std::string result;
  CharType previous_type = unknown;
  for (int i = 0; i < name.size(); ++i) {
    char c = name[i];
    const CharType type = CharacterType(c);

    switch (type) {
      case separator:
        break;
      case lower:
        switch (previous_type) {
          case separator:
          case digit:
          case unknown:
            c = toupper(c);
            break;
          default:
            break;
        }
        PROTOBUF_FALLTHROUGH_INTENDED;
      default:
        result.append(1, c);
        break;
    }
    previous_type = type;
  }
  return result;
}

bool CamelIsSpitting(std::string_view name) {
  return ToCamelCase(ToLispName(name)) != name;
}

const std::string ToLispAliasSymbolName(std::string_view symbol_name) {
  auto splitter = absl::StrSplit(symbol_name, ":", absl::SkipWhitespace());
  return NonDestructiveStrToLower(absl::StrJoin(splitter, "::"));
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
