// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "literals.h"

#include "absl/strings/str_cat.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

const std::string LispSimpleFtoa(float value) {
  std::string result = absl::StrCat(value);
  if (result == "inf") {
    return "float-features:single-float-positive-infinity";
  } else if (result == "-inf") {
    return "float-features:single-float-negative-infinity";
  } else if (result == "nan") {
    return "float-features:single-float-nan";
  }

  std::string::size_type pos = result.find('e', 0);
  if (pos != std::string::npos) {
    result.replace(pos, 1, "f");
    return result;
  }
  return result + "f0";
}

const std::string LispSimpleDtoa(double value) {
  std::string result = absl::StrCat(value);
  if (result == "inf") {
    return "float-features:double-float-positive-infinity";
  } else if (result == "-inf") {
    return "float-features:double-float-negative-infinity";
  } else if (result == "nan") {
    return "float-features:double-float-nan";
  }

  std::string::size_type pos = result.find('e', 0);
  if (pos != std::string::npos) {
    result.replace(pos, 1, "d");
    return result;
  }

  return result + "d0";
}

const std::string LispEscapeString(const std::string& str) {
  std::string lisp;
  lisp.append(1, '"');
  for (char c : str) {
    if (c == '"') lisp.append(1, '\\');
    lisp.append(1, c);
  }
  lisp.append(1, '"');
  return lisp;
}

const std::string StringOctets(const std::string& str) {
  std::string octets;
  for (char c : str) {
    if (!octets.empty()) octets += " ";
    octets += absl::StrCat(c & 0xff);
  }
  return octets;
}

const std::string LispBool(bool value) { return value ? "cl:t" : "cl:nil"; }

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
