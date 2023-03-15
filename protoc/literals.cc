// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "literals.h"

#include <float.h>

#include "absl/log/absl_check.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "google/protobuf/io/strtod.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

std::string LispSimpleFtoa(float value) {
  constexpr int kFloatToBufferSize = 24;
  if (value == std::numeric_limits<double>::infinity()) {
    return "float-features:single-float-positive-infinity";
  } else if (value == -std::numeric_limits<double>::infinity()) {
    return "float-features:single-float-negative-infinity";
  } else if (std::isnan(value)) {
    return "float-features:single-float-nan";
  }

  char buffer[kFloatToBufferSize];
  int snprintf_result =
      absl::SNPrintF(buffer, kFloatToBufferSize, "%.*g", FLT_DIG, value);
  ABSL_DCHECK(snprintf_result > 0 && snprintf_result < kFloatToBufferSize);
  std::string result = buffer;

  std::string::size_type pos = result.find('e', 0);
  if (pos != std::string::npos) {
    result.replace(pos, 1, "f");
    return result;
  }
  return result + "f0";
}

std::string LispSimpleDtoa(double value) {
  constexpr int kDoubleToBufferSize = 32;
  if (value == std::numeric_limits<double>::infinity()) {
    return "float-features:single-float-positive-infinity";
  } else if (value == -std::numeric_limits<double>::infinity()) {
    return "float-features:single-float-negative-infinity";
  } else if (std::isnan(value)) {
    return "float-features:single-float-nan";
  }

  char buffer[kDoubleToBufferSize];
  int snprintf_result =
      absl::SNPrintF(buffer, kDoubleToBufferSize, "%.*g", DBL_DIG, value);
  ABSL_DCHECK(snprintf_result > 0 && snprintf_result < kDoubleToBufferSize);
  std::string result = buffer;

  std::string::size_type pos = result.find('e', 0);
  if (pos != std::string::npos) {
    result.replace(pos, 1, "d");
    return result;
  }

  return result + "d0";
}

std::string LispEscapeString(const std::string& str) {
  std::string lisp;
  lisp.append(1, '"');
  for (char c : str) {
    if (c == '"') lisp.append(1, '\\');
    lisp.append(1, c);
  }
  lisp.append(1, '"');
  return lisp;
}

std::string StringOctets(const std::string& str) {
  std::string octets;
  for (char c : str) {
    if (!octets.empty()) octets += " ";
    octets += absl::StrCat(c & 0xff);
  }
  return octets;
}

std::string LispBool(bool value) { return value ? "cl:t" : "cl:nil"; }

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
