// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#ifndef THIRD_PARTY_LISP_CL_PROTOBUFS_PROTOC_LITERALS_H_
#define THIRD_PARTY_LISP_CL_PROTOBUFS_PROTOC_LITERALS_H_

#include <string>

namespace google {
namespace protobuf {
namespace cl_protobufs {

std::string LispSimpleFtoa(float value);
std::string LispSimpleDtoa(double value);
std::string LispEscapeString(const std::string& str);
std::string StringOctets(const std::string& str);
std::string LispBool(bool value);

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google

#endif  // THIRD_PARTY_LISP_CL_PROTOBUFS_PROTOC_LITERALS_H_
