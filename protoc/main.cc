// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "generator.h"
#include "google/protobuf/compiler/plugin.h"

int main(int argc, char* argv[]) {
  google::protobuf::cl_protobufs::LispGenerator generator;
  return google::protobuf::compiler::PluginMain(argc, argv, &generator);
}
