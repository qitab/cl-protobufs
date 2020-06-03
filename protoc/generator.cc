// Copyright 2016-2020 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "generator.h"

#include <memory>
#include <unordered_set>
#include <vector>

#include <google/protobuf/io/printer.h>
#include <google/protobuf/io/zero_copy_stream.h>
#include <google/protobuf/descriptor.h>
#include "file.h"

namespace google {
namespace protobuf {
namespace cl_protobufs {

LispGenerator::LispGenerator() {}
LispGenerator::~LispGenerator() {}

bool ParseParameter(const std::string& parameter, std::string* outfile_name,
                    bool* annotate) {
  std::vector<std::pair<std::string, std::string>> options;
  compiler::ParseGeneratorParameter(parameter, &options);

  for (auto& option : options) {
    if (option.first == "output-file") {
      *outfile_name = option.second;
    } else if (option.first == "annotate-code") {
      *annotate = true;
    }
  }

  return true;
}

bool LispGenerator::Generate(const FileDescriptor* file,
                             const std::string& parameter,
                             compiler::OutputDirectory* output_directory,
                             std::string* error) const {
  std::string file_name;
  bool annotate = false;

  ParseParameter(parameter, &file_name, &annotate);

  GeneratedCodeInfo annotations;
  io::AnnotationProtoCollector<GeneratedCodeInfo> ac(&annotations);
  std::unique_ptr<io::ZeroCopyOutputStream> output(
      output_directory->Open(file_name));
  io::Printer printer(output.get(), '$', annotate ? &ac : nullptr);

  FileGenerator file_generator(file);
  file_generator.GenerateSource(&printer);

  if (annotate) {
    std::unique_ptr<io::ZeroCopyOutputStream> meta(
        output_directory->Open(file_name + ".meta"));
    GOOGLE_CHECK(annotations.SerializeToZeroCopyStream(meta.get()));
  }

  return true;
}

void AccumulateSortedFiles(
    const FileDescriptor* file,
    const std::unordered_set<const FileDescriptor*>& all_files,
    std::unordered_set<const FileDescriptor*>& visited_files,
    std::vector<const FileDescriptor*>& sorted_files) {
  // If we've added this already or we're outside of the files we're processing,
  // we're done.
  if (visited_files.count(file) > 0 || all_files.count(file) == 0) {
    return;
  }
  visited_files.insert(file);
  for (int i = 0; i < file->dependency_count(); i++) {
    AccumulateSortedFiles(file->dependency(i), all_files, visited_files,
                          sorted_files);
  }
  sorted_files.push_back(file);
}

bool LispGenerator::GenerateAll(const std::vector<const FileDescriptor*>& files,
                                const std::string& parameter,
                                compiler::GeneratorContext* context,
                                std::string* error) const {
  if (files.empty()) {
    // Something unusual.
    *error = "No input files specified.";
    return false;
  }

  std::string file_name;
  bool annotate = false;

  ParseParameter(parameter, &file_name, &annotate);

  // Single file output.
  // The name of the file is passed in the parameter.
  // Setup printer.
  GeneratedCodeInfo annotations;
  io::AnnotationProtoCollector<GeneratedCodeInfo> ac(&annotations);
  std::unique_ptr<io::ZeroCopyOutputStream> output(context->Open(file_name));
  io::Printer printer(output.get(), '$', annotate ? &ac : nullptr);

  if (files.size() == 1) {
    // Most common case.
    FileGenerator file_generator(files[0]);
    file_generator.GenerateSource(&printer);
  } else {
    // Sort the FileDescriptors based on the dependencies between them, so that
    // if one processed file imports another, the import comes first. Note that
    // the files passed to this function are in the order of
    // compiler::CodeGeneratorRequest.files_to_generate, which is the order in which
    // those files were passed to the protoc command-line.
    std::unordered_set<const FileDescriptor*> all_files(files.begin(),
                                                        files.end());
    std::unordered_set<const FileDescriptor*> visited_files;
    std::vector<const FileDescriptor*> sorted_files;
    for (const FileDescriptor* file : files) {
      AccumulateSortedFiles(file, all_files, visited_files, sorted_files);
    }
    for (const FileDescriptor* file : sorted_files) {
      FileGenerator file_generator(file);
      file_generator.GenerateSource(&printer);
    }
  }

  if (annotate) {
    std::unique_ptr<io::ZeroCopyOutputStream> meta(
      context->Open(file_name + ".meta"));
    GOOGLE_CHECK(annotations.SerializeToZeroCopyStream(meta.get()));
  }
  return true;
}

}  // namespace cl_protobufs
}  // namespace protobuf
}  // namespace google
