# Copyright 2016-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.


# This Makefile depends on the Google protobuf library.
# See ../README.md for information on installing that


# Directory into which the Lisp protocol buffer plugin,
# protoc-gen-lisp, will be installed.
INSTALL_ROOT ?= /usr/local/bin

# Directory where Google's protocol buffer compiler is installed. This
# should be the same directory you gave as the --prefix option to
# ./configure when installing protobuf.
PROTOC_ROOT ?= /usr/local
PROTOC      ?= $(PROTOC_ROOT)/bin/protoc

OFILES = enum.o field.o file.o generator.o literals.o message.o \
	names.o service.o proto2-descriptor-extensions.o main.o

CXXFLAGS = -std=c++14 -I$(PROTOC_ROOT)/include -I.
LIBS = -Wl,-L$(PROTOC_ROOT)/lib -Wl,-rpath -Wl,$(PROTOC_ROOT)/lib -lprotoc -lprotobuf -pthread

protoc-gen-lisp: $(OFILES) Makefile
	$(CXX) $(OFILES) -o protoc-gen-lisp $(LIBS)

install: protoc-gen-lisp
	mkdir -p $(INSTALL_ROOT)
	cp protoc-gen-lisp $(INSTALL_ROOT)/

clean: .
	rm -f $(OFILES) protoc_middleman protoc-gen-lisp

# Generate the proto2 descriptor cpp files.
protoc_middleman: ../google/protobuf/proto2-descriptor-extensions.proto
	$(PROTOC) -I=../google/protobuf/ --cpp_out=. ../google/protobuf/proto2-descriptor-extensions.proto
	@touch protoc_middleman

proto2-descriptor-extensions.o: protoc_middleman
	$(CXX) $(CXXFLAGS) $(LIBS) -c -o proto2-descriptor-extensions.o proto2-descriptor-extensions.pb.cc

names.o: names.h
enum.o: enum.h enum.cc protoc_middleman
field.o: field.h
service.o: service.h service.cc
message.o:  message.h enum.h
generator.o: generator.h
literals.o: literals.h
file.o: file.h enum.h message.h service.h
main.o: main.cc
