# Copyright 2016-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

# First install the latest protobuf from github.
#
#  https://github.com/google/protobuf
#
# The steps should be something like:
#
#    cd /tmp
#    git clone https://github.com/google/protobuf google-protobuf
#    cd google-protobuf
#    ./autogen.sh
#    ./configure --prefix=~/local/software/package/google-protobuf
#    make
#    make check
#    make install
#
#  Locally clone this directory
#  In the protoc directory call make.

# Directory into which the Lisp protocol buffer plugin will be installed.
INSTALL_ROOT ?= $(HOME)/local/software/package/protoc-gen-lisp

# Directory where Google's protocol buffer compiler is installed.
PROTOC_ROOT ?= $(HOME)/local/software/package/google-protobuf

PROTOC ?= $(PROTOC_ROOT)/bin/protoc

PROTOC_INCLUDE = $(PROTOC_ROOT)/include
PROTOC_LIB = $(PROTOC_ROOT)/lib

OFILES = enum.o field.o file.o generator.o literals.o message.o \
	 names.o service.o main.o

CXXFLAGS = -std=c++14 -I$(PROTOC_INCLUDE) -I.
LIBS = -Wl,-L$(PROTOC_LIB) -Wl,-rpath -Wl,$(PROTOC_LIB) -lprotoc -lprotobuf -lpthread

protoc-gen-lisp: $(OFILES) Makefile
	$(CXX) -o main $(OFILES) $(LIBS)

install: protoc-gen-lisp
	mkdir -p $(INSTALL_ROOT)/bin
	cp protoc-gen-lisp $(INSTALL_ROOT)/bin

clean: .
	rm proto2-descriptor-extensions*
	rm -f protoc_middleman
	rm -f $(OFILES) protoc-gen-lisp

# Generate the proto2 description cpp files.
protoc_middleman: ../proto2-descriptor-extensions.proto
	$(PROTOC) -I=.. --cpp_out=. ../proto2-descriptor-extensions.proto
	@touch protoc_middleman

proto2-descriptor-extensions.o: protoc_middleman
	pkg-config --cflags protobuf  # fails if protobuf is not installed
	c++ -std=c++11  proto2-descriptor-extensions.pb.cc -o proto2-descriptor-extensions `pkg-config --cflags --libs protobuf`

names.o: names.h
enum.o: enum.h enum.cc protoc_middleman
field.o: field.h
service.o: service.h service.cc
message.o:  message.h enum.h
generator.o: generator.h
literals.o: literals.h
file.o: file.h enum.h message.h service.h
