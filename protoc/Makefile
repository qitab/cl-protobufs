# Copyright 2016-2020 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.


# This Makefile depends on the Google protobuf library.
# See ../README.md for information on installing that.


# Directory into which the Lisp protocol buffer plugin,
# protoc-gen-cl-pb, will be installed.
INSTALL_ROOT ?= /usr/local/bin

# Directory where Google's protocol buffer compiler is installed. This
# should be the same directory you gave as the --prefix option to
# ./configure when installing protobuf.
PROTOC_ROOT ?= /usr/local
PROTOC      ?= $(PROTOC_ROOT)/bin/protoc
PROTOC_DEPS ?= -lutf8_validity -lutf8_range  -labsl_symbolize -labsl_bad_any_cast_impl -labsl_bad_optional_access -labsl_bad_variant_access -labsl_base -labsl_city -labsl_civil_time -labsl_cord -labsl_cord_internal -labsl_cordz_functions -labsl_cordz_handle -labsl_cordz_info -labsl_cordz_sample_token -labsl_crc32c -labsl_crc_cord_state -labsl_crc_cpu_detect -labsl_crc_internal -labsl_debugging_internal -labsl_demangle_internal -labsl_die_if_null -labsl_examine_stack -labsl_exponential_biased -labsl_failure_signal_handler -labsl_flags -labsl_flags_commandlineflag -labsl_flags_commandlineflag_internal -labsl_flags_config -labsl_flags_internal -labsl_flags_marshalling -labsl_flags_parse -labsl_flags_private_handle_accessor -labsl_flags_program_name -labsl_flags_reflection -labsl_flags_usage -labsl_flags_usage_internal -labsl_graphcycles_internal -labsl_hash -labsl_hashtablez_sampler -labsl_int128 -labsl_leak_check -labsl_log_entry -labsl_log_flags -labsl_log_globals -labsl_log_initialize -labsl_log_internal_check_op -labsl_log_internal_conditions -labsl_log_internal_format -labsl_log_internal_globals -labsl_log_internal_log_sink_set -labsl_log_internal_message -labsl_log_internal_nullguard -labsl_log_internal_proto -labsl_log_severity -labsl_log_sink -labsl_low_level_hash -labsl_malloc_internal -labsl_periodic_sampler -labsl_random_distributions -labsl_random_internal_distribution_test_util -labsl_random_internal_platform -labsl_random_internal_pool_urbg -labsl_random_internal_randen -labsl_random_internal_randen_hwaes -labsl_random_internal_randen_hwaes_impl -labsl_random_internal_randen_slow -labsl_random_internal_seed_material -labsl_random_seed_gen_exception -labsl_random_seed_sequences -labsl_raw_hash_set -labsl_raw_logging_internal -labsl_scoped_set_env -labsl_spinlock_wait -labsl_stacktrace -labsl_status -labsl_statusor -labsl_strerror -labsl_str_format_internal -labsl_strings -labsl_strings_internal -labsl_synchronization -labsl_throw_delegate -labsl_time -labsl_time_zone 

OFILES = enum.o field.o file.o generator.o literals.o message.o \
	names.o service.o proto2-descriptor-extensions.o main.o

CXXFLAGS = -std=c++14 -I$(PROTOC_ROOT)/include -I.
LIBS = -Wl,-L$(PROTOC_ROOT)/lib -Wl,-rpath -Wl,$(PROTOC_ROOT)/lib -lprotoc -lprotobuf -pthread $(PROTOC_DEPS) $(PROTOC_DEPS)

protoc-gen-cl-pb: $(OFILES) Makefile
	$(CXX) $(OFILES) -o protoc-gen-cl-pb $(LIBS)

install: protoc-gen-cl-pb
	mkdir -p $(INSTALL_ROOT)
	cp protoc-gen-cl-pb $(INSTALL_ROOT)/

clean: .
	rm -f $(OFILES) protoc_middleman protoc-gen-cl-pb

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
