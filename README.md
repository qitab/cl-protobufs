# cl-protobufs

CL-Protobufs is a common lisp protocol buffer library.

## API

All of the following interfaces are written for you by the protoc tool based
on a .proto file.

Note: The API is a work in progress.

### cl-protobufs:

```lisp
(defstruct base-message)
```
The base type every protobuf message inherits.

```lisp
(defun print-text-format (object &key (stream *standard-output*) (print-name t)
                                 (suppress-line-breaks *suppress-line-breaks*))
```
Prints a protocol buffer message to a stream.
The object is the protocol buffer message to print.
The stream is the stream to print to.
Can specify whether to print the top level proto message name with print-name.
Can specify whether to suppress-line-breaks.

```lisp
(defun is-initialized (object))
```
Check if OBJECT has every required field set, and recursively
all of its sub-objects have all of their required fields set.
A protobuf object that is not initialized may not be correctly
serialized/deserialized and may throw an error on
serialization/deserialization.
Object is the protobuf message to check. Will give an error
  if object is not a protobuf message.

```lisp
(defun proto-equal (message-1 message-2 &key exact nil))
```
Check if two protobuf messages are equal. By default
two messages are equal if calling the getter on each
field would retrieve the same value. This means that a
message with a field explicitly set to the default value
is considered the same as a message with that field not
set.

If EXACT is true, consider the messages to be equal
only if the same fields have been explicitly set.

MESSAGE-1 and MESSAGE-2 should both be protobuf
message objects.

```lisp
(defgeneric clear (object message))
```
Resets the protobuf message to its init state.

```lisp
(defun has-field (object field))
```
Returns a bool describing whether the object has the field set.
Object is the object which may have field defined.
Field is the symbol package::field-name of the field in the proto message.

```lisp
(defun proto-slot-value (object slot))
```
Returns the value of SLOT in OBJECT.
Object is the lisp proto object.
Slot is the symbol package::field-name of the field in the proto message.
Deprecated, instead use the accessors specified below.

```lisp
(defun (setf proto-slot-value) (value object slot))
```
The setf function for proto-slot-value.
Deprecated, do not use.

```lisp
(defun proto-slot-is-set (object slot))
```
Deprecated, instead use the accessors setter specified below.

### Proto package

We assume that the protobuf assigned lisp package package, and a message:

```protocol-buffer
lisp_package = "math";

message msg {
  optional int32 field = 1;
}
```

This will put the protobufs in the lisp package ```cl-protobufs.math``` package.

```lisp
(defun make-msg ($key field-names))
```
Construct the proto object for MSG setting the fields in FIELD-NAMES.

```lisp
(defun msg.field (object))
```
Get the value of FIELD in protobuf MSG from OBJECT.
If the field has a default and the field is unset it returns that default,
otherwise return a type specific default value of field bar.
For example, for an int32 field it should return 0 if unset.

```lisp
(defmethod field (object 'msg))
```
Same as msg.field.
Deprecated.

```lisp
(defun msg.has-field (object))
```
Returns a bool describing whether the OBJECT has FIELD set.
Object is the object which may have field defined.

```lisp
(defun msg.clear-field (object))
```
Clear the value of FIELD in OBJECT, setting it to the fields init state.
Object is the lisp proto object.

### Proto package-rpc2

We will now discuss the api for  a protobuf service in a proto file.
You must have a gRPC lisp library as well, cl-protobufs just generates
the methods.

We will use this example protocol buffer:


```protocol-buffer
lisp_package = "math";

message AddNumbersRequest {
  optional int32 number1 = 1;
  optional int32 number2 = 1;
}

message AddNumbersResponse {
  optional int32 AddNumbersResponse = 1;
}

Service MyService
  rpc AddNumbers(AddNumbersRequest) returns (AddNumbersResponse) {}
}
```

This will put the service forms in the lisp package ```cl-protobufs.math-rpc``` package.
For brevity we will alias ```cl-protobufs.math``` as ```math```.

```lisp
(defgeneric add-numbers-request-impl (channel (request math:add-numbers-request) rpc))
```
The implementation of the RPC call, "-impl" is prepended to the RPC name.
The REQUEST will be the proto defined by the AddNumbersRequest message.
The user should have the function return a AddNumbersResponse message proto.
The CHANNEL is the channel that is being used and the user can usually ignore it.
The RPC is the RPC object being used, and the user can usually ignore it.

The user must override this with a method, for example

```lisp
(defmethod add-numbers-impl (service (request math:add-numbers-request) rpc)
  (make-add-numbers-response :sum (+ (math:add-numbers-request.number1 request)
                                     (math:add-numbers-request.number2 request))))
```

```lisp
(defgeneric call-add-numbers (channel (request math:add-numbers-request) &key callback response))
```
Your proto library should implement an override for this function.
The CHANNEL should be the RPC to use.
The REQUEST is the AddNumbersRequest proto message use in the call.
The CALLBACK is a function to call when the call is over.
The RESPONSE is a proto to set for the response.

### Serialization

```lisp
(deftype byte-vector)
```
A vector of unsigned-bytes.
In serialization functions this is often referred to as buffer.

```lisp
(defun make-byte-vector (size &key adjustable))
```
Constructor to make a byte vector.
The SIZE is the size of the underlying vector.
ADJUSTABLE is a boolean value determining whether the byte-vector can change size.

```lisp
(defun serialize-object-to-bytes (object &optional (type (type-of object))))
```
Creates a byte-vector and serializes a protobuf message to that byte-vector.
The OBJECT is the protobuf message instance to serialize.
Optionally use TYPE to specify the type of object to serialize.

```lisp
(serialize-object-to-file (filename object &optional (type (type-of object))))
```
Serialize an protobuf message to a file.
Calls with-open-file using :direction :output and :element-type '(unsigned-byte 8).
The FILENAME is the name of the file to save the object to.
The OBJECT is the object instance to serialize.
Optionally use TYPE to specify the type of object to serialize.

```lisp
(defun serialize-object-to-stream (stream object &optional (type (type-of object)))
```
Serialize a protobuf message to a stream.
The STREAM is the stream instance to serialize the protobuf message to.
The OBJECT is the object to serialize.
Optionally use TYPE to specify the type of object to serialize.

```lisp
(defun deserialize-object (type buffer &optional (start 0) (end (length buffer))))
```
Deserialize a protobuf message returning the newly created structure.
The TYPE is the symbol of the protobuf message to deserialize.
The BUFFER is the byte-vector containing the object to deserialize.
START is the index in byte-vector at which the serialized object originates.
END is the last index in byte-vector of the serialized object.

## Example

In `./examples/math*` we give a simple example of creating a proto structure,
populating its fields, serializing, then deserializing.
All of the files listed below can be found in ./cl_protobufs/example/.

The proto file [`math.proto`](./example/math.proto) has two messages:
`AddNumbersRequest` and `AddNumbersResponse`.

The build files `BUILD` uses `proto_library` and `cl_protobufs_library`
rules to compile the proto code into lisp generated proto files.
We use `lisp_library` to build the lisp code and `lisp_test`
for our test code.

The prefix `cl-protobufs.` is automatically added to the package name as a
prefix to avoid conflicts with existing packages. You may also use the
`lisp_package` option to specify a package. To do this add:

```lisp
option (lisp_package) = "different-package"
```

under the package decleration and lisp will use `cl-protobufs.different-package`
as the package name.

The full name of the Lisp type for `AddNumbersRequest` is
`cl-protobufs.math:add-numbers-request`.

In [`math.lisp`](./example/math.lisp)
we make functions to add-numbers and serialize the request and response.

Finally [`math-test.lisp`](./example/math-test.lisp)
defines a few tests to see our math library works.

# Protoc

To get a Lisp Schema file one must first run protoc.
To run protoc install the latest protobuf from github.

https://github.com/google/protobuf

```shell
cd /tmp
git clone https://github.com/google/protobuf google-protobuf
cd google-protobuf
./autogen.sh
./configure --prefix=~/local/software/package/google-protobuf
make
make check
make install
```

Locally clone this github repo.
In the protoc directory call make.

Then run:

```shell
$(HOME)/local/software/package/protoc-gen-lisp /path/to/proto/file
```

# Notes on ASDF Build

It's currently possible to build cl-protobufs and cl-protobufs-test with ASDF,
but only the wire-tests target is included in the tests because the
:protobuf-file build action no longer works.

* Install [Quicklisp](http://quicklisp.org) and make sure to add it to your
  ~/.sbclrc file.
* Install ASDF. Normally SBCL would contain a recent version of ASDF but the
  //lisp Blaze target doesn't. You can copy asdf.lisp somewhere locally and load
  it from your ~/.sbclrc file. If you use an externally built SBCL you can skip
  this step.
* Create a link to cl-protobufs so that Quicklisp will use the local version:

  ```shell
  $ cd ~/quicklisp/local-projects
  $ ln -s $cl-protobufs
  ```

* Start SBCL and evaluate `(ql:quickload :cl-protobufs)`.
* Load and run the tests:

  ```lisp
  cl-user> (ql:quickload :cl-protobufs-test)
  cl-user> (clunit:run-suite 'cl-protobufs.wire-test::wire-tests)
  ```
