# Protocol Buffers for R6RS Scheme #

## What is it? ##

This project provides a pure Scheme implementation of Protocol Buffers, including parsing and code generation.  Visit the [protobuf project page](http://code.google.com/p/protobuf/) for information about the Protocol Buffers description language and the Protocol Buffers wire protocol.

## Downloads ##

Links to the latest r6rs-protobuf distribution can be found on the left.

As of January 15th, 2014, Google Code has disabled the addition of new releases to the Download section of hosted projects. As such, all new releases of r6rs-protobuf (beginning with 0.7) will be available for download from this project's associated [Google Drive folder](https://googledrive.com/host/0B07mS-YQv5YtY09TczFfUG54bFk/). Releases of r6rs-protobuf through 0.6 are still available [here](http://code.google.com/p/r6rs-protobuf/downloads/list).

## Latest Updates ##

http://r6rs-protobuf.googlecode.com/svn/trunk/CHANGES.txt

## Documentation ##

[Read the documentation.](http://r6rs-protobuf.googlecode.com/svn/trunk/README)

## Discussion ##

[Visit the discussion group.](http://groups.google.com/group/r6rs-protobuf)

## Quick Example ##

You write a .proto file like this

```
package protobuf.person;

message Person {
  required int32 id = 1;
  required string name = 2;
  optional string email = 3;
}
```

and compile it like this

```
(import (protobuf compile))
(define proto (protoc:read-proto (open-input-file "Person.proto")))
(protoc:generate-libraries proto)
```

to produce an R6RS library form like this

```
(library (protobuf person)
  (export make-Person-builder	  
	  Person-builder?
          Person-builder-build
	  
	  Person-builder-id
	  set-Person-builder-id!
	  has-Person-builder-id?
	  clear-Person-builder-id!

	  Person-builder-name
	  set-Person-builder-name!
	  has-Person-builder-name?
	  clear-Person-builder-name!

	  Person-builder-email
	  set-Person-builder-email!
	  has-Person-builder-email?
	  clear-Person-builder-email!

	  Person-builder-extension
	  set-Person-builder-extension!
	  has-Person-builder-extension?
	  clear-Person-builder-extension!

	  Person?
	  Person-id
	  Person-name
	  Person-email
          has-Person-email?

	  has-Person-extension?
	  Person-extension

	  Person-read
	  Person-write)

  (import (rnrs base)
	  (rnrs enums)
	  (rnrs records syntactic)
	  (protobuf private))

  ...
)
```