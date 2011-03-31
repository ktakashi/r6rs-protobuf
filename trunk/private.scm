;; private.scm: private definitions and support API for r6rs-protobuf
;; Copyright (C) 2011 Julian Graham

;; r6rs-protobuf is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library (protobuf private)
  (export protobuf:field-type-descriptor-default
          
          protobuf:field-type-double
	  protobuf:field-type-float
	  protobuf:field-type-int32
	  protobuf:field-type-int64
	  protobuf:field-type-uint32
	  protobuf:field-type-uint64
	  protobuf:field-type-sint32
	  protobuf:field-type-sint64
	  protobuf:field-type-fixed32
	  protobuf:field-type-fixed64
	  protobuf:field-type-sfixed32
	  protobuf:field-type-sfixed64
	  protobuf:field-type-bool
	  protobuf:field-type-string
	  protobuf:field-type-bytes
	  protobuf:field-type-message

	  protobuf:make-field-descriptor
	  protobuf:field-descriptor-default
	  protobuf:field-descriptor-name
	  
	  protobuf:field-field-descriptor
	  protobuf:field-value
	  protobuf:field-has-value?
	  protobuf:set-field-value!
	  protobuf:clear-field!

	  protobuf:message-builder-build
	  protobuf:message-builder-field
	  
	  protobuf:message-write
	  protobuf:message-read)
  (import (rnrs))

  (define (zigzag-encode n bits)
    (bitwise-xor (bitwise-arithmetic-shift-left n 1)
		 (bitwise-arithmetic-shift-right n (- bits 1))))

  (define (zigzag-decode n bits) 
    (- (bitwise-arithmetic-shift-right n 1)
       (* (bitwise-and n 1) n)))
    
  (define (write-varint port varint)
    (let ((b (bitwise-bit-field varint 0 7)))
      (if (> varint 127)
	  (begin (put-u8 
		  port (bitwise-ior (bitwise-arithmetic-shift-left 1 7) b))
		 (write-varint port (bitwise-arithmetic-shift-right varint 7)))
	  (put-u8 port b))))

  (define (read-varint port)
    (define (read-varint-inner port tally septets)
      (let* ((b (get-u8 port))
	     (tally (bitwise-ior (bitwise-arithmetic-shift-left 
				  (bitwise-bit-field b 0 7) (* septets 7)) 
				 tally)))
	(if (bitwise-bit-set? b 7)
	    (read-varint-inner port tally (+ septets 1))
	    tally)))
    (read-varint-inner port 0 0))

  (define (write-double port double)
    (let ((vec (make-bytevector 8)))
      (bytevector-ieee-double-set! vec 0 double (endianness little))
      (put-bytevector port vec)))

  (define (write-float port float)
    (let ((vec (make-bytevector 4)))
      (bytevector-ieee-single-set! vec 0 float (endianness little))
      (put-bytevector port vec)))

  (define (write-int32 port int32) (write-varint port int32))
  (define (write-int64 port int64) (write-varint port int64))
  (define (write-uint32 port uint32) (write-varint port uint32))
  (define (write-uint64 port uint64) (write-varint port uint64))
  (define (write-sint32 port sint32) 
    (write-varint port (zigzag-encode sint32 32)))
  (define (write-sint64 port sint64)
    (write-varint port (zigzag-encode sint64 64)))
  (define (write-fixed32 port fixed32)
    (let ((vec (make-bytevector 4)))
      (bytevector-u32-set! vec 0 fixed32 (endianness little))
      (put-bytevector port vec)))

  (define (write-fixed64 port fixed64)
    (let ((vec (make-bytevector 8)))
      (bytevector-u64-set! vec 0 fixed64 (endianness little))
      (put-bytevector port vec)))

  (define (write-sfixed32 port sfixed32)
    (let ((vec (make-bytevector 4)))
      (bytevector-s32-set! vec 0 sfixed32 (endianness little))
      (put-bytevector port vec)))

  (define (write-sfixed64 port sfixed64)
    (let ((vec (make-bytevector 8)))
      (bytevector-s64-set! vec 0 sfixed64 (endianness little))
      (put-bytevector port vec)))

  (define (write-bool port bool) (put-u8 port (if bool 1 0)))

  (define (write-string port string)
    (write-varint port (string-length string))
    (put-bytevector port (string->utf8 string)))

  (define (write-bytes port bytes) 
    (write-varint port (bytevector-length bytes)) 
    (put-bytevector port bytes))

  (define (read-double port)
    (bytevector-ieee-double-ref 
     (get-bytevector-n port 8) 0 (endianness little)))
  (define (read-float port) 
    (bytevector-ieee-single-ref 
     (get-bytevector-n port 4) 0 (endianness little)))
  (define (read-int32 port) (read-varint port))
  (define (read-int64 port) (read-varint port))
  (define (read-uint32 port) (read-varint port))
  (define (read-uint64 port) (read-varint port))
  (define (read-sint32 port) (zigzag-decode (read-varint port) 32))
  (define (read-sint64 port) (zigzag-decode (read-varint port) 64))
  (define (read-fixed32 port) 
    (bytevector-u32-ref (get-bytevector-n port 4) 0 (endianness little)))
  (define (read-fixed64 port) 
    (bytevector-u64-ref (get-bytevector-n port 8) 0 (endianness little)))
  (define (read-sfixed32 port) 
    (bytevector-s32-ref (get-bytevector-n port 4) 0 (endianness little)))
  (define (read-sfixed64 port) 
    (bytevector-s64-ref (get-bytevector-n port 8) 0 (endianness little)))
  (define (read-bool port) 
    (case (get-u8 port) 
      ((0) #f) ((1) #t) (else (raise (make-assertion-violation)))))
  (define (read-string port) 
    (utf8->string (get-bytevector-n port (read-varint port))))
  (define (read-bytes port) (get-bytevector-n port (read-varint port)))

  (define-enumeration 
    wire-type (varint fixed64 length-delimited fixed32) wire-types)
  (define-enumeration
    field-type 
    (double float int32 int64 uint32 uint64 sint32 sint64 fixed32 fixed64 
     sfixed32 sfixed64 bool string bytes message) 
    field-types)
  
  (define-record-type (protobuf:field-type-descriptor
		       protobuf:make-field-type-descriptor
		       protobuf:field-type-descriptor?)
    (fields name wire-type serializer deserializer predicate default))

  (define-record-type (protobuf:field-descriptor
		       protobuf:make-field-descriptor
		       protobuf:field-descriptor?)
    (fields index name type repeated? required? default))
 
  (define-record-type (protobuf:message-field-descriptor
		       protobuf:make-message-field-descriptor
		       protobuf:message-field-descriptor?)
    (parent field-descriptor)
    (fields message-type))

  (define-record-type (protobuf:field protobuf:make-field protobuf:field?)
    (fields
     (mutable value protobuf:field-value protobuf:set-field-value-internal!)
     (immutable descriptor protobuf:field-field-descriptor)
     (mutable has-value? 
	      protobuf:field-has-value? 
	      protobuf:set-field-has-value!))
    (protocol 
     (lambda (p)
       (lambda (descriptor . value)
	 (if (null? value)
	     (p (protobuf:field-descriptor-default descriptor) descriptor #f)
	     (p (car value) descriptor #t))))))

  (define (clone-field field)
    (if (protobuf:field-has-value? field)
	(protobuf:make-field (protobuf:field-field-descriptor field) 
			     (protobuf:field-value field))
	(protobuf:make-field (protobuf:field-field-descriptor field))))

  (define-record-type (protobuf:message protobuf:make-message protobuf:message?)
    (fields fields)
    (protocol (lambda (p) (lambda (fields) (p (map clone-field fields))))))
  
  (define-record-type (protobuf:message-builder 
		       protobuf:make-message-builder 
		       protobuf:message-builder?)
    (fields type fields)
    (protocol 
     (lambda (p) 
       (lambda (type field-descriptors) 
	 (define fields 
	   (map (lambda (x) (protobuf:make-field x)) field-descriptors))
	 (p type (list->vector fields))))))
     
  (define (protobuf:message-builder-build b)
    (define (ensure-required f)
      (let ((fd (protobuf:field-field-descriptor f)))
	(if (and (protobuf:field-descriptor-required? fd)
		 (not (protobuf:field-has-value? f)))
	    (raise (condition 
		    (make-assertion-violation)
		    (make-message-condition 
		     (string-append "Field " 
				    (protobuf:field-descriptor-name fd) 
				    " is required.")))))))

    (let* ((type (protobuf:message-builder-type b))
	   (ctor (record-constructor 
		  (make-record-constructor-descriptor type #f #f)))
	   (fields (protobuf:message-builder-fields b)))
      (vector-for-each ensure-required fields)
      (let ((cfs (vector-map clone-field fields))) 
	(apply ctor (cons cfs (vector->list
			       (vector-map protobuf:field-value cfs)))))))
  
  (define (protobuf:message-builder-field builder index)
    (find (lambda (x)
	    (eqv? (protobuf:field-descriptor-index 
		   (protobuf:field-field-descriptor x)) 
		  index))
	  (vector->list (protobuf:message-builder-fields builder))))

  (define (protobuf:set-field-value! field value)
    (let* ((field-descriptor (protobuf:field-field-descriptor field))
	   (type-descriptor (protobuf:field-descriptor-type field-descriptor))
	   (predicate (protobuf:field-type-descriptor-predicate 
		       type-descriptor)))
      (if (or (protobuf:field-descriptor-repeated? field-descriptor)
	      (not (predicate value)))
	  (raise (make-assertion-violation))
	  (begin 
	    (protobuf:set-field-value-internal! field value)
	    (protobuf:set-field-has-value! field #t)))))

  (define (protobuf:clear-field! field)
    (protobuf:set-field-has-value! field #f)
    (protobuf:set-field-value-internal! 
     field (protobuf:field-descriptor-default 
	    (protobuf:field-field-descriptor field))))

  (define (int32? obj) 
    (and (integer? obj) (>= obj -2147483648) (<= obj 2147483647)))
  (define (uint32? obj) (and (integer? obj) (>= obj 0) (<= 4294967295)))
  (define (int64? obj)
    (and (integer? obj) 
	 (>= obj -9223372036854775808) 
	 (<= obj 9223372036854775807)))
  (define (uint64? obj) 
    (and (integer? obj) (>= obj 0) (<= 18446744073709551615)))
  
  (define protobuf:field-type-double 
    (protobuf:make-field-type-descriptor 
     (field-type double) (wire-type fixed64) write-double read-double real? 0))

  (define protobuf:field-type-float 
    (protobuf:make-field-type-descriptor 
     (field-type float) (wire-type fixed32) write-float read-float real? 0))

  (define protobuf:field-type-int32 
    (protobuf:make-field-type-descriptor 
     (field-type int32) (wire-type varint) write-int32 read-int32 int32? 0))

  (define protobuf:field-type-int64 
    (protobuf:make-field-type-descriptor 
     (field-type int64) (wire-type varint) write-int64 read-int64 int64? 0))

  (define protobuf:field-type-uint32
    (protobuf:make-field-type-descriptor 
     (field-type uint32) (wire-type varint) write-uint32 read-uint32 uint32? 0))

  (define protobuf:field-type-uint64 
    (protobuf:make-field-type-descriptor
     (field-type uint64) (wire-type varint) write-uint64 read-uint64 uint64? 0))

  (define protobuf:field-type-sint32 
    (protobuf:make-field-type-descriptor 
     (field-type sint32) (wire-type varint) write-sint32 read-sint32 int32? 0))

  (define protobuf:field-type-sint64 
    (protobuf:make-field-type-descriptor 
     (field-type sint64) (wire-type varint) write-sint64 read-sint64 int64? 0))

  (define protobuf:field-type-fixed32 
    (protobuf:make-field-type-descriptor 
     (field-type fixed32) 
     (wire-type fixed32) write-fixed32 read-fixed32 int32? 0))

  (define protobuf:field-type-fixed64 
    (protobuf:make-field-type-descriptor 
     (field-type fixed64) 
     (wire-type fixed64) write-fixed64 read-fixed64 int64? 0))

  (define protobuf:field-type-sfixed32
    (protobuf:make-field-type-descriptor
     (field-type sfixed32)
     (wire-type fixed32) write-sfixed32 read-sfixed32 int32? 0))

  (define protobuf:field-type-sfixed64 
    (protobuf:make-field-type-descriptor 
     (field-type sfixed64)
     (wire-type fixed64) write-sfixed64 read-sfixed64 int64? 0))

  (define protobuf:field-type-bool
    (protobuf:make-field-type-descriptor 
     (field-type bool) (wire-type varint) write-bool read-bool boolean? #f))

  (define protobuf:field-type-string
    (protobuf:make-field-type-descriptor
     (field-type string) 
     (wire-type length-delimited) write-string read-string string? ""))

  (define protobuf:field-type-bytes
    (protobuf:make-field-type-descriptor 
     (field-type bytes) 
     (wire-type length-delimited) 
     write-bytes read-bytes bytevector? (make-bytevector 0)))

  (define (protobuf:message-write obj port)    
    (define (write-field field)
      (define (wire-type->ordinal wire-type)
	(case wire-type
	  ((varint) 0)
	  ((fixed64) 1)
	  ((length-delimited) 2)
	  ((fixed32) 5)))

      (define field-descriptor (protobuf:field-field-descriptor field))
      (define type-descriptor (protobuf:field-descriptor-type field-descriptor))
      (if (protobuf:field-has-value? field)
	  (begin
	    (write-varint 
	     port (bitwise-ior 
		   (bitwise-arithmetic-shift-left
		    (protobuf:field-descriptor-index field-descriptor) 3)
		   (wire-type->ordinal
		    (protobuf:field-type-descriptor-wire-type 
		     type-descriptor))))
	    (let ((writer (protobuf:field-type-descriptor-serializer 
			   type-descriptor)))
	      (writer port (protobuf:field-value field))))))
    (vector-for-each write-field (protobuf:message-fields obj)))
    
  (define (protobuf:message-read builder port)
    (define field-table (make-hashtable (lambda (idx) idx) eqv?))

    (define (read-fields)
      (define (read-field)
	(define (ordinal->wire-type ordinal)
	  (case ordinal
	    ((0) (wire-type varint))
	    ((1) (wire-type fixed64))
	    ((2) (wire-type length-delimited))
	    ((5) (wire-type fixed32))
	    (else (raise (make-assertion-violation)))))
	(let* ((field-header (read-varint port))
	       (wire-type (ordinal->wire-type (bitwise-and field-header 7)))
	       (field-number (bitwise-arithmetic-shift-right field-header 3)))
	  
	  (if (hashtable-contains? field-table field-number)
	      (let* ((field (hashtable-ref field-table field-number #f))
		     (deserializer (protobuf:field-type-descriptor-deserializer
				    (protobuf:field-descriptor-type 
				     (protobuf:field-field-descriptor field)))))
		
		(protobuf:set-field-value! field (deserializer port)))
	      (raise (make-assertion-violation)))))

      (if (port-eof? port)
	  (protobuf:message-builder-build builder)
	  (begin (read-field) (read-fields))))    
    
    (vector-for-each 
     (lambda (field)
       (hashtable-set! field-table 
		       (protobuf:field-descriptor-index 
			(protobuf:field-field-descriptor field)) 
		       field))

     (protobuf:message-builder-fields builder))
    (read-fields))
)
