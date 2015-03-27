;; private.scm: private definitions and support API for r6rs-protobuf
;; Copyright (C) 2014 Julian Graham

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
  (export protobuf:make-field-type-descriptor
	  protobuf:field-type-descriptor-default
	  protobuf:field-type-descriptor-name
	  protobuf:field-type-descriptor-predicate
	  protobuf:field-type-descriptor-wire-type

	  protobuf:make-message-field-type-descriptor
	  protobuf:message-field-type-descriptor?
	  protobuf:message-field-type-descriptor-definition

	  protobuf:make-enum-field-type-descriptor
	  protobuf:enum-field-type-descriptor?
	  protobuf:enum-field-type-descriptor-definition

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

	  protobuf:make-field-descriptor
	  protobuf:field-descriptor-default
	  protobuf:field-descriptor-name

	  protobuf:make-extension-field-descriptor

	  protobuf:make-field
	  protobuf:field-field-descriptor
	  protobuf:field-value
	  protobuf:field-has-value?
	  protobuf:set-field-value!
	  protobuf:clear-field!

	  protobuf:register-extension

	  protobuf:message-builder
	  protobuf:message-builder-build
	  protobuf:message-builder-field
	  protobuf:clear-message-builder-extension!
	  protobuf:message-builder-extension
	  protobuf:message-builder-has-extension?
	  protobuf:set-message-builder-extension!
	  
	  protobuf:message
	  protobuf:make-message
	  protobuf:message-extension
	  protobuf:message-field
	  protobuf:message-has-extension?
	  protobuf:message-write
	  protobuf:message-read

	  protobuf:read-varint
	  protobuf:write-varint

	  protobuf:write-double
	  protobuf:write-float
	  protobuf:write-int32
	  protobuf:write-int64
	  protobuf:write-uint32
	  protobuf:write-uint64
	  protobuf:write-sint32
	  protobuf:write-sint64
	  protobuf:write-fixed32
	  protobuf:write-fixed64
	  protobuf:write-sfixed32
	  protobuf:write-sfixed64
	  protobuf:write-bool
	  protobuf:write-string
	  protobuf:write-bytes
	  protobuf:write-message)

  (import (rnrs))

  (define (zigzag-encode n bits)
    (bitwise-xor (bitwise-arithmetic-shift-left n 1)
		 (bitwise-arithmetic-shift-right n (- bits 1))))

  (define (zigzag-decode n bits) 
    (- (bitwise-arithmetic-shift-right n 1)
       (* (bitwise-and n 1) n)))
    
  (define (protobuf:write-varint port varint)
    (let ((b (bitwise-bit-field varint 0 7)))
      (if (> varint 127)
	  (begin (put-u8 
		  port (bitwise-ior (bitwise-arithmetic-shift-left 1 7) b))
		 (protobuf:write-varint 
		  port (bitwise-arithmetic-shift-right varint 7)))
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

  (define protobuf:read-varint read-varint)

  (define (protobuf:write-double port double)
    (let ((vec (make-bytevector 8)))
      (bytevector-ieee-double-set! vec 0 double (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-float port float)
    (let ((vec (make-bytevector 4)))
      (bytevector-ieee-single-set! vec 0 float (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-int32 port int32) (protobuf:write-varint port int32))
  (define (protobuf:write-int64 port int64) (protobuf:write-varint port int64))
  (define (protobuf:write-uint32 port uint32) 
    (protobuf:write-varint port uint32))
  (define (protobuf:write-uint64 port uint64) 
    (protobuf:write-varint port uint64))
  (define (protobuf:write-sint32 port sint32) 
    (protobuf:write-varint port (zigzag-encode sint32 32)))
  (define (protobuf:write-sint64 port sint64)
    (protobuf:write-varint port (zigzag-encode sint64 64)))
  (define (protobuf:write-fixed32 port fixed32)
    (let ((vec (make-bytevector 4)))
      (bytevector-u32-set! vec 0 fixed32 (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-fixed64 port fixed64)
    (let ((vec (make-bytevector 8)))
      (bytevector-u64-set! vec 0 fixed64 (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-sfixed32 port sfixed32)
    (let ((vec (make-bytevector 4)))
      (bytevector-s32-set! vec 0 sfixed32 (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-sfixed64 port sfixed64)
    (let ((vec (make-bytevector 8)))
      (bytevector-s64-set! vec 0 sfixed64 (endianness little))
      (put-bytevector port vec)))

  (define (protobuf:write-bool port bool) (put-u8 port (if bool 1 0)))

  (define (protobuf:write-string port string)
    (protobuf:write-varint port (string-length string))
    (put-bytevector port (string->utf8 string)))

  (define (protobuf:write-bytes port bytes) 
    (protobuf:write-varint port (bytevector-length bytes)) 
    (put-bytevector port bytes))

  (define (protobuf:write-message port message) 
    (protobuf:message-write message port))

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
    wire-type 
    (varint fixed64 length-delimited start-group end-group fixed32) 
    wire-types)

  (define-enumeration
    field-type 
    (double float int32 int64 uint32 uint64 sint32 sint64 fixed32 fixed64 
     sfixed32 sfixed64 bool string bytes message) 
    field-types)
  
  (define-record-type (protobuf:field-type-descriptor
		       protobuf:make-field-type-descriptor
		       protobuf:field-type-descriptor?)
    (fields name wire-type serializer deserializer predicate default))

  (define-record-type (protobuf:message-field-type-descriptor
		       protobuf:make-message-field-type-descriptor
		       protobuf:message-field-type-descriptor?)
    (parent protobuf:field-type-descriptor)
    (fields definition))

  (define-record-type (protobuf:enum-field-type-descriptor
		       protobuf:make-enum-field-type-descriptor
		       protobuf:enum-field-type-descriptor?)
    (parent protobuf:field-type-descriptor)
    (fields definition))

  (define-record-type (protobuf:field-descriptor
		       protobuf:make-field-descriptor
		       protobuf:field-descriptor?)
    (fields index name type repeated? required? default)) 

  (define-record-type (protobuf:extension-field-descriptor
		       protobuf:make-extension-field-descriptor
		       protobuf:extension-field-descriptor?)
    (parent protobuf:field-descriptor)
    (opaque #t))

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

  (define-record-type (protobuf:message protobuf:make-message protobuf:message?)
    (fields fields extension-fields))
  
  (define-record-type (protobuf:message-builder 
		       protobuf:make-message-builder 
		       protobuf:message-builder?)
    (fields type fields extension-fields)
    (protocol 
     (lambda (p)
       (lambda (type field-descriptors) 
	 ;; type must be RTD, not defined record type which might be a
	 ;; mere macro in some implementations (e.g. Chez, Mosh and Vicare)
	 (unless (record-type-descriptor? type)
	   (assertion-violation 'protobuf:make-message-builder 
				"type must be a record type descriptor"
				type))
	 (p type
	    (map protobuf:make-field 
		 (list-sort (lambda (f1 f2)
			      (< (protobuf:field-descriptor-index f1)
				 (protobuf:field-descriptor-index f2)))
			    field-descriptors))
	    (make-eqv-hashtable))))))

  (define (protobuf:message-field message index)
    (find (lambda (x)
	    (eqv? (protobuf:field-descriptor-index 
		   (protobuf:field-field-descriptor x)) 
		  index))
	  (protobuf:message-fields message)))

  (define extension-registry (make-eq-hashtable))
  
  (define (protobuf:register-extension prototype fd)
    (define type (protobuf:message-builder-type prototype))
    (define (update exts) 
      (hashtable-set! exts (protobuf:field-descriptor-index fd) fd) exts)
    (hashtable-update! extension-registry type update (make-eqv-hashtable)))
  
  (define (assert-registered-extension type fd)
    (or (hashtable-contains? 
	 (hashtable-ref extension-registry type (make-eqv-hashtable)) 
	 (protobuf:field-descriptor-index fd))
	(raise (condition (make-assertion-violation)
			  (make-message-condition "Unknown extension.")))))

  (define (protobuf:message-has-extension? m type fd)
    (assert-registered-extension type fd)
    (hashtable-contains? (protobuf:message-extension-fields m) 
			 (protobuf:field-descriptor-index fd)))

  (define (protobuf:message-extension m type fd)
    (assert-registered-extension type fd)
    (let ((field (hashtable-ref (protobuf:message-extension-fields m)
				(protobuf:field-descriptor-index fd)
				#f)))
      (if field
	  (protobuf:field-value field) 
	  (protobuf:field-descriptor-default fd))))

  (define (protobuf:message-builder-has-extension? b fd)
    (assert-registered-extension (protobuf:message-builder-type b) fd)
    (hashtable-contains? (protobuf:message-builder-extension-fields b)
			 (protobuf:field-descriptor-index fd)))

  (define (protobuf:clear-message-builder-extension! b fd)
    (assert-registered-extension (protobuf:message-builder-type b) fd)
    (hashtable-delete! (protobuf:message-builder-extension-fields b) 
		       (protobuf:field-descriptor-index fd)))

  (define (protobuf:set-message-builder-extension! b fd val)
    (assert-registered-extension (protobuf:message-builder-type b) fd)
    (let ((f (protobuf:make-field fd)))
      (protobuf:set-field-value! f val)
      (hashtable-set! (protobuf:message-builder-extension-fields b) 
		      (protobuf:field-descriptor-index fd)
		      f)))

  (define (protobuf:message-builder-extension b fd)
    (assert-registered-extension (protobuf:message-builder-type b) fd)
    (let ((field (hashtable-ref (protobuf:message-builder-extension-fields b)
				(protobuf:field-descriptor-index fd)
				#f)))
      (if field 
	  (protobuf:field-value field) 
	  (protobuf:field-descriptor-default fd))))

  (define (protobuf:message-builder-build b)
    (define (clone-field field)
      (if (protobuf:field-has-value? field)	     
	  (if (protobuf:field-descriptor-repeated?
	       (protobuf:field-field-descriptor field))
	      (protobuf:make-field 
	       (protobuf:field-field-descriptor field)
	       (list->vector (protobuf:field-value field)))
	      (protobuf:make-field (protobuf:field-field-descriptor field) 
				   (protobuf:field-value field)))
	  (if (protobuf:field-descriptor-repeated?
	       (protobuf:field-field-descriptor field))
	      (protobuf:make-field 
	       (protobuf:field-field-descriptor field) (vector))
	      (protobuf:make-field (protobuf:field-field-descriptor field)))))

    (define (clone-extensions extension-fields)
      (let ((ht (make-eqv-hashtable)))
	(vector-for-each 
	 (lambda (k)
	   (hashtable-set! 
	    ht k (clone-field (hashtable-ref extension-fields k #f)))) 
	 (hashtable-keys extension-fields))
	ht))
      
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
	    
      (for-each ensure-required fields)
      (let ((cfs (map clone-field fields))
	    (ecfs (clone-extensions 
		   (protobuf:message-builder-extension-fields b))))
	(apply ctor (cons cfs (cons ecfs (map protobuf:field-value cfs)))))))

  (define (protobuf:message-builder-field builder index)
    (find (lambda (x)
	    (eqv? (protobuf:field-descriptor-index 
		   (protobuf:field-field-descriptor x)) 
		  index))
	  (protobuf:message-builder-fields builder)))

  (define (protobuf:set-field-value! field value)
    (let* ((field-descriptor (protobuf:field-field-descriptor field))
	   (type-descriptor (protobuf:field-descriptor-type field-descriptor))
	   (predicate (protobuf:field-type-descriptor-predicate 
		       type-descriptor)))
      (if (protobuf:field-descriptor-repeated? field-descriptor)
	  (begin (if (not (list? value))
		     (raise (condition
			     (make-assertion-violation)
			     (make-message-condition
			      (string-append "Repeated field "
					     (protobuf:field-descriptor-name 
					      field-descriptor)
					     " must be a list"))))
		 (if (not (for-all predicate value))
		     (raise (condition
			     (make-assertion-violation)
			     (make-message-condition
			      (string-append 
			       "Wrong type in value list for field "
			       (protobuf:field-descriptor-name 
				field-descriptor))))))))
	  
	  (if (not (predicate value)) 
	      (raise (condition 
		      (make-assertion-violation)
		      (make-message-condition
		       (string-append
			"Wrong type for field " 
			(protobuf:field-descriptor-name field-descriptor)))))))

      (protobuf:set-field-value-internal! field value)
      (protobuf:set-field-has-value! field #t)))

  (define (protobuf:clear-field! field)
    (let ((field-descriptor (protobuf:field-field-descriptor field)))
      (if (not (protobuf:field-descriptor-repeated? field-descriptor))
	  (protobuf:set-field-has-value! field #f))
      (protobuf:set-field-value-internal!
       field (protobuf:field-descriptor-default field-descriptor))))

  (define (int32? obj) 
    (and (integer? obj) (>= obj -2147483648) (<= obj 2147483647)))
  (define (uint32? obj) (and (integer? obj) (>= obj 0) (<= obj 4294967295)))
  (define (int64? obj)
    (and (integer? obj) 
	 (>= obj -9223372036854775808) 
	 (<= obj 9223372036854775807)))
  (define (uint64? obj) 
    (and (integer? obj) (>= obj 0) (<= obj 18446744073709551615)))
  
  (define protobuf:field-type-double 
    (protobuf:make-field-type-descriptor 
     (field-type double) 
     (wire-type fixed64) protobuf:write-double read-double real? 0))

  (define protobuf:field-type-float 
    (protobuf:make-field-type-descriptor 
     (field-type float) 
     (wire-type fixed32) protobuf:write-float read-float real? 0))

  (define protobuf:field-type-int32 
    (protobuf:make-field-type-descriptor 
     (field-type int32) 
     (wire-type varint) protobuf:write-int32 read-int32 int32? 0))

  (define protobuf:field-type-int64 
    (protobuf:make-field-type-descriptor 
     (field-type int64) 
     (wire-type varint) protobuf:write-int64 read-int64 int64? 0))

  (define protobuf:field-type-uint32
    (protobuf:make-field-type-descriptor 
     (field-type uint32) 
     (wire-type varint) protobuf:write-uint32 read-uint32 uint32? 0))

  (define protobuf:field-type-uint64 
    (protobuf:make-field-type-descriptor
     (field-type uint64) 
     (wire-type varint) protobuf:write-uint64 read-uint64 uint64? 0))

  (define protobuf:field-type-sint32 
    (protobuf:make-field-type-descriptor 
     (field-type sint32) 
     (wire-type varint) protobuf:write-sint32 read-sint32 int32? 0))

  (define protobuf:field-type-sint64 
    (protobuf:make-field-type-descriptor 
     (field-type sint64) 
     (wire-type varint) protobuf:write-sint64 read-sint64 int64? 0))

  (define protobuf:field-type-fixed32 
    (protobuf:make-field-type-descriptor 
     (field-type fixed32) 
     (wire-type fixed32) protobuf:write-fixed32 read-fixed32 int32? 0))

  (define protobuf:field-type-fixed64 
    (protobuf:make-field-type-descriptor 
     (field-type fixed64) 
     (wire-type fixed64) protobuf:write-fixed64 read-fixed64 int64? 0))

  (define protobuf:field-type-sfixed32
    (protobuf:make-field-type-descriptor
     (field-type sfixed32)
     (wire-type fixed32) protobuf:write-sfixed32 read-sfixed32 int32? 0))

  (define protobuf:field-type-sfixed64 
    (protobuf:make-field-type-descriptor 
     (field-type sfixed64)
     (wire-type fixed64) protobuf:write-sfixed64 read-sfixed64 int64? 0))

  (define protobuf:field-type-bool
    (protobuf:make-field-type-descriptor 
     (field-type bool) 
     (wire-type varint) protobuf:write-bool read-bool boolean? #f))

  (define protobuf:field-type-string
    (protobuf:make-field-type-descriptor
     (field-type string) 
     (wire-type length-delimited) protobuf:write-string read-string string? ""))

  (define protobuf:field-type-bytes
    (protobuf:make-field-type-descriptor 
     (field-type bytes) 
     (wire-type length-delimited) 
     protobuf:write-bytes read-bytes bytevector? (make-bytevector 0)))

  (define (protobuf:message-write obj port)    
    (define extension-fields
      (let ((efs (protobuf:message-extension-fields obj)))
	(let-values (((keys values) (hashtable-entries efs))) values)))

    (define (write-field field)

      (define (wire-type->ordinal wire-type)
	(case wire-type
	  ((varint) 0)
	  ((fixed64) 1)
	  ((length-delimited) 2)
	  ((fixed32) 5)))

      (define field-descriptor (protobuf:field-field-descriptor field))
      (define type-descriptor (protobuf:field-descriptor-type field-descriptor))
      (define serialize (protobuf:field-type-descriptor-serializer 
			 type-descriptor))
      
      (define (write-field-inner value)
	(protobuf:write-varint 
	 port (bitwise-ior
	       (bitwise-arithmetic-shift-left
		(protobuf:field-descriptor-index field-descriptor) 3)
	       (wire-type->ordinal
		(protobuf:field-type-descriptor-wire-type 
		 type-descriptor))))

	(if (protobuf:message? value)
	    (call-with-values
	      (lambda () (open-bytevector-output-port))
	      (lambda (sub-port get-bytevector)
		(protobuf:write-message sub-port value)
		(protobuf:write-bytes port (get-bytevector))))
	    (serialize port value)))

      (if (protobuf:field-has-value? field)
	  (if (protobuf:field-descriptor-repeated? field-descriptor)
	      (vector-for-each write-field-inner (protobuf:field-value field))
	      (write-field-inner (protobuf:field-value field)))))

    (for-each write-field (protobuf:message-fields obj))
    (vector-for-each write-field extension-fields))

  (define (protobuf:message-read builder port)
    (define field-table (make-eqv-hashtable))
    (define (lookup-field-metadata field-number)
      (let ((field (hashtable-ref field-table field-number #f)))
	(if field
	    (values field (protobuf:field-field-descriptor field))
	    (values #f (hashtable-ref (hashtable-ref 
				       extension-registry
				       (protobuf:message-builder-type builder)
				       (make-eqv-hashtable)) 
				      field-number 
				      #f)))))

    (define (read-fields)
      (define (read-field)
	(define (ordinal->wire-type ordinal)
	  (case ordinal
	    ((0) (wire-type varint))
	    ((1) (wire-type fixed64))
	    ((2) (wire-type length-delimited))
	    ((3) (wire-type start-group))
	    ((4) (wire-type end-group))
	    ((5) (wire-type fixed32))
	    (else (raise (make-assertion-violation)))))
	(let* ((field-header (read-varint port))
	       (wire-type (ordinal->wire-type (bitwise-and field-header 7)))
	       (field-number (bitwise-arithmetic-shift-right field-header 3)))
	  (let-values (((field field-descriptor) 
			(lookup-field-metadata field-number)))
	  
	    (if field-descriptor
		(let ((deserializer (protobuf:field-type-descriptor-deserializer
				     (protobuf:field-descriptor-type 
				      field-descriptor))))

		  (if (protobuf:field-descriptor-repeated? field-descriptor)
		      (if field
			  (protobuf:set-field-value!
			   field (append (protobuf:field-value field) 
					 (list (deserializer port))))
			  (protobuf:set-message-builder-extension! 
			   builder field-descriptor 
			   (append (protobuf:message-builder-extension 
				    builder field-descriptor)
				   (list (deserializer port)))))
		      (if field
			  (protobuf:set-field-value! field (deserializer port))
			  (protobuf:set-message-builder-extension!
			   builder field-descriptor (deserializer port)))))

	      ;; If we don't have metadata about the field, consume its content
	      ;; based on its wire type and then discard it.

	      (case wire-type
		((varint) (read-varint port))
		((fixed64) (read-fixed64 port))
		((length-delimited) (read-string port))
		((start-group) (read-string port))
		((end-group) #f)
		((fixed32) (read-fixed32 port)))))))

      (if (port-eof? port)
	  (protobuf:message-builder-build builder)
	  (begin (read-field) (read-fields))))    
    
    (for-each (lambda (field)
		(hashtable-set! field-table 
				(protobuf:field-descriptor-index 
				 (protobuf:field-field-descriptor field)) 
				field))
	      
	      (protobuf:message-builder-fields builder))
    (read-fields))
)
