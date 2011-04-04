;; test-private.scm: private API test routines for r6rs-protobuf
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

(import (rnrs))
(import (protobuf private))
(import (srfi :64))

(test-begin "private")
(test-begin "read")

(define-record-type read-test-message
  (fields foo) (opaque #t) (parent protobuf:message) (sealed #t))

(define-record-type read-test-message-builder
  (fields (mutable foo))
  (parent protobuf:message-builder)
  (protocol 
   (lambda (p)
     (lambda ()
       (let ((n (p read-test-message
		   (list (protobuf:make-field-descriptor 
			  0 "foo" protobuf:field-type-string #f #t #f)))))
	 (n #f))))))

(define (make-field-header field-number wire-type-num)
  (bitwise-ior (bitwise-arithmetic-shift-left field-number 3) wire-type-num))

(test-begin "unknown-fields")

(test-group "varint"
  (let-values (((bv-out bv-transcoder) (open-bytevector-output-port)))
    (protobuf:write-varint bv-out (make-field-header 1 0))
    (protobuf:write-varint bv-out 256)
    (protobuf:write-varint bv-out (make-field-header 0 2))
    (protobuf:write-string bv-out "Test")
    (let ((m (protobuf:message-read 
	      (make-read-test-message-builder)
	      (open-bytevector-input-port (bv-transcoder)))))
      (test-assert (read-test-message? m))
      (test-equal "Test" (read-test-message-foo m)))))

(test-group "64-bit"
  (let-values (((bv-out bv-transcoder) (open-bytevector-output-port)))
    (protobuf:write-varint bv-out (make-field-header 1 1))
    (protobuf:write-fixed64 bv-out 256)
    (protobuf:write-varint bv-out (make-field-header 0 2))
    (protobuf:write-string bv-out "Test")
    (let ((m (protobuf:message-read 
	      (make-read-test-message-builder)
	      (open-bytevector-input-port (bv-transcoder)))))
      (test-assert (read-test-message? m))
      (test-equal "Test" (read-test-message-foo m)))))

(test-group "length-delimited"
  (let-values (((bv-out bv-transcoder) (open-bytevector-output-port)))
    (protobuf:write-varint bv-out (make-field-header 1 2))
    (protobuf:write-string bv-out "Ignore")
    (protobuf:write-varint bv-out (make-field-header 0 2))
    (protobuf:write-string bv-out "Test")
    (let ((m (protobuf:message-read 
	      (make-read-test-message-builder)
	      (open-bytevector-input-port (bv-transcoder)))))
      (test-assert (read-test-message? m))
      (test-equal "Test" (read-test-message-foo m)))))

(test-group "groups"
  (let-values (((bv-out bv-transcoder) (open-bytevector-output-port)))
    (protobuf:write-varint bv-out (make-field-header 1 3))
    (protobuf:write-string bv-out "[group contents]")
    (protobuf:write-varint bv-out (make-field-header 1 4))
    (protobuf:write-varint bv-out (make-field-header 0 2))
    (protobuf:write-string bv-out "Test")
    (let ((m (protobuf:message-read 
	      (make-read-test-message-builder)
	      (open-bytevector-input-port (bv-transcoder)))))
      (test-assert (read-test-message? m))
      (test-equal "Test" (read-test-message-foo m)))))

(test-group "32-bit"
  (let-values (((bv-out bv-transcoder) (open-bytevector-output-port)))
    (protobuf:write-varint bv-out (make-field-header 1 5))
    (protobuf:write-fixed32 bv-out 256)
    (protobuf:write-varint bv-out (make-field-header 0 2))
    (protobuf:write-string bv-out "Test")
    (let ((m (protobuf:message-read 
	      (make-read-test-message-builder)
	      (open-bytevector-input-port (bv-transcoder)))))
      (test-assert (read-test-message? m))
      (test-equal "Test" (read-test-message-foo m)))))

(test-end "unknown-fields")
(test-end "read")
(test-end "private")
