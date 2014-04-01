;; test-codegen.scm: code generation test routines for r6rs-protobuf
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
(import (rnrs eval))
(import (srfi :64))
(import (protobuf compile codegen)
	(protobuf compile parse)
	(protobuf private))

(test-begin "codegen")

(test-begin "message")

(test-group "embedded"
  (let* ((package (protoc:make-package "com.google.protobuf.test" #f))
	 (message-definition-1
	  (protoc:make-message-definition "TestMessage1" package))
	 (message-definition-2
	  (protoc:make-message-definition "TestMessage2" package))

	 (message-field-1
	  (protoc:make-field-definition
	   message-definition-1 'required 
	   (protoc:make-type-reference 
	    "TestMessage2" 
	    (protobuf:make-message-field-type-descriptor
	     "com.google.protobuf.test.TestMessage2" 'length-delimited #f #f #f
	     #f message-definition-2))
	   "msg" 1))
	 (message-field-2
	  (protoc:make-field-definition
	   message-definition-1 'required 
	   (protoc:make-type-reference "int32" protobuf:field-type-int32) 
	   "num" 2))

	 (message-field-3
	  (protoc:make-field-definition
	   message-definition-2 'required 
	   (protoc:make-type-reference "string" protobuf:field-type-string) 
	   "text" 1))

	 (test-env (environment '(rnrs) '(protobuf private))))

    (protoc:set-message-definition-fields! 
     message-definition-1 (list message-field-1 message-field-2))
    (protoc:set-message-definition-fields! 
     message-definition-2 (list message-field-3))

    (for-each (lambda (exp) (eval exp test-env)) 
	      (append
	       (protoc:generate-message
		message-definition-1 protoc:default-naming-context)
	       (protoc:generate-builder
		message-definition-1 protoc:default-naming-context)
	       (protoc:generate-message
		message-definition-2 protoc:default-naming-context)
	       (protoc:generate-builder
		message-definition-2 protoc:default-naming-context)))

    (let ((mm1 
	   (eval '(let ((b2 (make-TestMessage2-builder))
			(b1 (make-TestMessage1-builder)))
		    (set-TestMessage1-builder-num! b1 123)
		    (set-TestMessage2-builder-text! b2 "Hello, 2!")
		    (set-TestMessage1-builder-msg! 
		     b1 (TestMessage2-builder-build b2))
		    (let ((m1 (TestMessage1-builder-build b1)))
		      (let-values 
			  (((port proc) (open-bytevector-output-port)))
			(TestMessage1-write m1 port)
			(TestMessage1-read 
			 (open-bytevector-input-port (proc))))))
		 test-env)))

      (test-assert 
       (eqv? (eval `(TestMessage1-num ,mm1) test-env) 123))
      (test-assert 
       (equal? (eval `(TestMessage2-text (TestMessage1-msg ,mm1)) test-env) 
	       "Hello, 2!")))))

(test-end "message")

(test-begin "enum")

(test-group "simple"
  (let* ((package (protoc:make-package "com.google.protobuf.test" #f))
	 (enum-definition (protoc:make-enum-definition "test-enum" package)))
    (protoc:set-enum-definition-values! 
     enum-definition (list (protoc:make-enum-value-definition "ONE" 0)
			   (protoc:make-enum-value-definition "TWO" 1)
			   (protoc:make-enum-value-definition "THREE" 2)))

    (let ((expressions (protoc:generate-enum 
			enum-definition
			(protoc:naming-context-enum-naming-context 
			 protoc:default-naming-context)))
	  (test-env (environment '(rnrs))))
      (for-each (lambda (exp) (eval exp test-env)) expressions)
      (test-assert (eval '(test-enum test-enum-ONE) test-env) 'test-enum-ONE)
      (test-assert (eval '(test-enum test-enum-TWO) test-env) 'test-enum-TWO)
      (test-assert (eval '(test-enum test-enum-THREE) test-env) 
		   'test-enum-THREE))))

(test-end "enum")
(test-end "codegen")
