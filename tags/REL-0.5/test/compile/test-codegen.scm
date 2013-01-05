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
	(protobuf compile parse))

(test-begin "codegen")
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
