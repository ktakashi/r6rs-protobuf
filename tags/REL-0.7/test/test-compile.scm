;; test-compile.scm: compiler test routines for r6rs-protobuf
;; Copyright (C) 2013 Julian Graham

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
(import (protobuf compile))
(import (srfi :64))

(test-begin "compile")
(test-group "full-serialization"
  (let* ((proto (string-append
		 "message Test {\n"
		 "  required string str = 1;\n"
		 "  required int32 i = 2;\n"
		 "  required double f = 3;\n"
		 "}")))
    (let-values (((library)
		  (protoc:generate-libraries 
		   (protoc:read-proto (open-input-string proto)))))
      (let ((env (environment '(rnrs) '(protobuf private))))
	(eval (cons 'begin (cddddr library)) env)
	(let-values 
	    (((str i f)
	      (eval '(let ((b (make-Test-builder)))
		       (set-Test-builder-str! b "foo")
		       (set-Test-builder-i! b 123)
		       (set-Test-builder-f! b 0.321)
		       (let* ((bv (call-with-values
				      (lambda () (open-bytevector-output-port))
				    (lambda (port get-bytevector)
				      (Test-write (Test-builder-build b) port)
				      (get-bytevector))))
			      (p (Test-read (open-bytevector-input-port bv))))
			 (values (Test-str p) (Test-i p) (Test-f p))))
		    env)))
	  (test-equal "foo" str)
	  (test-eqv 123 i)
	  (test-eqv 0.321 f))))))

(test-end "compile")
