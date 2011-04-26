;; compile: r6rs-protobuf compiler front-end
;; Copyright (C) 2011 Julian Graham

;; SCSS is free software: you can redistribute it and/or modify
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

(library (protobuf compile)
  (export protoc:read-proto 
	  protoc:generate-libraries)
  (import (rnrs)
	  (protobuf compile codegen)
	  (protobuf compile parse)
	  (protobuf compile tokenize))

  (define (protoc:read-proto port) 
    ((protoc:make-parser (protoc:make-tokenizer port))))

  (define (protoc:generate-libraries proto)
    (define (generate-libraries package)
      (let ((library (and (not (null? (protoc:package-definitions package)))
			  (protoc:generate-package 
			   package protoc:default-naming-context))))
	(let loop ((subpackages (protoc:package-subpackages package))
		   (libraries (list)))
	  (if (null? subpackages)
	      (if library (cons library libraries) libraries)
	      (let* ((p (car subpackages))
		     (l (generate-libraries p)))
		(if (null? l)
		    (loop (cdr subpackages) libraries)
		    (loop (cdr subpackages) (append l libraries))))))))
			
    (apply values (generate-libraries (protoc:proto-root-package proto))))
)
