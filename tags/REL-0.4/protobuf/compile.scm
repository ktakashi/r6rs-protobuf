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

  (define default-package-name-transformer
    (protoc:naming-context-library-name protoc:default-naming-context))
  (define default-message-naming-context
    (protoc:naming-context-message-naming-context
     protoc:default-naming-context))
  (define default-enum-naming-context
    (protoc:naming-context-enum-naming-context protoc:default-naming-context))
  (define default-builder-naming-context
    (protoc:naming-context-builder-naming-context
     protoc:default-naming-context))
  (define default-extension-naming-context
    (protoc:naming-context-extension-naming-context
     protoc:default-naming-context))

  (define (singleton-definition-package-name-transformer package)
    (let ((default-name (default-package-name-transformer package)))
      (case (length (protoc:package-definitions package))
	((0) default-name)
	((1) (let ((definition (car (protoc:package-definitions package))))
	       (cond ((protoc:message-definition? definition)
		      (append default-name 
			      (list ((protoc:message-naming-context-type-name
				      default-message-naming-context) 
				     definition))))
		     ((protoc:enum-definition? definition)
		      (append default-name 
			      (list ((protoc:enum-naming-context-type-name
				      default-enum-naming-context) 
				     definition))))
	       
		     (else (raise (make-assertion-violation))))))
	(else (raise (make-assertion-violation))))))

  (define singleton-definition-naming-context
    (protoc:make-naming-context singleton-definition-package-name-transformer
				default-enum-naming-context
				default-message-naming-context 
				default-builder-naming-context
				default-extension-naming-context))

  (define (make-singleton-package package definition)
    (define new-package 
      (protoc:make-package (protoc:package-name package) 
			   (protoc:package-parent package)))

    (protoc:set-package-definitions! new-package (list definition))

    new-package)

  (define (protoc:read-proto port) 
    ((protoc:make-parser (protoc:make-tokenizer port))))

  (define (protoc:generate-libraries proto)
    (define (generate-libraries package)
      (let* ((singleton-packages 
	     (map (lambda (definition) 
		    (make-singleton-package package definition))
		  (protoc:package-definitions package)))
	     (singleton-libraries
	      (map (lambda (singleton-package) 
		     (protoc:generate-package 
		      singleton-package singleton-definition-naming-context))
		   singleton-packages)))
	(let loop ((subpackages (protoc:package-subpackages package))
		   (libraries (list)))
	  (if (null? subpackages)
	      (if (not (null? singleton-libraries))
		  (append singleton-libraries libraries)
		  libraries)
	      (let* ((p (car subpackages))
		     (l (generate-libraries p)))
		(if (null? l)
		    (loop (cdr subpackages) libraries)
		    (loop (cdr subpackages) (append l libraries))))))))
			
    (apply values (generate-libraries (protoc:proto-root-package proto))))
)
