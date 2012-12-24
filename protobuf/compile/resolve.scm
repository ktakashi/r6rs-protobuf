;; resolve.scm: Validation and type resolution routines for r6rs-protobuf
;; Copyright (C) 2012 Julian Graham

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

(library (protobuf compile resolve)
  (export protoc:resolve)
  (import (rnrs)
	  (srfi :13)
	  (srfi :14)
	  (protobuf compile conditions)
	  (protobuf compile parse)
	  (protobuf compile tokenize)
	  (protobuf private))

  (define (string-split str chr)
    (string-tokenize str (char-set-complement (char-set chr))))

  (define-record-type (protoc:type-resolution-context
		       protoc:make-type-resolution-context
		       protoc:type-resolution-context?)
    (fields root-package
	    basedir
	    package-index
	    unresolved-references
	    unresolved-extensions)
    (protocol (lambda (p)
		(lambda (root-package basedir)
		  (p root-package
		     basedir
		     (make-hashtable string-hash equal?)
		     (make-hashtable string-hash equal?)
		     (make-hashtable string-hash equal?))))))
  
  (define (clone-package package)
    (define (clone-package-inner package new-parent)
      (let ((new-package 
	     (protoc:make-package (protoc:package-name package) new-parent)))
	(protoc:set-package-definitions!
	 new-package (protoc:package-definitions package))
	(protoc:set-package-options!
	 new-package (protoc:package-options package))
	(protoc:set-package-subpackages!
	 new-package (map (lambda (subpackage)
			    (clone-package-inner subpackage new-package))
			  (protoc:package-subpackages package)))
	new-package))
    (clone-package-inner package #f))

  (define (indexed-package context package-name)
    (if package-name
	(hashtable-ref
	 (protoc:type-resolution-context-package-index context) 
	 package-name #f)
	(protoc:type-resolution-context-root-package context)))

  (define (merge-package to from)
    (protoc:set-package-definitions! 
     to (append (protoc:package-definitions to) 
		(protoc:package-definitions from)))

    (for-each (lambda (s)
		(let ((ts (find (lambda (x)
				  (equal? (protoc:package-name x)
					  (protoc:package-name s)))
				(protoc:package-subpackages to))))
		  (if ts
		      (merge-package ts s)
		      (protoc:set-package-subpackages! 
		       to (cons s (protoc:package-subpackages to))))))
	      (protoc:package-subpackages from)))

    (define (definition->descriptor definition)
      (define (message-definition->descriptor definition)
	(protobuf:make-message-field-type-descriptor 
	 (protoc:message-definition-name definition) 
	 'length-delimited #f #f #f #f definition))
      
      (define (enum-definition->descriptor definition)
	(protobuf:make-enum-field-type-descriptor
	 (protoc:enum-definition-name definition) 
	 'varint #f #f #f #f definition))
      
      (cond ((protoc:message-definition? definition)
	     (message-definition->descriptor definition))
	    ((protoc:enum-definition? definition)
	     (enum-definition->descriptor definition))
	    (else raise (make-assertion-violation))))

  (define (resolve-type type-reference context)
    (define (resolve-type-relative name context)
      (define (resolve-type-relative-inner components context)
	(let* ((first-component (car components))
	       (definitions 
		 (cond ((protoc:package? context)
			(protoc:package-definitions context))
		       ((protoc:message-definition? context)
			(protoc:message-definition-definitions context))
		       ((protoc:extension-definition? context) '())
		       (else (raise (make-assertion-violation))))))
	  (let loop ((definitions definitions))
	    (and (not (null? definitions))
		 (let ((definition (car definitions)))
		   (cond ((protoc:message-definition? definition)
			  (if (equal? first-component 
				      (protoc:message-definition-name 
				       definition))
			      (if (= (length components) 1)
				  definition
				  (resolve-type-relative-inner 
				   (cdr components) definition))
			      (loop (cdr definitions))))
			 ((protoc:enum-definition? definition)
			  (if (equal? first-component 
				      (protoc:enum-definition-name 
				       definition))
			      definition
			      (loop (cdr definitions))))
			 (else (loop (cdr definitions)))))))))
      (resolve-type-relative-inner (string-split name #\.) context))
    
    (define (resolve-type-upwards name location)
      (let ((definition (resolve-type-relative name location)))
	(or definition
	    (cond ((protoc:message-definition? location)
		   (cond ((protoc:message-definition-parent location)
			  (resolve-type-upwards
			   name (protoc:message-definition-parent location)))
			 ((protoc:message-definition-package location)
			  (resolve-type-upwards 
			   name (indexed-package
				 context
				 (protoc:package-name
				  (protoc:message-definition-package 
				   location)))))
			 (else #f)))
		  ((protoc:extension-definition? location)
		   (cond ((protoc:extension-definition-parent location)
			  (resolve-type-upwards
			   name (protoc:extension-definition-parent location)))
			 ((protoc:extension-definition-package location)
			  (resolve-type-upwards
			   name (indexed-package
				 context
				 (protoc:package-name
				  (protoc:extension-definition-package 
				   location)))))
			 (else #f)))
		  ((protoc:package? location)
		   (and (protoc:package-parent location)
			(resolve-type-upwards
			 name (indexed-package
			       context
			       (protoc:package-name
				(protoc:package-parent location))))))
		  (else (raise (make-assertion-violation)))))))
    
    (define (resolve-type-downwards name package)
      (define (strip-package-prefix name package-name)
	(and (string-prefix? (string-append package-name ".") name)
	     (substring name (+ (string-length package-name) 1))))
      
      (let* ((package-name (protoc:package-name package))
	     (package-relative-name
	      (if package-name (strip-package-prefix name package-name) name))
	     (definition
	       (resolve-type-relative package-relative-name package)))
	(or definition
	    (let ((components (string-split name #\.)))
	      (and (> (length components) 1)
		   (let loop ((subpackages 
			       (protoc:package-subpackages package)))
		     (and (not (null? subpackages))
			  (let* ((subpackage (car subpackages))
				 (subname (strip-package-prefix 
					   name (protoc:package-name 
						 subpackage))))
			    (or (and subname
				     (resolve-type-downwards 
				      name subpackage))
				(loop (cdr subpackages)))))))))))

    (let* ((location (protoc:type-reference-location type-reference))
	   (location (cond ((protoc:extension-definition? location) location)
			   ((protoc:field-definition? location)
			    (protoc:field-definition-parent location))
			   (else (raise (make-assertion-violation)))))
	   (package (cond ((protoc:extension-definition? location)
			   (protoc:extension-definition-package location))
			  ((protoc:message-definition? location)
			   (protoc:message-definition-package location))
			  (else #f)))
	   (name (protoc:type-reference-name type-reference))
	   (definition (or (resolve-type-upwards name location)
			   (resolve-type-downwards 
			    name (protoc:type-resolution-context-root-package 
				  context))))
	   (descriptor (and definition (definition->descriptor definition))))
      
      (if descriptor
	  (protoc:set-type-reference-descriptor! type-reference descriptor)
	  (raise (condition 
		  (make-assertion-violation)
		  (make-message-condition
		   (string-append "Reference to unknown type " name)))))))
  
  (define (resolve-extension extension-def)
    (define (valid-extension? extension-field message-def)
      (define idx (protoc:field-definition-ordinal extension-field))
      (define (extension-field-within-range? extension-range)
	(and (>= idx (protoc:extension-range-definition-from extension-range))
	     (<= idx (protoc:extension-range-definition-to extension-range))))
      (find extension-field-within-range? 
	    (protoc:message-definition-extension-ranges message-def)))
    
    (let* ((type (protoc:extension-definition-target extension-def))
	   (descriptor (protoc:type-reference-descriptor type)))
      (if (not (protobuf:message-field-type-descriptor? descriptor))
	  (raise (condition
		  (make-assertion-violation)
		  (make-message-condition
		   (string-append "Cannot extend non-message type "
				  (protobuf:field-type-descriptor-name 
				   descriptor))))))
      (let ((m (protobuf:message-field-type-descriptor-definition 
		descriptor)))
	(for-each 
	 (lambda (extension-field)
	   (or (valid-extension? extension-field m)
	       (raise 
		(condition
		 (make-assertion-violation)
		 (make-message-condition
		  (string-append "Invalid extension index "
				 (number->string 
				  (protoc:field-definition-ordinal 
				   extension-field))
				 " for message "
				 (protoc:message-definition-name m)))))))
	 (protoc:extension-definition-fields extension-def)))))

  (define (resolve-type-reference reference context)
    (or (protoc:type-reference-descriptor reference)
	(begin
	  (resolve-type reference context)
	  (protoc:type-reference-descriptor reference))))

  (define (normalize-package-name package)
    (or (protoc:package-name package) "protobuf.default"))
  
  (define (resolve proto context)
    (define (resolve-package package)
      (define (resolve-field-definition definition)
	(resolve-type-reference
	 (protoc:field-definition-type definition) context))
      
      (define (resolve-enum-definition definition) (if #f #f))
      
      (define (resolve-message-definition definition)
	(for-each resolve-field-definition
		  (protoc:message-definition-fields definition)))
      
      (define (resolve-extension-definition definition)
	(raise (make-assertion-violation)))
      
      (define (resolve-definition definition)
	(cond ((protoc:enum-definition? definition)
	       (resolve-enum-definition definition))
	      ((protoc:message-definition? definition)
	       (resolve-message-definition definition))
	      ((protoc:extension-definition? definition)
	       (resolve-extension-definition definition))
	      (else (raise 
		     (condition 
		      (make-assertion-violation)
		      (make-message-condition "Unknown definition type")
		      (protoc:make-type-resolution-condition))))))
      
      (for-each resolve-definition (protoc:package-definitions package))
      (for-each resolve-package (protoc:package-subpackages package)))
    
    (define (absolute? path) (eqv? (string-ref path 0) #\x002f))
    
    (define (resolve-import-path import)
      (if (or (absolute? import) 
	      (not (protoc:type-resolution-context-basedir context)))
	  import
	  (string-append 
	   (protoc:type-resolution-context-basedir context) "/" import)))

    (define (index-package package)
      (define index (protoc:type-resolution-context-package-index context))
      (define (index-package-inner package)
	(if (protoc:package-name package)
	    (hashtable-set! index (protoc:package-name package) package))
	(for-each index-package-inner (protoc:package-subpackages package)))
      (index-package-inner package))

    (map (lambda (import)
	   (let* ((filename (resolve-import-path import))
		  (other-proto ((protoc:make-parser
				 (protoc:make-tokenizer 
				  (open-input-file filename)))))
		  (resolution-basedir
		   (and (absolute? filename)
			(substring 
			 filename 0 (string-index-right filename #\x002f))))
		  (other-context (protoc:make-type-resolution-context
				  (clone-package
				   (protoc:proto-root-package other-proto))
				  resolution-basedir)))
	     
	     (resolve other-proto other-context)
	     (merge-package 
	      (protoc:type-resolution-context-root-package context)
	      (protoc:proto-root-package other-proto))))

	 (protoc:proto-imports proto))

    (index-package (protoc:type-resolution-context-root-package context))
    (resolve-package (protoc:proto-root-package proto)))
    
  (define (scan proto context)
    (define (register-unresolved-type-reference type-reference)
      (if (not (protoc:type-reference-descriptor type-reference))
	  (hashtable-update! 
	   (protoc:type-resolution-context-unresolved-references context)
	   (protoc:type-reference-name type-reference)
	   (lambda (refs) (cons type-reference refs))
	   (list type-reference))))
    
    (define (scan-definition definition)
      (define (scan-field-definition definition)
	(register-unresolved-type-reference
	 (protoc:field-definition-type definition)))

      (cond ((protoc:message-definition? definition)
	     (for-each scan-field-definition
		       (protoc:message-definition-fields definition)))
	    ((protoc:extension-definition? definition)
	     (register-unresolved-type-reference 
	      (protoc:extension-definition-target definition)))))

    (define (scan-package package)
      (for-each scan-definition (protoc:package-definitions package))
      (for-each scan-package (protoc:package-subpackages package)))
    (scan-package (protoc:proto-root-package proto)))

  (define (protoc:resolve proto . rest)
    (define resolution-basedir (and (not (null? rest)) (car rest)))
    (define resolution-context 
      (protoc:make-type-resolution-context 
       (clone-package (protoc:proto-root-package proto)) 
       resolution-basedir))
    
    (scan proto resolution-context)
    (resolve proto resolution-context))
)
