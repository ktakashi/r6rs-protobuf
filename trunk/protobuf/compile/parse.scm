;; parse.scm: .proto format parsing routines for r6rs-protobuf
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

(library (protobuf compile parse)
  (export protoc:make-parser
	  
	  protoc:proto?
	  protoc:make-proto
	  protoc:proto-root-package
	  protoc:proto-options

	  protoc:make-option-declaration
	  protoc:option-declaration?
	  protoc:option-declaration-name
	  protoc:option-declaration-value

	  protoc:make-package
	  protoc:package
	  protoc:package?
	  protoc:package-name
	  protoc:package-definitions
	  protoc:package-options
	  protoc:package-required-packages
	  protoc:package-subpackages
	  protoc:set-package-definitions!
	  protoc:set-package-subpackages!
	  
	  protoc:make-message-definition
	  protoc:message-definition?
	  protoc:message-definition-name
	  protoc:message-definition-definitions
	  protoc:message-definition-extension-ranges
	  protoc:message-definition-fields
	  protoc:message-definition-options
	  protoc:message-definition-parent
	  protoc:set-message-definition-extension-ranges!

	  protoc:make-extension-range-definition
	  protoc:extension-range-definition?
	  protoc:extension-range-definition-from
	  protoc:extension-range-definition-to

	  protoc:make-extension-definition
	  protoc:extension-definition?
	  protoc:extension-definition-fields
	  protoc:extension-definition-parent
	  protoc:extension-definition-target

	  protoc:make-enum-definition
	  protoc:enum-definition?
	  protoc:enum-definition-name
	  protoc:enum-definition-values
	  protoc:enum-definition-options
	  protoc:enum-definition-parent
	  
	  protoc:make-enum-value-definition
	  protoc:enum-value-definition?
	  protoc:enum-value-definition-name
	  protoc:enum-value-definition-ordinal
	  
	  protoc:make-type-reference
	  protoc:type-reference?
	  protoc:type-reference-name
	  protoc:type-reference-descriptor
	  protoc:type-reference-location
	  protoc:set-type-reference-location!

	  protoc:make-field-definition
	  protoc:field-definition?
	  protoc:field-definition-rule
	  protoc:field-definition-type
	  protoc:field-definition-name
	  protoc:field-definition-ordinal
	  protoc:field-definition-options)
  (import (rnrs)
 	  (protobuf private)
	  (protobuf compile tokenize)
	  (srfi :13)
	  (srfi :14))

  (define (string-split str chr)
    (string-tokenize str (char-set-complement (char-set chr))))

  (define-record-type (protoc:proto protoc:make-proto protoc:proto?)
    (fields root-package
	    (mutable imports
		     protoc:proto-imports
		     protoc:set-proto-imports!)
	    (mutable options
		     protoc:proto-options
		     protoc:set-proto-options!))
    (protocol 
     (lambda (p) 
       (lambda (root-package . rest)
	 (case (length rest)
	   ((0) (p root-package '() '()))
	   ((1) (p root-package (car rest) '()))
	   ((2) (p root-package (car rest) (cadr rest)))
	   (else (raise (make-assertion-violation))))))))

  (define-record-type (protoc:package protoc:make-package protoc:package?)
    (fields name 
	    parent
	    (mutable required-packages
		     protoc:package-required-packages
		     protoc:set-package-required-packages!)
	    (mutable definitions 
		     protoc:package-definitions 
		     protoc:set-package-definitions!)
	    (mutable subpackages
		     protoc:package-subpackages
		     protoc:set-package-subpackages!)
	    (mutable options 
		     protoc:package-options 
		     protoc:set-package-options!))
    (protocol 
     (lambda (p)
       (lambda (name parent . rest)
	 (case (length rest)
	   ((0) (p name parent '() '() '() '()))
	   ((1) (p name parent (car rest) '() '() '()))
	   ((2) (p name parent (car rest) (cadr rest) '() '()))
	   ((3) (p name parent (car rest) (cadr rest) (caddr rest) '()))
	   ((4) (apply p (cons name (cons parent rest))))
	   (else (raise (make-assertion-violation))))))))

  (define-record-type (protoc:extension-range-definition
		       protoc:make-extension-range-definition
		       protoc:extension-range-definition?)
    (fields from to))

  (define-record-type (protoc:extension-definition
		       protoc:make-extension-definition
		       protoc:extension-definition?)
    (fields target 
	    parent
	    package
	    (mutable fields 
		     protoc:extension-definition-fields
		     protoc:set-extension-definition-fields!))
    (protocol
     (lambda (p)
       (lambda (target parent . rest)
	 (p target parent (and (not (null? rest)) (car rest)) '())))))

  (define-record-type (protoc:option-declaration
		       protoc:make-option-declaration
		       protoc:option-declaration?)
    (fields name value))
  
  (define-record-type (protoc:message-definition
		       protoc:make-message-definition
		       protoc:message-definition?)
    (fields name
	    parent
	    package
	    (mutable extension-ranges
		     protoc:message-definition-extension-ranges
		     protoc:set-message-definition-extension-ranges!)
	    (mutable options 
		     protoc:message-definition-options
		     protoc:set-message-definition-options!)
	    (mutable fields 
		     protoc:message-definition-fields
		     protoc:set-message-definition-fields!)
	    (mutable definitions 
		     protoc:message-definition-definitions
		     protoc:set-message-definition-definitions!))
    (protocol 
     (lambda (p) 
       (lambda (name package . parent)
	 (p name (and (not (null? parent)) (car parent)) 
	    package  '() '() '() '())))))

  (define-record-type (protoc:type-reference
		       protoc:make-type-reference
		       protoc:type-reference?)
    (fields name
	    (mutable descriptor
		     protoc:type-reference-descriptor
		     protoc:set-type-reference-descriptor!)
	    (mutable location
		     protoc:type-reference-location
		     protoc:set-type-reference-location!))
    (protocol
     (lambda (p)
       (lambda (name . descriptor)
	 (p name (and (not (null? descriptor)) (car descriptor)) #f)))))

  (define-record-type (protoc:field-definition
		       protoc:make-field-definition
		       protoc:field-definition?)
    (fields parent
	    rule
	    type
	    name
	    ordinal
	    (mutable options
		     protoc:field-definition-options
		     protoc:set-field-definition-options!))
    (protocol (lambda (p) 
		(lambda (message rule type type-expr name ordinal . options) 
		  (p message rule type type-expr name ordinal 
		     (if (null? options) options (car options)))))))
 
  (define-record-type (protoc:enum-value-definition
		       protoc:make-enum-value-definition
		       protoc:enum-value-definition?)
    (fields name ordinal))

  (define-record-type (protoc:enum-definition
		       protoc:make-enum-definition
		       protoc:enum-definition?)
    (fields name 
	    parent
	    package
	    (mutable options 
		     protoc:enum-definition-options
		     protoc:set-enum-definition-options!)
	    (mutable values 
		     protoc:enum-definition-values 
		     protoc:set-enum-definition-values!))
    (protocol 
     (lambda (p) 
       (lambda (name package . parent) 
	 (p name (if (null? parent) #f (car parent)) package  '() '() '())))))
  
  (define (merge-package! scope package) #f)

  (define (protoc:make-parser lexer)
    (define unresolved-type-references (list))
    (define unresolved-extensions (list))

    (define resolved-type-descriptors (make-hashtable string-hash equal?))

    (define external-packages (make-hashtable string-hash equal?))
    (define internal-packages (make-hashtable string-hash equal?))
    (define root-package (protoc:make-package #f #f))

    (define proto (protoc:make-proto root-package))
    (define current-package root-package)

    (define current-token #f)
    (define current-category #f)
    (define current-value #f)
    
    (define token-stack (list))

    (define (unexpected-token-error)
      (raise (condition 
	      (make-assertion-violation)
	      (make-message-condition 
	       (string-append "Unexpected token: " 
			      (symbol->string current-category))))))

    (define (get-token) 
      (define (set-data token)
	(set! current-token token)
	(if (eq? token '*eoi*)
	    (begin
	      (set! current-category '*eoi*)
	      (set! current-value '*eoi*))
	    (begin
	      (set! current-category 
		    (protoc:lexical-token-category current-token))
	      (set! current-value 
		    (protoc:lexical-token-value current-token)))))
      (if (null? token-stack) 
	  (set-data (lexer))
	  (begin (set-data (car token-stack))
		 (set! token-stack (cdr token-stack)))))
    
    (define (unget-token token)
      (set! current-token #f)
      (set! current-category #f)
      (set! current-value #f)
      
      (set! token-stack (cons token token-stack)))

    (define (assert-next-category category)
      (get-token)
      (if (not (eq? current-category category))
	  (unexpected-token-error)))

    (define (resolve-type type-reference)
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
	  
      (define (resolve-type-upwards name context)
	(let ((definition (resolve-type-relative name context)))
	  (or definition
	      (cond ((protoc:message-definition? context)
		     (cond ((protoc:message-definition-parent context)
			    (resolve-type-upwards
			     name (protoc:message-definition-parent context)))
			   ((protoc:message-definition-package context)
			    (resolve-type-upwards 
			     name (protoc:message-definition-package context)))
			   (else #f)))
		    ((protoc:extension-definition? context)
		     (cond ((protoc:extension-definition-parent context)
			    (resolve-type-upwards
			     name (protoc:extension-definition-parent context)))
			   ((protoc:extension-definition-package context)
			    (resolve-type-upwards
			     name (protoc:extension-definition-package 
				   context)))
			   (else #f)))
		    ((protoc:package? context)
		     (and (protoc:package-parent context)
			  (resolve-type-upwards
			   name (protoc:package-parent context))))
		    (else (raise (make-assertion-violation)))))))
	
      (define (resolve-type-downwards name package)
	(define (strip-package-prefix name package-name)
	  (and (string-prefix? (string-append package-name ".") name)
	       (substring name (+ (string-length package-name) 1))))

	(let ((definition (resolve-type-relative name package)))
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
					subname subpackage))
				  (loop (cdr subpackages)))))))))))
 
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
	     (descriptor (hashtable-ref resolved-type-descriptors name #f))
	     (definition (and (not descriptor)
			      (or (resolve-type-upwards name location)
				  (resolve-type-downwards name root-package))))
	     (descriptor
	      (or descriptor
		  (and definition (definition->descriptor definition)))))

	(if descriptor
	    (begin
	      (if (not (hashtable-contains? resolved-type-descriptors name))
		  (hashtable-set! resolved-type-descriptors name descriptor))
	      (let ((definition-package
		      (cond ((protoc:message-definition? definition)
			     (protoc:message-definition-package definition))
			    ((protoc:enum-definition? definition)
			     (protoc:enum-definition-package definition))
			    (else (raise (make-assertion-violation)))))
		    (required-packages (protoc:package-required-packages 
					package)))
		(if (and (not (equal? (protoc:package-name definition-package)
				      (protoc:package-name package)))
			 (not (memp (lambda (p) 
				      (equal? (protoc:package-name p)
					      (protoc:package-name 
					       definition-package)))
				    required-packages)))
		    (protoc:set-package-required-packages! 
		     package (cons definition-package required-packages))))
	      (protoc:set-type-reference-descriptor! type-reference descriptor))
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

    (define (parse-type)
      (get-token)
      (case current-category
	((DOUBLE) 
	 (protoc:make-type-reference "double" protobuf:field-type-double))
	((FLOAT) (protoc:make-type-reference "float" protobuf:field-type-float))
	((INT32) (protoc:make-type-reference "int32" protobuf:field-type-int32))
	((INT64) (protoc:make-type-reference "int64" protobuf:field-type-int64))
	((UINT32) 
	 (protoc:make-type-reference"uint32" protobuf:field-type-uint32))
	((UINT64) 
	 (protoc:make-type-reference "uint64" protobuf:field-type-uint64))
	((SINT32) 
	 (protoc:make-type-reference "sint32" protobuf:field-type-sint32))
	((SINT64) 
	 (protoc:make-type-reference "sint64" protobuf:field-type-sint64))
	((FIXED32) 
	 (protoc:make-type-reference "fixed32" protobuf:field-type-fixed32))
	((FIXED64) 
	 (protoc:make-type-reference "fixed64" protobuf:field-type-fixed64))
	((SFIXED32) 
	 (protoc:make-type-reference "sfixed32" protobuf:field-type-sfixed32))
	((SFIXED64) 
	 (protoc:make-type-reference "sfixed64" protobuf:field-type-sfixed64))
	((BOOL) (protoc:make-type-reference "bool" protobuf:field-type-bool))
	((STRING) 
	 (protoc:make-type-reference "string" protobuf:field-type-string))
	((BYTES) (protoc:make-type-reference "bytes" protobuf:field-type-bytes))

	((IDENTIFIER)
	 (let loop ((name ""))
	   (let ((val current-value))
	     (get-token)
	     (if (eq? current-category 'DOT)
		 (begin (get-token) (loop (string-append name val ".")))
		 (begin (unget-token current-token)
			(protoc:make-type-reference
			 (string-append name val)))))))
	(else (unexpected-token-error))))

    (define (parse-package)
      (define (parse-package-element parent pkg-name)	
	(assert-next-category 'IDENTIFIER)
	(let* ((pkg-name (string-append pkg-name current-value))
	       (package (hashtable-ref internal-packages pkg-name #f))
	       (package
		(or package 
		    (let ((p (protoc:make-package pkg-name parent)))
		      (hashtable-set! internal-packages pkg-name p)
		      (protoc:set-package-subpackages!
		       parent (cons p (protoc:package-subpackages parent)))
		      p))))

	  (set! current-package package)

	  (get-token)
	  (case current-category
	    ((DOT) (parse-package-element package (string-append pkg-name ".")))
	    ((SEMICOLON) package)
	    (else (unexpected-token-error)))))

      (parse-package-element root-package ""))

    (define (parse-import)
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
      
      (assert-next-category 'STRING-LITERAL)
      (let* ((lexer (protoc:make-tokenizer 
		     (open-input-file current-value)))
	     (parser (protoc:make-parser lexer))
	     (proto (parser)))
	
	(merge-package root-package (protoc:proto-root-package proto)))
      (assert-next-category 'SEMICOLON))

    (define (parse-enum parent)
      (define (parse-enum-elements enum)
	(define (parse-enum-field field-name)
	  (assert-next-category 'EQUAL)
	  (assert-next-category 'NUM-INTEGER)
	  (let ((value (protoc:make-enum-value-definition 
			field-name current-value)))
	    (assert-next-category 'SEMICOLON)
	    value))

	(get-token)
	(case current-category
	  ((IDENTIFIER) 
	   (protoc:set-enum-definition-values! 
	    enum (cons (parse-enum-field current-value)
		       (protoc:enum-definition-values enum)))
	   (parse-enum-elements enum))
	  ((OPTION) (parse-enum-elements enum))
	  ((RBRACE) enum)
	  (else (unexpected-token-error))))

      (assert-next-category 'IDENTIFIER)
      (let ((enum (protoc:make-enum-definition 
		   current-value current-package parent)))
	(assert-next-category 'LBRACE)
	(parse-enum-elements enum)))

    (define (parse-field parent rule)
      (define (parse-maybe-field-options) #f)
      
      (let ((type (parse-type)))
	(assert-next-category 'IDENTIFIER)
	(let ((field-name current-value))
	  (assert-next-category 'EQUAL)
	  (assert-next-category 'NUM-INTEGER)
	  (let* ((index current-value)
		 (options (parse-maybe-field-options))
		 (fd (protoc:make-field-definition 
		      parent rule type field-name index options)))
	    
	    (if (not (protoc:type-reference-descriptor type))
		(set! unresolved-type-references
		      (cons type unresolved-type-references)))
	    
	    (assert-next-category 'SEMICOLON)
	    (protoc:set-type-reference-location! type fd)

	    fd))))

    (define (parse-message parent)
      (define (parse-message-element message-def)
	(define (parse-extension-ranges)
	  (define (parse-extension-range-element exts)
	    (assert-next-category 'NUM-INTEGER)
	    (let ((from current-value))
	      (get-token)
	      (case current-category
		((COMMA) 
		 (parse-extension-range-element 
		  (cons (protoc:make-extension-range-definition from from)
			exts)))
		((SEMICOLON)
		 (reverse 
		  (cons (protoc:make-extension-range-definition from from) 
			exts)))
		((TO)
		 (get-token)
		 (let* ((to (case current-category
			      ((NUM-INTEGER) current-value)
			      ((MAX) 536870911)
			      (else (unexpected-token-error))))
			(ext (protoc:make-extension-range-definition from to)))
		   (get-token)
		   (case current-category
		     ((COMMA) (parse-extension-range-element (cons ext exts)))
		     ((SEMICOLON) (reverse (cons ext exts)))
		     (else (unexpected-token-error)))))
		(else (unexpected-token-error)))))
	  (parse-extension-range-element (list)))
	
	(get-token)
	(case current-category
	  ((ENUM) 
	   (protoc:set-message-definition-definitions!
	    message-def 
	    (cons (parse-enum message-def) 
		  (protoc:message-definition-definitions message-def)))
	   (parse-message-element message-def))
	  ((EXTEND)
	   (protoc:set-message-definition-definitions!
	    message-def
	    (cons (parse-extension message-def)
		  (protoc:message-definition-definitions message-def)))
	   (parse-message-element message-def))
	  ((EXTENSIONS)
	   (protoc:set-message-definition-extension-ranges! 
	    message-def (parse-extension-ranges))
	   (parse-message-element message-def))
	  ((MESSAGE)
	   (protoc:set-message-definition-definitions!
	    message-def
	    (cons (parse-message message-def) 
		  (protoc:message-definition-definitions message-def)))
	   (parse-message-element message-def))
	  ((OPTIONAL)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field message-def 'optional) 
			      (protoc:message-definition-fields message-def)))	
	   (parse-message-element message-def))
	  ((RBRACE) message-def)
	  ((REPEATED)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field message-def 'repeated) 
			      (protoc:message-definition-fields message-def)))	
	   (parse-message-element message-def))
	  ((REQUIRED)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field message-def 'required) 
			      (protoc:message-definition-fields message-def)))
	   (parse-message-element message-def))
	  (else (unexpected-token-error))))

      (assert-next-category 'IDENTIFIER)
      (let ((name current-value))
	(assert-next-category 'LBRACE)
	(let ((md (protoc:make-message-definition name current-package parent)))
	  (parse-message-element md))))
    
    (define (parse-extension parent)
      (define (parse-extension-element extension-def)
	(get-token)
	(case current-category
	  ((OPTIONAL)
	   (protoc:set-extension-definition-fields!
	    extension-def (cons (parse-field extension-def 'optional)
				(protoc:extension-definition-fields 
				 extension-def)))
	   (parse-extension-element extension-def))
	  ((REPEATED)
	   (protoc:set-extension-definition-fields!
	    extension-def (cons (parse-field extension-def 'repeated)
				(protoc:extension-definition-fields 
				 extension-def)))
	   (parse-extension-element extension-def))
	  ((RBRACE) extension-def)
	  ((REQUIRED)
	   (protoc:set-extension-definition-fields!
	    extension-def (cons (parse-field extension-def 'required)
				(protoc:extension-definition-fields 
				 extension-def)))
	   (parse-extension-element extension-def))
	  (else (unexpected-token-error))))

      (let* ((type (parse-type))
	     (extension-def (protoc:make-extension-definition 
			     type parent current-package)))

	(set! unresolved-type-references (cons type unresolved-type-references))
	(set! unresolved-extensions (cons extension-def unresolved-extensions))

	(protoc:set-type-reference-location! type extension-def)
	(assert-next-category 'LBRACE)
	(parse-extension-element extension-def)))

    (define (parse-proto)
      (define (parse-proto-elements)
	(get-token)
	(case current-category
	  ((ENUM) 
	   (let ((enum (parse-enum #f)))
	     (protoc:set-package-definitions!
	      current-package (cons enum (protoc:package-definitions
					  current-package))))
	   (parse-proto-elements))
	  ((EXTEND)
	   (let ((extension (parse-extension #f)))
	     (protoc:set-package-definitions!
	      current-package (cons extension (protoc:package-definitions
					       current-package))))
	   (parse-proto-elements))
	  ((IMPORT) (parse-import) (parse-proto-elements))
	  ((MESSAGE)
	   (let ((message (parse-message #f)))
	     (protoc:set-package-definitions!
	      current-package (cons message (protoc:package-definitions 
					     current-package))))
	   (parse-proto-elements))
	  ((PACKAGE) (parse-package) (parse-proto-elements))
	  ((*eoi*) proto)
	  (else (unexpected-token-error))))
      
      (parse-proto-elements)
      (for-each resolve-type unresolved-type-references)
      (for-each resolve-extension unresolved-extensions)
      proto)
      
    (lambda () (parse-proto)))
)
