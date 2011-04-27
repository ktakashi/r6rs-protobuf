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
	  protoc:message-definition-fields
	  protoc:message-definition-options
	  protoc:message-definition-parent

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

  (define-record-type (protobuf:extension-definition
		       protobuf:make-extension-definition
		       protobuf:extension-definition?))

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
	 (p name (if (null? parent) #f (car parent)) package  '() '() '())))))

  (define-record-type (protoc:type-reference
		       protoc:make-type-reference
		       protoc:type-reference?)
    (fields name 
	    (mutable descriptor
		     protoc:type-reference-descriptor
		     protoc:set-type-reference-descriptor!)))

  (define-record-type (protoc:field-definition
		       protoc:make-field-definition
		       protoc:field-definition?)
    (fields message
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
    (define unresolved-type-fields (list))
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

    (define (resolve-type p)
      (define (resolve-type-relative name context)
	(define (resolve-type-relative-inner components context)
	  (let* ((first-component (car components))
		 (definitions 
		   (cond ((protoc:package? context)
			  (protoc:package-definitions context))
			 ((protoc:message-definition? context)
			  (protoc:message-definition-definitions context))
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
	      (if (protoc:message-definition? context)
		  (cond ((protoc:message-definition-parent context)
			 (resolve-type-upwards
			  name (protoc:message-definition-parent context)))
			((protoc:message-definition-package context)
			 (resolve-type-upwards 
			  name (protoc:message-definition-package context)))
			(else #f))
		  (and (protoc:package-parent context)
		       (resolve-type-upwards 
			name (protoc:package-parent context)))))))
	
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

      (let* ((name (car p))
	     (field (cdr p))
	     (message (protoc:field-definition-message field))
	     (package (protoc:message-definition-package message))
	     (type-reference (protoc:field-definition-type field))
	     (descriptor (hashtable-ref resolved-type-descriptors name #f))
	     (definition (and (not descriptor)
			      (or (resolve-type-upwards name message)
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

    (define (parse-type)
      (get-token)
      (case current-category
	((DOUBLE) (protoc:make-type-reference 
		   "double" protobuf:field-type-double))
	((FLOAT) (protoc:make-type-reference "float" protobuf:field-type-float))
	((INT32) (protoc:make-type-reference "int32" protobuf:field-type-int32))
	((INT64) (protoc:make-type-reference "int64" protobuf:field-type-int64))
	((UINT32) (protoc:make-type-reference
		   "uint32" protobuf:field-type-uint32))
	((UINT64) (protoc:make-type-reference
		   "uint64" protobuf:field-type-uint64))	 
	((SINT32) (protoc:make-type-reference
		   "sint32" protobuf:field-type-sint32))
	((SINT64) (protoc:make-type-reference
		   "sint64" protobuf:field-type-sint64))
	((FIXED32) (protoc:make-type-reference
		    "fixed32" protobuf:field-type-fixed32))
	((FIXED64) (protoc:make-type-reference 
		    "fixed64" protobuf:field-type-fixed64))
	((SFIXED32) (protoc:make-type-reference
		     "sfixed32" protobuf:field-type-sfixed32))
	((SFIXED64) (protoc:make-type-reference
		     "sfixed64" protobuf:field-type-sfixed64))
	((BOOL) (protoc:make-type-reference "bool" protobuf:field-type-bool))
	((STRING) (protoc:make-type-reference 
		   "string" protobuf:field-type-string))
	((BYTES) (protoc:make-type-reference "bytes" protobuf:field-type-bytes))

	((IDENTIFIER)
	 (let loop ((name ""))
	   (let ((val current-value))
	     (get-token)
	     (if (eq? current-category 'DOT)
		 (begin (get-token) (loop (string-append name val ".")))
		 (begin (unget-token current-token)
			(protoc:make-type-reference 
			 (string-append name val) #f))))))
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

    (define (parse-message parent)
      (define (parse-message-element message-def)
	(define (parse-field rule)
	  (define (parse-maybe-field-options) #f)

	  (let ((type (parse-type)))
	    (assert-next-category 'IDENTIFIER)
	    (let ((field-name current-value))
	      (assert-next-category 'EQUAL)
	      (assert-next-category 'NUM-INTEGER)
	      (let* ((index current-value)
		     (options (parse-maybe-field-options))
		     (fd (protoc:make-field-definition 
			  message-def rule type field-name index options)))

		(if (not (protoc:type-reference-descriptor type))
		    (set! unresolved-type-fields 
			  (cons (cons (protoc:type-reference-name type) fd)
				unresolved-type-fields)))

		(assert-next-category 'SEMICOLON)

		fd))))
	
	(get-token)
	(case current-category
	  ((ENUM) 
	   (protoc:set-message-definition-definitions!
	    message-def 
	    (cons (parse-enum message-def) 
		  (protoc:message-definition-definitions message-def)))
	   (parse-message-element message-def))
	  ((MESSAGE)
	   (protoc:set-message-definition-definitions!
	    message-def
	    (cons (parse-message message-def) 
		  (protoc:message-definition-definitions message-def)))
	   (parse-message-element message-def))
	  ((OPTIONAL)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field 'optional) 
			      (protoc:message-definition-fields message-def)))	
	   (parse-message-element message-def))
	  ((RBRACE) message-def)
	  ((REPEATED)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field 'repeated) 
			      (protoc:message-definition-fields message-def)))	
	   (parse-message-element message-def))
	  ((REQUIRED)
	   (protoc:set-message-definition-fields!
	    message-def (cons (parse-field 'required) 
			      (protoc:message-definition-fields message-def)))
	   (parse-message-element message-def))
	  (else (unexpected-token-error))))

      (assert-next-category 'IDENTIFIER)
      (let ((name current-value))
	(assert-next-category 'LBRACE)
	(let ((md (protoc:make-message-definition name current-package parent)))
	  (parse-message-element md))))
    
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
      (for-each resolve-type unresolved-type-fields)
      proto)
      
    (lambda () (parse-proto)))
)
