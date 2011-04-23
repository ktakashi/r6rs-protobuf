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
	  protoc:package-subpackages
	  protoc:set-package-definitions!
	  protoc:set-package-subpackages!
	  
	  protoc:make-message-definition
	  protoc:message-definition?
	  protoc:message-definition-name
	  protoc:message-definition-fields
	  protoc:message-definition-options

	  protoc:make-enum-definition
	  protoc:enum-definition?
	  protoc:enum-definition-name
	  protoc:enum-definition-values
	  protoc:enum-definition-options
	  
	  protoc:make-enum-value-definition
	  protoc:enum-value-definition?
	  protoc:enum-value-definition-name
	  protoc:enum-value-definition-ordinal
	  
	  protoc:make-type-reference
	  protoc:type-reference?
	  protoc:type-reference-expr
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
	  (protobuf compile tokenize))

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
	   ((0) (p name parent '() '() '()))
	   ((1) (p name parent (car rest) '() '()))
	   ((2) (p name parent (car rest) (cadr rest) '()))
	   ((3) (apply p (cons name (cons parent rest))))
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
	    (mutable options 
		     protoc:message-definition-options
		     protoc:set-message-definition-options!)
	    (mutable fields 
		     protoc:message-definition-fields
		     protoc:set-message-definition-fields!)
	    (mutable definitions 
		     protoc:message-definition-definitions
		     protoc:set-message-definition-definitions!))
    (protocol (lambda (p) (lambda (name) (p name '() '() '())))))

  (define-record-type (protoc:type-reference
		       protoc:make-type-reference
		       protoc:type-reference?)
    (fields name))

  (define-record-type (protoc:primitive-type-reference
		       protoc:make-primitive-type-reference
		       protoc:primitive-type-reference?)
    (parent protoc:type-reference)
    (fields descriptor))

  (define-record-type (protoc:field-definition
		       protoc:make-field-definition
		       protoc:field-definition?)
    (fields rule
	    type
	    name
	    ordinal
	    (mutable options
		     protoc:field-definition-options
		     protoc:set-field-definition-options!))
    (protocol (lambda (p) 
		(lambda (rule type type-expr name ordinal . options) 
		  (p rule type type-expr name ordinal 
		     (if (null? options) options (car options)))))))
 
  (define-record-type (protoc:enum-value-definition
		       protoc:make-enum-value-definition
		       protoc:enum-value-definition?)
    (fields name ordinal))

  (define-record-type (protoc:enum-definition
		       protoc:make-enum-definition
		       protoc:enum-definition?)
    (fields name 
	    (mutable options 
		     protoc:enum-definition-options
		     protoc:set-enum-definition-options!)
	    (mutable values 
		     protoc:enum-definition-values 
		     protoc:set-enum-definition-values!))
    (protocol (lambda (p) (lambda (name) (p name '() '())))))
  
  (define (merge-package! scope package) #f)

  (define (protoc:make-parser lexer)

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

    (define (parse-type)
      (get-token)
      (case current-category
	((DOUBLE) (protoc:make-primitive-type-reference 
		   "double" protobuf:field-type-double))
	((FLOAT) (protoc:make-primitive-type-reference 
		  "float" protobuf:field-type-float))
	((INT32) (protoc:make-primitive-type-reference 
		  "int32" protobuf:field-type-int32))
	((INT64) (protoc:make-primitive-type-reference
		  "int64" protobuf:field-type-int64))
	((UINT32) (protoc:make-primitive-type-reference
		   "uint32" protobuf:field-type-uint32))
	((UINT64) (protoc:make-primitive-type-reference
		   "uint64" protobuf:field-type-uint64))	 
	((SINT32) (protoc:make-primitive-type-reference
		   "sint32" protobuf:field-type-sint32))
	((SINT64) (protoc:make-primitive-type-reference
		   "sint64" protobuf:field-type-sint64))
	((FIXED32) (protoc:make-primitive-type-reference
		    "fixed32" protobuf:field-type-fixed32))
	((FIXED64) (protoc:make-primitive-type-reference 
		    "fixed64" protobuf:field-type-fixed64))
	((SFIXED32) (protoc:make-primitive-type-reference
		     "sfixed32" protobuf:field-type-sfixed32))
	((SFIXED64) (protoc:make-primitive-type-reference
		     "sfixed64" protobuf:field-type-sfixed64))
	((BOOL)
	 (protoc:make-primitive-type-reference "bool" protobuf:field-type-bool))
	((STRING) (protoc:make-primitive-type-reference 
		   "string" protobuf:field-type-string))
	((BYTES) (protoc:make-primitive-type-reference
		  "bytes" protobuf:field-type-bytes))

	((IDENTIFIER)
	 (let loop ((name ""))
	   (let ((val current-value))
	     (get-token)
	     (if (eq? current-category 'DOT)
		 (loop (string-append name "." val))
		 (begin (unget-token current-token) 
			(protoc:make-type-reference name))))))
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
      (assert-next-category 'STRING-LITERAL)
      (let* ((lexer (protoc:make-tokenizer 
		     (open-input-file current-value)))
	     (parser (protoc:make-parser lexer))
	     (proto (parser)))
	(for-each (lambda (package)
		    (merge-package! external-packages package))
		  (list (protoc:proto-root-package proto))))
      (assert-next-category 'SEMICOLON))

    (define (parse-enum)
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
      (let ((enum (protoc:make-enum-definition current-value)))
	(assert-next-category 'LBRACE)
	(parse-enum-elements enum)))

    (define (parse-message)
      (define (parse-message-element message-definition)
	(define (parse-field rule)
	  (define (parse-maybe-field-options) #f)

	  (let ((type (parse-type)))
	    (assert-next-category 'IDENTIFIER)
	    (let ((field-name current-value))
	      (assert-next-category 'EQUAL)
	      (assert-next-category 'NUM-INTEGER)
	      (let ((index current-value)
		    (options (parse-maybe-field-options)))
		(assert-next-category 'SEMICOLON)
		(protoc:make-field-definition 
		 rule type field-name index options)))))

	(get-token)
	(case current-category
	  ((ENUM) 
	   (protoc:set-message-definition-definitions!
	    message-definition
	    (cons (parse-enum) (protoc:message-definition-definitions 
				message-definition)))
	   (parse-message-element message-definition))
	  ((MESSAGE)
	   (protoc:set-message-definition-definitions!
	    message-definition
	    (cons (parse-message) (protoc:message-definition-definitions 
				   message-definition)))
	   (parse-message-element message-definition))
	  ((OPTIONAL)
	   (protoc:set-message-definition-fields!
	    message-definition
	    (cons (parse-field 'optional) 
		  (protoc:message-definition-fields message-definition)))	
	   (parse-message-element message-definition))
	  ((RBRACE) message-definition)
	  ((REPEATED)
	   (protoc:set-message-definition-fields!
	    message-definition
	    (cons (parse-field 'repeated) 
		  (protoc:message-definition-fields message-definition)))	
	   (parse-message-element message-definition))
	  ((REQUIRED)
	   (protoc:set-message-definition-fields!
	    message-definition
	    (cons (parse-field 'required) 
		  (protoc:message-definition-fields message-definition)))
	   (parse-message-element message-definition))
	  (else (unexpected-token-error))))

      (assert-next-category 'IDENTIFIER)
      (let ((message-name current-value))
	(assert-next-category 'LBRACE)
	(let ((md (protoc:make-message-definition message-name)))
	  (parse-message-element md))))
    
    (define (parse-proto)
      (define (parse-proto-elements)
	(get-token)
	(case current-category
	  ((ENUM) 
	   (let ((enum (parse-enum)))
	     (protoc:set-package-definitions!
	      current-package (cons enum (protoc:package-definitions
					  current-package))))
	   (parse-proto-elements))
	  ((IMPORT) (parse-import) (parse-proto))
	  ((MESSAGE) 
	   (let ((message (parse-message)))
	     (protoc:set-package-definitions!
	      current-package (cons message (protoc:package-definitions 
					     current-package))))
	   (parse-proto-elements))
	  ((PACKAGE) (parse-package) (parse-proto))
	  ((*eoi*) proto)
	  (else (unexpected-token-error))))
      
      (parse-proto-elements))

    (lambda () (parse-proto)))
)
