;; test-parse.scm: parser test routines for r6rs-protobuf
;; Copyright (C) 2011 Julian Graham
;; Copyright (C) 2015 Takashi Kato

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

(import (rnrs)
	(srfi :64)
	(protobuf compile parse)
	(protobuf compile tokenize)
	(protobuf private))

(define (mock-lexer . token-list)
  (define tokens token-list)
  (lambda ()
    (if (null? tokens)
	'*eoi*
	(let ((token (car tokens)))
	  (set! tokens (cdr tokens))
	  (if (pair? token)	      
	      (protoc:make-lexical-token (car token) #f (cdr token))
	      (protoc:make-lexical-token token #f #f))))))

(define (option-declaration-equal? o1 o2)
  (and (protoc:option-declaration? o1)
       (protoc:option-declaration? o2)
       (eq? (protoc:option-declaration-name o1)
	       (protoc:option-declaration-name o2))
       (equal? (protoc:option-declaration-value o1)
	       (protoc:option-declaration-value o2))))

(define (proto-definition-equal? p1 p2)
  (and (protoc:proto? p1)
       (protoc:proto? p2)
       (let loop ((options1 (protoc:proto-options p1))
		  (options2 (protoc:proto-options p2)))
	 (if (null? options1)
	     (null? options2)
	     (let ((option1 (car options1))
		   (option2 (car options2)))
	       (and (option-declaration-equal? option1 option2)
		    (loop (cdr options1) (cdr options2))))))		   
       
       (package-equal? (protoc:proto-root-package p1)
		       (protoc:proto-root-package p2))))

(define (package-equal? p1 p2)
  (and (protoc:package? p1)
       (protoc:package? p2)
       (equal? (protoc:package-name p1)
	       (protoc:package-name p2))
       (let loop ((options1 (protoc:package-options p1))
		  (options2 (protoc:package-options p2)))
	 (if (null? options1)
	     (null? options2)
	     (let ((option1 (car options1))
		   (option2 (car options2)))
	       (and (option-declaration-equal? option1 option2)
		    (loop (cdr options1) (cdr options2))))))		        
       (let loop ((definitions1 (protoc:package-definitions p1))
		  (definitions2 (protoc:package-definitions p2)))
	 (if (null? definitions1)
	     (null? definitions2)
	     (let ((definition1 (car definitions1))
		   (definition2 (car definitions2)))
	       (cond ((and (protoc:message-definition? definition1)
			   (protoc:message-definition? definition2))
		      (and (message-definition-equal? definition1 definition2)
			   (loop (cdr definitions1) (cdr definitions2))))
		     ((and (protoc:enum-definition? definition1)
			   (protoc:enum-definition? definition2))
		      (and (enum-definition-equal? definition1 definition2)
			   (loop (cdr definitions1) (cdr definitions2))))
		     ((and (protoc:extension-definition? definition1)
			   (protoc:extension-definition? definition2))
		      (and (extension-definition-equal? definition1 definition2)
			   (loop (cdr definitions1) (cdr definitions2))))
		     (else #f)))))))

(define (message-definition-equal? m1 m2)
  (and (protoc:message-definition? m1)
       (protoc:message-definition? m2)
       (equal? (protoc:message-definition-name m1)
	       (protoc:message-definition-name m2))
       (let loop ((options1 (protoc:message-definition-options m1))
		  (options2 (protoc:message-definition-options m2)))
	 (if (null? options1)
	     (null? options2)
	     (let ((option1 (car options1))
		   (option2 (car options2)))
	       (and (option-declaration-equal? option1 option2)
		    (loop (cdr options1) (cdr options2))))))
       (let loop ((fields1 (protoc:message-definition-fields m1))
		  (fields2 (protoc:message-definition-fields m2)))
	 (if (null? fields1)
	     (null? fields2)
	     (let ((field1 (car fields1))
		   (field2 (car fields2)))
	       (and (field-definition-equal? field1 field2)
		    (loop (cdr fields1) (cdr fields2))))))))

(define (field-definition-equal? f1 f2)
  (define (type-reference-equal? f1 f2)
    (and (protoc:type-reference? f1)
	 (protoc:type-reference? f2)
	 (equal? (protoc:type-reference-name f1)
		 (protoc:type-reference-name f2))
	 (eq? (protoc:type-reference-descriptor f1)
	      (protoc:type-reference-descriptor f2))
	 (eq? (protoc:type-reference-location f1)
	      (protoc:type-reference-location f2))))
  (and (protoc:field-definition? f1)
       (protoc:field-definition? f2)
       (eq? (protoc:field-definition-rule f1)
	    (protoc:field-definition-rule f2))
       (type-reference-equal? (protoc:field-definition-type f1)
			      (protoc:field-definition-type f2))
       (equal? (protoc:field-definition-name f1)
	       (protoc:field-definition-name f2))
       (eqv? (protoc:field-definition-ordinal f1)
	     (protoc:field-definition-ordinal f2))
       (let loop ((options1 (protoc:field-definition-options f1))
		  (options2 (protoc:field-definition-options f2)))
	 (if (null? options1)
	     (null? options2)
	     (let ((option1 (car options1))
		   (option2 (car options2)))
	       (and (option-declaration-equal? option1 option2)
		    (loop (cdr options1) (cdr options2))))))))

(define (extension-range-definition-equal? e1 e2)
  (and (protoc:extension-range-definition? e1)
       (protoc:extension-range-definition? e2)
       (eqv? (protoc:extension-range-definition-from e1)
	     (protoc:extension-range-definition-from e2))
       (eqv? (protoc:extension-range-definition-to e1)
	     (protoc:extension-range-definition-to e2))))

(define (extension-definition-equal? e1 e2)
  (define (type-reference-equal? t1 t2)
    (and (protoc:type-reference? t1)
	 (protoc:type-reference? t2)
	 (equal? (protoc:type-reference-name t1)
		 (protoc:type-reference-name t2))))
  (and (protoc:extension-definition? e1)
       (protoc:extension-definition? e2)
       (type-reference-equal? (protoc:extension-definition-target e1)
			      (protoc:extension-definition-target e2))
       (let loop ((fields1 (protoc:extension-definition-fields e1))
		  (fields2 (protoc:extension-definition-fields e2)))
	 (if (null? fields1)
	     (null? fields2)
	     (let ((field1 (car fields1))
		   (field2 (car fields2)))
	       (and (field-definition-equal? field1 field2)
		    (loop (cdr fields1) (cdr fields2))))))))

(define (enum-value-definition-equal? v1 v2)
  (and (protoc:enum-value-definition? v1)
       (protoc:enum-value-definition? v2)
       (equal? (protoc:enum-value-definition-name v1)
	       (protoc:enum-value-definition-name v2))
       (eqv? (protoc:enum-value-definition-ordinal v1)
	     (protoc:enum-value-definition-ordinal v2))))

(define (enum-definition-equal? e1 e2)
  (and (protoc:enum-definition? e1)
       (protoc:enum-definition? e2)
       (equal? (protoc:enum-definition-name e1)
	       (protoc:enum-definition-name e2))
       (let loop ((options1 (protoc:enum-definition-options e1))
		  (options2 (protoc:enum-definition-options e2)))
	 (if (null? options1)
	     (null? options2)
	     (let ((option1 (car options1))
		   (option2 (car options2)))
	       (and (option-declaration-equal? option1 option2)
		    (loop (cdr options1) (cdr options2))))))
       (let loop ((values1 (protoc:enum-definition-values e1))
		  (values2 (protoc:enum-definition-values e2)))
	 (if (null? values1)
	     (null? values2)
	     (let ((value1 (car values1))
		   (value2 (car values2)))
	       (and (enum-value-definition-equal? value1 value2)
		    (loop (cdr values1) (cdr values2))))))))

(test-begin "parse")
(test-begin "simple")

(test-group "package"
  (let* ((p ((protoc:make-parser 
	      (mock-lexer 'PACKAGE '(IDENTIFIER . "foo") 'SEMICOLON))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-package "foo" target-root-package)))
    (protoc:set-package-subpackages!
     target-root-package 
     (cons q (protoc:package-subpackages target-root-package)))
    (test-assert "proto-definition-equal?"
     (proto-definition-equal? (protoc:make-proto target-root-package) p))))

(test-group "message"
  (let* ((p ((protoc:make-parser 
	      (mock-lexer 'MESSAGE '(IDENTIFIER . "Foo") 'LBRACE 'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package)))
    (protoc:set-package-definitions!
     target-root-package 
     (cons q (protoc:package-definitions target-root-package)))
    (test-assert "proto-definition-equal?"
     (proto-definition-equal? (protoc:make-proto target-root-package) p))))

(test-group "enum"
  (let* ((p ((protoc:make-parser 
	      (mock-lexer 'ENUM '(IDENTIFIER . "Foo") 'LBRACE 'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-enum-definition "Foo" target-root-package)))
    (protoc:set-package-definitions!
     target-root-package
     (cons q (protoc:package-definitions target-root-package)))
    (test-assert "proto-definition-equal?"
     (proto-definition-equal? (protoc:make-proto target-root-package) p))))

(test-group "extension"
  (let* ((p ((protoc:make-parser
	      (mock-lexer 'MESSAGE '(IDENTIFIER . "Foo") 'LBRACE 'EXTENSIONS
			  '(NUM-INTEGER . 1) 'SEMICOLON 'RBRACE 'EXTEND
			  '(IDENTIFIER . "Foo") 'LBRACE 'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package))
	 (rt (protoc:make-type-reference "Foo"))
	 (r (protoc:make-extension-definition rt target-root-package)))
    (protoc:set-type-reference-location! rt r)
    (protoc:set-package-definitions! target-root-package (list r q))
    (test-assert "proto-definition-equal?"
     (proto-definition-equal? (protoc:make-proto target-root-package) p))))

(test-end "simple")
(test-begin "enum")

(test-group "values"
  (let* ((p ((protoc:make-parser
	      (mock-lexer 'ENUM '(IDENTIFIER . "Foo") 'LBRACE
			  '(IDENTIFIER . "FOO") 'EQUAL '(NUM-INTEGER . 1)
			  'SEMICOLON 'RBRACE)))))
    (test-assert p)))

(test-end "enum")
(test-begin "message")

(test-begin "field")

(test-group "recursive"
  (let* ((p ((protoc:make-parser
	      (mock-lexer 'MESSAGE
			  '(IDENTIFIER . "Foo")
			  'LBRACE
			  'OPTIONAL
			  '(IDENTIFIER . "Foo")
			  '(IDENTIFIER . "foo")
			  'EQUAL 
			  '(NUM-INTEGER . 1)
			  'SEMICOLON
			  'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package)))
    (protoc:set-message-definition-fields!
     q (list (protoc:make-field-definition 
	      q 'required (protoc:make-type-reference 
			   "Foo" (protobuf:make-message-field-type-descriptor 
				  "Foo" #f #f #f #f #f
				  q))
	      "foo" 1))))
)

;; disabled it for now, I don't know what's wrong.
;; (test-group "options"
;;   (let* ((p ((protoc:make-parser
;; 	      (mock-lexer 'MESSAGE
;; 			  '(IDENTIFIER . "Foo")
;; 			  'LBRACE
;; 			  'REQUIRED
;; 			  'STRING
;; 			  '(IDENTIFIER . "foo")
;; 			  'EQUAL
;; 			  '(NUM-INTEGER . 1)
;; 			  'LBRACK
;; 			  '(IDENTIFIER . "bar_option")
;; 			  'EQUAL
;; 			  '(STRING-LITERAL . "bar_value")
;; 			  'RBRACK
;; 			  'SEMICOLON
;; 			  'RBRACE))))
;; 	 (target-root-package (protoc:make-package #f #f))
;; 	 (q (protoc:make-message-definition "Foo" target-root-package)))
;;     (protoc:set-message-definition-fields!
;;      q (list (protoc:make-field-definition 
;; 	      q 'required (protoc:make-type-reference 
;; 			   "string" protobuf:field-type-string)
;; 	      "foo" 1 (list (protoc:make-option-declaration 
;; 			     'bar_option "bar_value")))))
;;     (protoc:set-package-definitions!
;;      target-root-package 
;;      (cons q (protoc:package-definitions target-root-package)))
;;     (test-assert "proto-definition-equal? (options)"
;;      (proto-definition-equal? (protoc:make-proto target-root-package) p))))
(test-end "field")

(test-group "extension-ranges"
  (let* ((p ((protoc:make-parser
	      (mock-lexer 'MESSAGE 
			  '(IDENTIFIER . "Foo")
			  'LBRACE
			  'EXTENSIONS
			  '(NUM-INTEGER . 1)
			  'TO
			  '(NUM-INTEGER . 3)
			  'SEMICOLON
			  'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package)))
    (protoc:set-message-definition-extension-ranges!
     q (list (protoc:make-extension-range-definition 1 3)))
    (protoc:set-package-definitions!
     target-root-package
     (cons q (protoc:package-definitions target-root-package)))
    (test-assert
     (proto-definition-equal? (protoc:make-proto target-root-package) p)))

  (let* ((p ((protoc:make-parser
	      (mock-lexer 'MESSAGE 
			  '(IDENTIFIER . "Foo")
			  'LBRACE
			  'EXTENSIONS
			  '(NUM-INTEGER . 1)
			  'SEMICOLON
			  'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package)))
    (protoc:set-message-definition-extension-ranges!
     q (list (protoc:make-extension-range-definition 1 1)))
    (protoc:set-package-definitions!
     target-root-package
     (cons q (protoc:package-definitions target-root-package)))
    (test-assert
     (proto-definition-equal? (protoc:make-proto target-root-package) p)))

  (let* ((p ((protoc:make-parser
	      (mock-lexer 'MESSAGE 
			  '(IDENTIFIER . "Foo")
			  'LBRACE
			  'EXTENSIONS
			  '(NUM-INTEGER . 1)
			  'COMMA
			  '(NUM-INTEGER . 2)
			  'COMMA
			  '(NUM-INTEGER . 3)
			  'SEMICOLON
			  'RBRACE))))
	 (target-root-package (protoc:make-package #f #f))
	 (q (protoc:make-message-definition "Foo" target-root-package)))
    (protoc:set-message-definition-extension-ranges!
     q (list (protoc:make-extension-range-definition 1 1)))
    (protoc:set-package-definitions!
     target-root-package
     (cons q (protoc:package-definitions target-root-package)))
    (test-assert
     (proto-definition-equal? (protoc:make-proto target-root-package) p))))

(test-end "message")
(test-end "parse")
