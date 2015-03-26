;; test-tokenize.scm: lexer test routines for r6rs-protobuf
;; Copyright (C) 2012 Julian Graham
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
	(protobuf compile tokenize))

; (set! test-log-to-file #f)
(define open-input-string open-string-input-port)

(define (token-map str f)
  (let loop ((lexer (protoc:make-tokenizer (open-input-string str)))
	     (lst '()))
    (let ((tok (lexer)))
      (if (eq? tok '*eoi*) 
	  (reverse lst) 
	  (loop lexer (cons (f tok) lst))))))

(define (token-categories str)
  (token-map str protoc:lexical-token-category))
(define (token-pairs str)
  (token-map str (lambda (tok) (cons (protoc:lexical-token-category tok) 
				     (protoc:lexical-token-value tok)))))

(test-begin "protobuf:tokenize")
(test-begin "simple")

(test-group "lparen" (test-equal '(LPAREN) (token-categories "(")))
(test-group "rparen" (test-equal '(RPAREN) (token-categories ")")))
(test-group "lbrack" (test-equal '(LBRACK) (token-categories "[")))
(test-group "rbrack" (test-equal '(RBRACK) (token-categories "]")))
(test-group "lbrace" (test-equal '(LBRACE) (token-categories "{")))
(test-group "rbrace" (test-equal '(RBRACE) (token-categories "}")))
(test-group "dot" (test-equal '(DOT) (token-categories ".")))
(test-group "equal" (test-equal '(EQUAL) (token-categories "=")))
(test-group "semicolon" (test-equal '(SEMICOLON) (token-categories ";")))
(test-group "comma" (test-equal '(COMMA) (token-categories ",")))
(test-group "import" (test-equal '(IMPORT) (token-categories "import")))
(test-group "package" (test-equal '(PACKAGE) (token-categories "package")))
(test-group "message" (test-equal '(MESSAGE) (token-categories "message")))
(test-group "extend" (test-equal '(EXTEND) (token-categories "extend")))
(test-group "service" (test-equal '(SERVICE) (token-categories "service")))
(test-group "rpc" (test-equal '(RPC) (token-categories "rpc")))
(test-group "enum" (test-equal '(ENUM) (token-categories "enum")))
(test-group "returns" (test-equal '(RETURNS) (token-categories "returns")))
(test-group "option" (test-equal '(OPTION) (token-categories "option")))
(test-group "group" (test-equal '(GROUP) (token-categories "group")))
(test-group "extensions" 
  (test-equal '(EXTENSIONS) (token-categories "extensions")))
(test-group "true" (test-equal '(TRUE) (token-categories "true")))
(test-group "false" (test-equal '(FALSE) (token-categories "false")))
(test-group "required" (test-equal '(REQUIRED) (token-categories "required")))
(test-group "optional" (test-equal '(OPTIONAL) (token-categories "optional")))
(test-group "repeated" (test-equal '(REPEATED) (token-categories "repeated")))
(test-group "to" (test-equal '(TO) (token-categories "to")))
(test-group "max" (test-equal '(MAX) (token-categories "max")))
(test-group "double" (test-equal '(DOUBLE) (token-categories "double")))
(test-group "float" (test-equal '(FLOAT) (token-categories "float")))
(test-group "int32" (test-equal '(INT32) (token-categories "int32")))
(test-group "int64" (test-equal '(INT64) (token-categories "int64")))
(test-group "uint32" (test-equal '(UINT32) (token-categories "uint32")))
(test-group "uint64" (test-equal '(UINT64) (token-categories "uint64")))
(test-group "sint32" (test-equal '(SINT32) (token-categories "sint32")))
(test-group "sint64" (test-equal '(SINT64) (token-categories "sint64")))
(test-group "fixed32" (test-equal '(FIXED32) (token-categories "fixed32")))
(test-group "fixed64" (test-equal '(FIXED64) (token-categories "fixed64")))
(test-group "sfixed32" (test-equal '(SFIXED32) (token-categories "sfixed32")))
(test-group "sfixed64" (test-equal '(SFIXED64) (token-categories "sfixed64")))
(test-group "bool" (test-equal '(BOOL) (token-categories "bool")))
(test-group "string" (test-equal '(STRING) (token-categories "string")))
(test-group "bytes" (test-equal '(BYTES) (token-categories "bytes")))

(test-end "simple")

(test-begin "literals")

(test-group "integers"
  (test-equal '((NUM-INTEGER . 123)) (token-pairs "123"))
  (test-equal '((NUM-INTEGER . -456)) (token-pairs "-456")))
(test-group "floats"
  (test-equal '((NUM-FLOAT . 1.23)) (token-pairs "1.23"))
  (test-equal '((NUM-FLOAT . -0.456)) (token-pairs "-0.456")))
(test-group "strings"
  (test-equal '((STRING-LITERAL . "test")) (token-pairs "\"test\""))
  (test-equal '((STRING-LITERAL . "test")) (token-pairs "'test'"))
  (test-equal '((UNTERMINATED-STRING-LITERAL . "test")) (token-pairs "\"test"))
  (test-equal '((UNTERMINATED-STRING-LITERAL . "test")) (token-pairs "'test"))
  (test-equal '((UNTERMINATED-STRING-LITERAL . "test")
		(UNTERMINATED-STRING-LITERAL . ""))
	      (token-pairs "\"test\n\"")))

(test-end "literals")

(test-begin "comments")
(test-equal '((STRING-LITERAL . "foo")) (token-pairs "\"foo\" // Comment"))
(test-equal '((STRING-LITERAL . "foo") (STRING-LITERAL . "bar")) 
	    (token-pairs "\"foo\" // Comment\n\"bar\""))
(test-end "comments")

(test-end "protobuf:tokenize")
