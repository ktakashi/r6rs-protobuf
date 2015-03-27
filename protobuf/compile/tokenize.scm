;; tokenize.scm: .proto tokenization routines for r6rs-protobuf
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

(library (protobuf compile tokenize)
  (export protoc:make-tokenizer

	  protoc:make-lexical-token
	  protoc:lexical-token-category
	  protoc:lexical-token-source
	  protoc:lexical-token-value)
  (import (rnrs))

  (define-record-type (protoc:lexical-token 
		       protoc:make-lexical-token 
		       protoc:lexical-token?)
    (fields category source value))
  
  (define-record-type (protoc:source-location 
		       protoc:make-source-location 
		       protoc:source-location?)
    (fields input line column offset))

  (define (protoc:make-tokenizer port)
    (define line 0)
    (define num-lines 0)
    (define column 0)
    (define offset 0)  
    (define lines (list))

    (define buffer (list))

    (define (lexer:read-char)
      (if (null? buffer)
	  (let ((c (read-char port)))
	    (or (eof-object? c)
		(begin
		  (if (char=? c #\newline)
		      (begin
			(set! line (+ line 1))
			(if (> line num-lines)
			    (begin (if (null? lines)
				       (set! lines (list column))
				       (set! lines 
					     (append lines (list column))))
				   (set! num-lines (+ num-lines 1))))
			(set! column 0))
		      (set! column (+ column 1)))
		  (set! offset (+ offset 1))))
	    c)
	  (let ((c (car buffer)))
	    (set! buffer (cdr buffer))
	    c)))
  
    (define (lexer:peek-char) (if (null? buffer) (peek-char port) (car buffer)))
    
    (define (lexer:unread-char c)
      (set! offset (- offset 1))
      (if (char? c)
	  (begin 
	    (if (char=? c #\newline)
		(begin (set! line (- line 1))
		       (set! column (list-ref lines line))))
	    (set! buffer (cons c buffer)))))

    (define (make-token category . value)
      (protoc:make-lexical-token 
       category 
       (protoc:make-source-location port line column offset) 
       (if (null? value) #f (car value))))

    (define (read-string type)
      (define (read-string-inner lst)
	(let ((c (lexer:read-char)))
	  (cond ((eof-object? c) (list->string (reverse lst)))
		((eqv? c #\newline) (list->string (reverse lst)))

		((eqv? c #\\)
		 (let ((ec (lexer:read-char)))
		   (cond ((eof-object? ec) (list->string (reverse lst)))
			 ((eqv? ec #\newline) (list->string (reverse lst)))
			 ((eqv? ec #\n)
			  (read-string-inner (cons #\newline lst)))
			 
			 (else (cons c (read-string type))))))
		
		((char=? c type) (list->string (reverse (cons c lst))))
		(else (read-string-inner (cons c lst))))))
      (read-string-inner '()))

    (define (ident-start-char? chr)
      (or (char-alphabetic? chr) (char=? chr #\_)))
    (define (ident-rest-char? chr)
      (or (ident-start-char? chr) (char-numeric? chr)))

    (define (read-number chr)
      (define (list->number lst radix) 
	(string->number (list->string lst) radix))

      (define (read-octal)
	(define (read-octal-inner lst)
	  (let ((c (lexer:read-char)))
	    (cond ((eof-object? c) (list->number (reverse lst) 8))
		  ((and (char>=? c #\0) (char<=? c #\7))
		   (read-octal-inner (cons c lst)))
		  (else (lexer:unread-char c)
			(list->number (reverse lst) 8)))))
	(read-octal-inner '(#\0)))
      
      (define (read-hex)
	(define (read-hex-inner lst)
	  (let ((c (lexer:read-char)))
	    (cond ((eof-object? c) (list->number (reverse lst) 16))
		  ((or (and (char>=? c #\0) (char<=? c #\9))
		       (and (char-ci>=? c #\a) (char-ci<=? c #\f)))
		   (read-hex-inner (cons c lst)))
		  (else (lexer:unread-char c)
			(list->number (reverse lst) 16)))))
	(read-hex-inner '(#\0)))
      
      (define (read-number-inner lst seen-dot? seen-e?)
	(let ((c (lexer:read-char)))
	  (cond ((eof-object? c) (list->number (reverse lst) 10))
		((char-numeric? c) 
		 (read-number-inner (cons c lst) seen-dot? seen-e?))
		((char-ci=? c #\e)
		 (if seen-e?
		     (begin (lexer:unread-char c) 
			    (list->number (reverse lst) 10))
		     (read-number-inner (cons c lst) #t #t)))
		((char=? c #\.) 
		 (if (or seen-dot? seen-e?)
		     (begin (lexer:unread-char c)
			    (list->number (reverse lst) 10))
		     (read-number-inner (cons c lst) #t #f)))
		(else (begin (lexer:unread-char c)
			     (list->number (reverse lst) 10))))))

      (if (eqv? chr #\0)
	  (let ((cc (lexer:read-char)))
	    (cond
	      ((char-ci=? cc #\x) (read-hex))
	      ((eqv? cc #\.) (read-number-inner (list #\. #\0) #t #f))
	      ((char-numeric? cc) (begin (lexer:unread-char cc) (read-octal)))
	      (else (begin (lexer:unread-char cc) 0))))			   
	  (read-number-inner (list chr) #f #f)))

    (define (consume-comments)
      (define (until-newline)
	(let ((c (lexer:read-char)))
	  (or (eof-object? c) (eqv? #\newline c) (until-newline))))

      (define (block-comment)
	;; TODO: should we allow nested comments?
	;; for now we do C style (not allowed)
	(lexer:read-char) ;; discards the first *
	(let loop ()
	  (let ((c (lexer:read-char)))
	    (case c
	      ((#\*)
	       (unless (eqv? (lexer:read-char) #\/)
		 (loop)))
	      (else (loop))))))

      (let ((c (lexer:peek-char)))
	(cond ((eof-object? c) #f)
	      ((eqv? c #\/)
	       (lexer:read-char)
	       (let ((c (lexer:peek-char)))
		 (cond ((eof-object? c) #f)
		       ((eqv? c #\/) (until-newline) #t)
		       ((eqv? c #\*) (block-comment) #t)
		       (else (lexer:unread-char c)))))
	      (else #f))))
    
    (define (consume-whitespace)
      (let ((c (lexer:peek-char)))
	(or (eof-object? c)
	    (and (char-whitespace? c)
		 (lexer:read-char)
		 (consume-whitespace)))))
    
    (define (consume-whitespace-and-comments)
      (consume-whitespace)
      (if (consume-comments) 
	  (consume-whitespace-and-comments)))
    
    (define (read-ident chr)
      (define (read-rest)
	(let ((c (lexer:read-char)))
	  (cond ((eof-object? c) '())
		((ident-rest-char? c) (cons c (read-rest)))
		(else (lexer:unread-char c) '()))))
      (list->string (cons chr (read-rest))))
    
    (lambda ()
      (consume-whitespace-and-comments)
      (let ((c (lexer:read-char)))
	(cond ((eof-object? c) '*eoi*)
	      ((eqv? c #\() (make-token 'LPAREN))
	      ((eqv? c #\)) (make-token 'RPAREN))
	      ((eqv? c #\[) (make-token 'LBRACK))
	      ((eqv? c #\]) (make-token 'RBRACK))
	      ((eqv? c #\{) (make-token 'LBRACE))
	      ((eqv? c #\}) (make-token 'RBRACE))
	      ((eqv? c #\.) (make-token 'DOT))
	      ((eqv? c #\=) (make-token 'EQUAL))
	      ((eqv? c #\;) (make-token 'SEMICOLON))
	      ((eqv? c #\,) (make-token 'COMMA))

	      ((or (eqv? c #\') (eqv? c #\")) 
	       (let* ((s (read-string c))
		      (l (string-length s)))
		 (if (or (eqv? l 0) (not (eqv? (string-ref s (- l 1)) c)))
		     (make-token 'UNTERMINATED-STRING-LITERAL s)
		     (make-token 'STRING-LITERAL (substring s 0 (- l 1))))))

	      ((eqv? c #\-)
	       (let ((cc (lexer:read-char)))
		 (if (and (char-ci>=? cc #\0) (char-ci<=? cc #\9))
		     (let ((n (read-number cc)))
		       (make-token (if (exact? n) 
				       'NUM-INTEGER 
				       'NUM-FLOAT) 
				   (- n)))
		     (make-token 'SYMBOL (string c)))))
	      ((char-numeric? c)
	       (let ((n (read-number c)))
		 (make-token (if (exact? n) 'NUM-INTEGER 'NUM-FLOAT) n)))

	      ((ident-start-char? c)
	       (let ((ident (read-ident c)))
		 (cond ((equal? ident "import") (make-token 'IMPORT))
		       ((equal? ident "package") (make-token 'PACKAGE))
		       ((equal? ident "message") (make-token 'MESSAGE))
		       ((equal? ident "extend") (make-token 'EXTEND))
		       ((equal? ident "service") (make-token 'SERVICE))
		       ((equal? ident "rpc") (make-token 'RPC))
		       ((equal? ident "enum") (make-token 'ENUM))
		       ((equal? ident "returns") (make-token 'RETURNS))
		       ((equal? ident "option") (make-token 'OPTION))
		       ((equal? ident "group") (make-token 'GROUP))
		       ((equal? ident "extensions") (make-token 'EXTENSIONS))
		       ((equal? ident "true") (make-token 'TRUE))
		       ((equal? ident "false") (make-token 'FALSE))
		       ((equal? ident "required") (make-token 'REQUIRED))
		       ((equal? ident "optional") (make-token 'OPTIONAL))
		       ((equal? ident "repeated") (make-token 'REPEATED))
		       ((equal? ident "to") (make-token 'TO))
		       ((equal? ident "max") (make-token 'MAX))
		       ((equal? ident "double") (make-token 'DOUBLE))
		       ((equal? ident "float") (make-token 'FLOAT))
		       ((equal? ident "int32") (make-token 'INT32))
		       ((equal? ident "int64") (make-token 'INT64))
		       ((equal? ident "uint32") (make-token 'UINT32))
		       ((equal? ident "uint64") (make-token 'UINT64))
		       ((equal? ident "sint32") (make-token 'SINT32))
		       ((equal? ident "sint64") (make-token 'SINT64))
		       ((equal? ident "fixed32") (make-token 'FIXED32))
		       ((equal? ident "fixed64") (make-token 'FIXED64))
		       ((equal? ident "sfixed32") (make-token 'SFIXED32))
		       ((equal? ident "sfixed64") (make-token 'SFIXED64))
		       ((equal? ident "bool") (make-token 'BOOL))
		       ((equal? ident "string") (make-token 'STRING))
		       ((equal? ident "bytes") (make-token 'BYTES))
		       (else (make-token 'IDENTIFIER ident)))))
	      (else (make-token 'SYMBOL (string c)))))))
)
