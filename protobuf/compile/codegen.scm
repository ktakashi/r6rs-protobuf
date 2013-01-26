;; codegen.scm: code generation API for r6rs-protobuf
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

(library (protobuf compile codegen)
  (export protoc:default-naming-context
          
          protoc:make-naming-context
	  protoc:naming-context?
	  protoc:naming-context-library-name
	  protoc:naming-context-message-naming-context
	  protoc:naming-context-enum-naming-context
	  protoc:naming-context-builder-naming-context
	  protoc:naming-context-extension-naming-context
	  
	  protoc:enum-naming-context-type-name
	  protoc:message-naming-context-type-name

	  protoc:generate-package
	  protoc:generate-message
	  protoc:generate-enum
	  protoc:generate-extension
	  protoc:generate-builder)
  (import (rnrs)
	  (protobuf compile parse)
	  (protobuf private)
	  (srfi :13)
	  (srfi :14))

  (define-record-type (protoc:extension-naming-context
		       protoc:make-extension-naming-context
		       protoc:extension-naming-context?)
    (fields extension-name))

  (define-record-type (protoc:enum-naming-context
		       protoc:make-enum-naming-context
		       protoc:eum-naming-context?)
    (fields type-name constructor-name predicate-name value-name))

  (define-record-type (protoc:message-naming-context
		       protoc:make-message-naming-context
		       protoc:message-naming-context?)
    (fields type-name
	    predicate-name

	    field-accessor-name 
	    field-has-name

	    extension-accessor-name
	    extension-has-name
	    
	    writer-name 
	    reader-name))

  (define-record-type (protoc:builder-naming-context
		       protoc:make-builder-naming-context
		       protoc:builder-naming-context?)
    (fields type-name
	    constructor-name
	    predicate-name

	    field-accessor-name
	    field-mutator-name
	    field-clear-name
	    field-has-name

	    extension-accessor-name
	    extension-mutator-name
	    extension-clear-name
	    extension-has-name

	    build-name))
  
  (define-record-type (protoc:naming-context
		       protoc:make-naming-context
		       protoc:naming-context?)
    (fields library-name
	    enum-naming-context
	    message-naming-context
	    builder-naming-context
	    extension-naming-context))

  (define default-package-name "protobuf.default")

  (define (gensym-values . vars) 
    (apply values (syntax->datum (generate-temporaries vars))))

  (define (protoc:default-package-name-transformer package)
    (let ((package-name 
	   (or (protoc:package-name package) default-package-name)))
    (map string->symbol 
	 (string-tokenize package-name (char-set-complement (char-set #\.))))))

  (define (type-name-recursive def)
    (define (type-name-recursive-inner def suffix)
      (cond ((protoc:message-definition? def)
	     (if (protoc:message-definition-parent def)
		 (type-name-recursive-inner 
		  (protoc:message-definition-parent def)
		  (string-append 
		   "-" (protoc:message-definition-name def) suffix))
		 (string-append (protoc:message-definition-name def) suffix)))
	    ((protoc:enum-definition? def)
	     (if (protoc:enum-definition-parent def)
		 (type-name-recursive-inner 
		  (protoc:enum-definition-parent def)
		  (string-append "-" (protoc:enum-definition-name def) suffix))
		 (string-append (protoc:enum-definition-name def) suffix)))
	    (else (raise (make-assertion-violation)))))
    (type-name-recursive-inner def ""))

  (define protoc:default-enum-naming-context    
    (protoc:make-enum-naming-context
     (lambda (enum) (string->symbol (type-name-recursive enum)))
     (lambda (enum)
       (string->symbol (string-append "make-" (type-name-recursive enum))))
     (lambda (enum)
       (string->symbol (string-append (type-name-recursive enum) "?")))
     (lambda (enum value)
       (string->symbol 
	(string-append (type-name-recursive enum) "-" 
		       (protoc:enum-value-definition-name value))))))

  (define (default-message-builder-name message)
    (string-append (type-name-recursive message) "-builder"))

  (define protoc:default-message-naming-context 
    (protoc:make-message-naming-context 
     (lambda (message)
       (string->symbol (type-name-recursive message)))
     (lambda (message) 
       (string->symbol 
	(string-append (type-name-recursive message) "?")))

     (lambda (message field) 
       (string->symbol
	(string-append (type-name-recursive message) "-"
		       (protoc:field-definition-name field))))
     (lambda (message field)
       (string->symbol
	(string-append "has-" (type-name-recursive message) "-"
		       (protoc:field-definition-name field) "?")))

     (lambda (message)
       (string->symbol
	(string-append (type-name-recursive message) "-extension")))
     (lambda (message)
       (string->symbol
	(string-append "has-" (type-name-recursive message) "-extension?")))

     (lambda (message)
       (string->symbol (string-append (type-name-recursive message) "-write")))
     (lambda (message)
       (string->symbol (string-append (type-name-recursive message) "-read")))))

  (define protoc:default-builder-naming-context
    (protoc:make-builder-naming-context
     (lambda (message) (string->symbol (default-message-builder-name message)))
     (lambda (message) 
       (string->symbol 
	(string-append "make-" (default-message-builder-name message))))
     (lambda (message)
       (string->symbol
	(string-append (default-message-builder-name message) "?")))

     (lambda (message field)
       (string->symbol
	(string-append (default-message-builder-name message) "-"
		       (protoc:field-definition-name field))))
     (lambda (message field)
       (string->symbol 
	(string-append "set-" (default-message-builder-name message) "-"
		       (protoc:field-definition-name field) "!")))
     (lambda (message field)
       (string->symbol 
	(string-append "clear-" (default-message-builder-name message) "-"
		       (protoc:field-definition-name field) "!")))
     (lambda (message field)
       (string->symbol 
	(string-append "has-" (default-message-builder-name message) "-"
		       (protoc:field-definition-name field) "?")))

     (lambda (message)
       (string->symbol
	(string-append (default-message-builder-name message) "-extension")))
     (lambda (message)
       (string->symbol 
	(string-append "set-" (default-message-builder-name message) "-"
		       "extension!")))
     (lambda (message)
       (string->symbol 
	(string-append "clear-" (default-message-builder-name message) "-"
		       "extension!")))
     (lambda (message)
       (string->symbol 
	(string-append "has-" (default-message-builder-name message) "-"
		       "extension?")))

     (lambda (message)
       (string->symbol
	(string-append (default-message-builder-name message) "-build")))))
  
  (define protoc:default-extension-naming-context
    (protoc:make-extension-naming-context
     (lambda (extension field)
       (string->symbol
	(string-append (type-name-recursive
			(protobuf:message-field-type-descriptor-definition
			 (protoc:type-reference-descriptor
			  (protoc:extension-definition-target extension))))
		       "-" (protoc:field-definition-name field))))))
  
  (define protoc:default-naming-context
    (protoc:make-naming-context protoc:default-package-name-transformer 
				protoc:default-enum-naming-context
				protoc:default-message-naming-context 
				protoc:default-builder-naming-context
				protoc:default-extension-naming-context))

  (define default-imports
    '((rnrs base) (rnrs enums) (rnrs records syntactic) (protobuf private)))

  (define (protoc:generate-package package naming-context)
    (define enum-naming-context
      (protoc:naming-context-enum-naming-context naming-context))
    (define message-naming-context 
      (protoc:naming-context-message-naming-context naming-context))
    (define builder-naming-context 
      (protoc:naming-context-builder-naming-context naming-context))
    (define extension-naming-context
      (protoc:naming-context-extension-naming-context naming-context))

    (define (determine-imports package naming-context local-name)
      (define (determine-declaring-package definition)
	(cond ((protoc:enum-definition? definition)
	       (if (protoc:enum-definition-parent definition)
		   (determine-declaring-package
		    (protoc:enum-definition-parent definition))
		   (protoc:enum-definition-package definition)))
	      ((protoc:extension-definition? definition)
	       (if (protoc:extension-definition-parent definition)
		   (determine-declaring-package
		    (protoc:extension-definition-parent definition))
		   (protoc:extension-definition-package definition)))
	      ((protoc:message-definition? definition)
	       (if (protoc:message-definition-parent definition)
		   (determine-declaring-package
		    (protoc:message-definition-parent definition))
		   (protoc:message-definition-package definition)))
	      (else (raise (make-assertion-violation)))))

      (define (packages-for-fields field-definitions required-libraries)
	(if (null? field-definitions)
	    required-libraries
	    (let* ((field-definition (car field-definitions))
		   (field-type-reference 
		    (protoc:field-definition-type field-definition))
		   (field-type (protoc:type-reference-descriptor 
				field-type-reference))
		   (definition
		     (cond
		      ((protobuf:message-field-type-descriptor? field-type)
		       (protobuf:message-field-type-descriptor-definition 
			field-type))
		      ((protobuf:enum-field-type-descriptor? field-type)
		       (protobuf:enum-field-type-descriptor-definition 
			field-type))
		      (else #f))))
	      (packages-for-fields
	       (cdr field-definitions)	       
	       (if definition
		   (let* ((package (determine-declaring-package definition))
			  (library ((protoc:naming-context-library-name 
				     naming-context) package)))

		     (if (or (equal? library local-name)
			     (member library required-libraries))
			 required-libraries
			 (cons library required-libraries)))
		   required-libraries)))))

      (let loop ((definitions (protoc:package-definitions package))
		 (imported-libraries (list)))
	(if (null? definitions)
	    imported-libraries
	    (let ((definition (car definitions)))
	      (cond ((protoc:message-definition? definition)
		     (let* ((fields 
			     (protoc:message-definition-fields definition))
			    (sub-definitions
			     (protoc:message-definition-definitions 
			      definition)))
		       (loop (append sub-definitions (cdr definitions))
			     (append (packages-for-fields 
				      fields imported-libraries) 
				     imported-libraries))))

		    ((protoc:extension-definition? definition)
		     (let* ((fields
			     (protoc:extension-definition-fields definition)))
		       (loop (cdr definitions)
			     (append (packages-for-fields 
				      fields imported-libraries) 
				     imported-libraries))))

		    (else (loop (cdr definitions) imported-libraries)))))))

    (define (generate-definition definition)
      (cond ((protoc:message-definition? definition)
	     (append
	      (protoc:generate-message definition naming-context)
	      (protoc:generate-builder definition naming-context)
	      (apply append
		     (map generate-definition 
			  (protoc:message-definition-definitions definition)))))
	    ((protoc:enum-definition? definition)
	     (protoc:generate-enum definition enum-naming-context))
	    ((protoc:extension-definition? definition)
	     (protoc:generate-extension definition naming-context))
	    (else '())))

    (let ((lib ((protoc:naming-context-library-name naming-context) package)))
      `(library ,lib
         (export ,@(protoc:package-exports package naming-context))
	 (import ,@(append default-imports 
			   (determine-imports package naming-context lib)))
	 ,@(let loop ((definitions 
			(protoc:package-definitions package))
		      (output '()))
	     (if (or (not definitions) (null? definitions))
		 (reverse output)
		 (let ((definition (car definitions)))
		   (loop (cdr definitions) 
			 (append output (generate-definition 
					 definition)))))))))
  
  (define (protoc:enum-exports enum enum-naming-context)
    (list ((protoc:enum-naming-context-type-name enum-naming-context) enum)
	  ((protoc:enum-naming-context-predicate-name enum-naming-context) enum)
	  ((protoc:enum-naming-context-constructor-name enum-naming-context)
	   enum)))
  
  (define (protoc:message-exports message message-naming-context)
    (define accessor-name (protoc:message-naming-context-field-accessor-name 
			   message-naming-context))
    (define has-name (protoc:message-naming-context-field-has-name 
		      message-naming-context))

    (append (list ((protoc:message-naming-context-predicate-name 
		    message-naming-context) message)
		  ((protoc:message-naming-context-writer-name
		    message-naming-context) message)
		  ((protoc:message-naming-context-reader-name
		    message-naming-context) message)

		  ((protoc:message-naming-context-extension-accessor-name
		    message-naming-context) message)
		  ((protoc:message-naming-context-extension-has-name
		    message-naming-context) message))

	    (let loop ((fields (protoc:message-definition-fields message))
		       (bindings (list)))
	      (if (null? fields)
		  (reverse bindings)
		  (let ((f (car fields)))
		    (loop (cdr fields)
			  (cons (accessor-name message f)
				(if (eq? (protoc:field-definition-rule f) 
					 'optional)
				    (cons (has-name message f) bindings)
				    bindings))))))))

  (define (protoc:builder-exports message builder-naming-context)
    (define field-accessor-name
      (protoc:builder-naming-context-field-accessor-name 
       builder-naming-context))
    (define field-mutator-name
      (protoc:builder-naming-context-field-mutator-name builder-naming-context))
    (define field-has-name 
      (protoc:builder-naming-context-field-has-name builder-naming-context))
    (define field-clear-name
      (protoc:builder-naming-context-field-clear-name builder-naming-context))

    (append (list ((protoc:builder-naming-context-constructor-name
		    builder-naming-context) message)
		  ((protoc:builder-naming-context-predicate-name
		    builder-naming-context) message)
		  ((protoc:builder-naming-context-build-name
		    builder-naming-context) message)

		  ((protoc:builder-naming-context-extension-accessor-name
		    builder-naming-context) message)
		  ((protoc:builder-naming-context-extension-mutator-name
		    builder-naming-context) message)
		  ((protoc:builder-naming-context-extension-has-name
		    builder-naming-context) message)
		  ((protoc:builder-naming-context-extension-clear-name
		    builder-naming-context) message))

	    (let loop ((fields (protoc:message-definition-fields message))
		       (bindings (list)))
	      (if (null? fields)
		  (reverse bindings)
		  (let ((field (car fields)))
		    (if (eq? (protoc:field-definition-rule field) 'repeated)
			(loop (cdr fields) 
			      (append (list (field-accessor-name message field)
					    (field-mutator-name message field)
					    (field-clear-name message field))
				      bindings))
			(loop (cdr fields)
			      (append (list (field-accessor-name message field)
					    (field-mutator-name message field)
					    (field-has-name message field)
					    (field-clear-name message field))
				      bindings))))))))

  (define (protoc:extension-exports extension extension-naming-context)
    (define extension-name
      (protoc:extension-naming-context-extension-name 
       extension-naming-context))

    (map (lambda (field) (extension-name extension field)) 
	 (protoc:extension-definition-fields extension)))

  (define (protoc:package-exports package naming-context)
    (define enum-naming-context
      (protoc:naming-context-enum-naming-context naming-context))
    (define message-naming-context 
      (protoc:naming-context-message-naming-context naming-context))
    (define builder-naming-context
      (protoc:naming-context-builder-naming-context naming-context))
    (define extension-naming-context
      (protoc:naming-context-extension-naming-context naming-context))

    (define (generate-export definition)
      (cond ((protoc:message-definition? definition)
	     (append 
	      (protoc:message-exports definition message-naming-context)
	      (protoc:builder-exports definition builder-naming-context)
	      (apply append 
		     (map generate-export 
			  (protoc:message-definition-definitions definition)))))
	    ((protoc:enum-definition? definition)
	     (protoc:enum-exports definition enum-naming-context))
	    ((protoc:extension-definition? definition)
	     (protoc:extension-exports definition extension-naming-context))
	    (else '())))

    (let loop ((definitions (protoc:package-definitions package))
	       (output '()))
      (if (or (not definitions) (null? definitions))
	  (reverse output)
	  (let ((definition (car definitions)))
	    (loop (cdr definitions)
		  (append output (generate-export definition)))))))

  (define (protoc:generate-enum enum enum-naming-context)
    (define enum-predicate-name 
      (protoc:enum-naming-context-predicate-name enum-naming-context))
    (define enum-type-name 
      (protoc:enum-naming-context-type-name enum-naming-context))
    (define enum-constructor-name
      (protoc:enum-naming-context-constructor-name enum-naming-context))
    (define enum-value-name
      (protoc:enum-naming-context-value-name enum-naming-context))

    (let-values (((e0 e1) (gensym-values 'e0 'e1)))	  
      (let ((values (map (lambda (value) (enum-value-name enum value))
			 (protoc:enum-definition-values enum))))
	`((define-enumeration ,(enum-type-name enum) 
	    ,values ,(enum-constructor-name enum))

	  (define ,e1 (make-enumeration ,(list 'quote values)))
	  (define (,(enum-predicate-name enum) ,e0)	   
	    (enum-set-member? ,e0 ,e1))))))

  (define (field-less f1 f2)
    (< (protoc:field-definition-ordinal f1)
       (protoc:field-definition-ordinal f2)))

  (define (protoc:generate-message message naming-context)
    (define message-naming-context 
      (protoc:naming-context-message-naming-context naming-context))
    (define builder-naming-context
      (protoc:naming-context-builder-naming-context naming-context))

    (define message-type-name 
      (protoc:message-naming-context-type-name message-naming-context))
    (define builder-constructor-name
      (protoc:builder-naming-context-constructor-name builder-naming-context))
    (define field-accessor-name
      (protoc:message-naming-context-field-accessor-name 
       message-naming-context))
    (define field-has-name
      (protoc:message-naming-context-field-has-name message-naming-context))
    (define extension-accessor-name
      (protoc:message-naming-context-extension-accessor-name 
       message-naming-context))
    (define extension-has-name
      (protoc:message-naming-context-extension-has-name message-naming-context))
    (define message-writer-name
      (protoc:message-naming-context-writer-name message-naming-context))
    (define message-reader-name
      (protoc:message-naming-context-reader-name message-naming-context))

    (define (generate-field-has-predicate message field)
      (let-values (((m0) (gensym-values 'm0)))
	`(define (,(field-has-name message field) ,m0)
	   (protobuf:field-has-value?
	    (protobuf:message-field 
	     ,m0 ,(protoc:field-definition-ordinal field))))))

    (let-values (((e0 e1 w0 w1 r0) (gensym-values 'e0 'e1 'w0 'w1 'r0)))
      `((define-record-type ,(message-type-name message)
	  (fields ,@(let ((fields (list-sort 
				   field-less (protoc:message-definition-fields 
					       message))))
		      (if fields
			  (map (lambda (field) 
				 (list 'immutable
				       (string->symbol
					(protoc:field-definition-name field))
				       (field-accessor-name message field)))
			       fields)
			  '())))
	  (opaque #t)
	  (parent protobuf:message)
	  (sealed #t))

	,@(let loop ((fields (protoc:message-definition-fields message))
		     (bindings (list)))
	    (if (null? fields)
		(reverse bindings)
		(let ((f (car fields)))
		  (if (eq? (protoc:field-definition-rule f) 'optional)
		      (loop (cdr fields)
			    (cons (generate-field-has-predicate message f) 
				  bindings))
		      (loop (cdr fields) bindings)))))
	
	(define (,(extension-accessor-name message) ,e0 ,e1)
	  (protobuf:message-extension ,e0 ,(message-type-name message) ,e1))
	(define (,(extension-has-name message) ,e0 ,e1)
	  (protobuf:message-has-extension? 
	   ,e0 ,(message-type-name message) ,e1))
	(define (,(message-writer-name message) ,w0 ,w1)
	  (protobuf:message-write ,w0 ,w1))
	(define (,(message-reader-name message) ,r0)
	  (protobuf:message-read (,(builder-constructor-name message)) ,r0)))))

  (define (calc-field-default field enum-naming-context)
    (define enum-type-name
      (protoc:enum-naming-context-type-name enum-naming-context))
    (define enum-value-name 
      (protoc:enum-naming-context-value-name enum-naming-context)) 
    (define (find-enum-value enum value-name)
      (find (lambda (value) 
	      (equal? (protoc:enum-value-definition-name value) value-name))
	    (protoc:enum-definition-values enum)))

    (define (option-default? option) 
      (eq? (protoc:option-declaration-name option) 'default))

    (define options (protoc:field-definition-options field))
    (define type-descriptor
      (protoc:type-reference-descriptor (protoc:field-definition-type field)))
      
    (cond ((eq? (protoc:field-definition-rule field) 'repeated) (quote '()))
	  ((and options (find option-default? options)) =>
	   (lambda (option)
	     (let ((value (protoc:option-declaration-value option)))
	       (cond ((protobuf:enum-field-type-descriptor? type-descriptor)
		      (let* ((enum 
			      (protobuf:enum-field-type-descriptor-definition
			       type-descriptor))
			     (enum-value (find-enum-value enum value)))
			(if (not enum-value)
			    (raise (condition 
				   (make-assertion-violation)
				   (make-message-condition 
				    "Incompatible default value"))))
			
			(list (enum-type-name enum)
			      (enum-value-name enum enum-value))))

		     ((protobuf:field-type-descriptor-predicate type-descriptor)
		      value)
		     (list 'quote value)
		     (else (raise (condition 
				   (make-assertion-violation)
				   (make-message-condition 
				    "Incompatible default value"))))))))
	  (else (if (protobuf:enum-field-type-descriptor? type-descriptor)
		    (let* ((enum (protobuf:enum-field-type-descriptor-definition
				  type-descriptor))
			   (value (car (protoc:enum-definition-values enum))))
		      (list (enum-type-name enum) (enum-value-name enum value)))
		    (protobuf:field-type-descriptor-default type-descriptor)))))

  (define (type-reference->type-descriptor-expr type-ref naming-context)
    (define builder-naming-context 
      (protoc:naming-context-builder-naming-context naming-context))
    (define enum-naming-context
      (protoc:naming-context-enum-naming-context naming-context))
    (define message-naming-context 
      (protoc:naming-context-message-naming-context naming-context))

    (define builder-constructor-name
      (protoc:builder-naming-context-constructor-name builder-naming-context))
    (define enum-predicate-name
      (protoc:enum-naming-context-predicate-name enum-naming-context))
    (define message-predicate-name
      (protoc:message-naming-context-predicate-name message-naming-context))

    (define p0 (gensym-values 'p0))
    (define p1 (gensym-values 'p1))

    (define (message-field-type-descriptor-expr descriptor)
      `(protobuf:make-message-field-type-descriptor
	,(protobuf:field-type-descriptor-name descriptor)
	,(list 'quote (protobuf:field-type-descriptor-wire-type descriptor))
	protobuf:write-message
	(lambda (,p0)
	  (protobuf:message-read 
	   (,(builder-constructor-name 
	      (protobuf:message-field-type-descriptor-definition descriptor)))
	   ,p0))
	,(message-predicate-name 
	  (protobuf:message-field-type-descriptor-definition descriptor))
	,(protobuf:field-type-descriptor-default descriptor)))

      (define (enum-field-type-descriptor-expr descriptor)
	(define enum 
	  (protobuf:enum-field-type-descriptor-definition descriptor))
	(define enum-type-name
	  (protoc:enum-naming-context-type-name enum-naming-context))
	(define enum-value-name 
	  (protoc:enum-naming-context-value-name enum-naming-context)) 

	`(protobuf:make-enum-field-type-descriptor
	  ,(protobuf:field-type-descriptor-name descriptor)
	  ,(list 'quote (protobuf:field-type-descriptor-wire-type descriptor))
	  (lambda (,p0 ,p1)
	    (protobuf:write-varint
	     ,p0 (case ,p1
		   ,@(map (lambda (value)
			    `((,(enum-value-name enum value))
			      ,(protoc:enum-value-definition-ordinal value)))
			  (protoc:enum-definition-values enum)))))
	  (lambda (,p0)
	    (case (protobuf:read-varint ,p0)
	      ,@(map (lambda (value)
		       `((,(protoc:enum-value-definition-ordinal value))
			 (,(enum-type-name enum) 
			  ,(enum-value-name enum value))))
		     (protoc:enum-definition-values enum))))
	  ,(enum-predicate-name
	    (protobuf:enum-field-type-descriptor-definition descriptor))
	  ,(protobuf:field-type-descriptor-default descriptor)))

      (let ((descriptor (protoc:type-reference-descriptor type-ref)))	
	(cond
	  ((eq? descriptor protobuf:field-type-double)
	   'protobuf:field-type-double)
	  ((eq? descriptor protobuf:field-type-float) 
	   'protobuf:field-type-float)
	  ((eq? descriptor protobuf:field-type-int32) 
	   'protobuf:field-type-int32)
	  ((eq? descriptor protobuf:field-type-int64) 
	   'protobuf:field-type-int64)
	  ((eq? descriptor protobuf:field-type-uint32) 
	   'protobuf:field-type-uint32)
	  ((eq? descriptor protobuf:field-type-uint64) 
	   'protobuf:field-type-uint64)
	  ((eq? descriptor protobuf:field-type-sint32) 
	   'protobuf:field-type-sint32)
	  ((eq? descriptor protobuf:field-type-sint64) 
	   'protobuf:field-type-sint64)
	  ((eq? descriptor protobuf:field-type-fixed32) 
	   'protobuf:field-type-fixed32)
	  ((eq? descriptor protobuf:field-type-fixed64) 
	   'protobuf:field-type-sfixed32)
	  ((eq? descriptor protobuf:field-type-sfixed32) 
	   'protobuf:field-type-sfixed32)
	  ((eq? descriptor protobuf:field-type-sfixed64) 
	   'protobuf:field-type-sfixed64)
	  ((eq? descriptor protobuf:field-type-bool) 
	   'protobuf:field-type-bool)
	  ((eq? descriptor protobuf:field-type-string) 
	   'protobuf:field-type-string)
	  ((eq? descriptor protobuf:field-type-bytes) 
	   'protobuf:field-type-bytes)      
	  
	  ;; It must be a user-defined type
	  
	  ((protobuf:message-field-type-descriptor? descriptor)
	   (message-field-type-descriptor-expr descriptor))
	  ((protobuf:enum-field-type-descriptor? descriptor)
	   (enum-field-type-descriptor-expr descriptor))
	  (else (raise (make-assertion-violation))))))

  (define (protoc:generate-extension extension naming-context)
    (define builder-naming-context
      (protoc:naming-context-builder-naming-context naming-context))
    (define enum-naming-context
      (protoc:naming-context-enum-naming-context naming-context))
    (define extension-naming-context
      (protoc:naming-context-extension-naming-context naming-context))

    (define builder-constructor-name
      (protoc:builder-naming-context-constructor-name builder-naming-context))
    (define extension-name
      (protoc:extension-naming-context-extension-name extension-naming-context))

    (define (define-extension extension-field)
      `(define ,(extension-name extension extension-field)
	 (protobuf:make-extension-field-descriptor
	  ,(protoc:field-definition-ordinal extension-field)
	  ,(protoc:field-definition-name extension-field)
	  ,(type-reference->type-descriptor-expr
	    (protoc:field-definition-type extension-field) naming-context)
	  ,(eq? (protoc:field-definition-rule extension-field) 'repeated)
	  ,(eq? (protoc:field-definition-rule extension-field) 'required)
	  ,(calc-field-default extension-field enum-naming-context))))

    (define (make-extension-registrar prototype-binding)
      (lambda (extension-field)
	`(protobuf:register-extension
	  ,prototype-binding ,(extension-name extension extension-field))))

    (let ((fields (list-sort 
		   field-less (protoc:extension-definition-fields extension))))
      (append 
       (if (null? fields)
	   '()
	   (let-values (((e0) (gensym-values 'e0)))
	     `((let ((,e0 (,(builder-constructor-name
			     (protobuf:message-field-type-descriptor-definition
			      (protoc:type-reference-descriptor
			       (protoc:extension-definition-target 
				extension)))))))
		 ,@(map (make-extension-registrar e0) fields)))))
       (map define-extension fields))))

  (define (protoc:generate-builder message naming-context)
    (define enum-naming-context
      (protoc:naming-context-enum-naming-context naming-context))
    (define message-naming-context 
      (protoc:naming-context-message-naming-context naming-context))
    (define builder-naming-context 
      (protoc:naming-context-builder-naming-context naming-context))

    (define message-type-name
      (protoc:message-naming-context-type-name message-naming-context))
    (define message-predicate-name
      (protoc:message-naming-context-predicate-name message-naming-context))

    (define builder-type-name
      (protoc:builder-naming-context-type-name builder-naming-context))
    (define builder-constructor-name
      (protoc:builder-naming-context-constructor-name builder-naming-context))
    (define builder-predicate-name
      (protoc:builder-naming-context-predicate-name builder-naming-context))
    (define builder-build-name
      (protoc:builder-naming-context-build-name builder-naming-context))

    (define field-accessor-name
      (protoc:builder-naming-context-field-accessor-name 
       builder-naming-context))
    (define field-mutator-name
      (protoc:builder-naming-context-field-mutator-name builder-naming-context))
    (define field-has-name
      (protoc:builder-naming-context-field-has-name builder-naming-context))
    (define field-clear-name
      (protoc:builder-naming-context-field-clear-name builder-naming-context))
    
    (define extension-accessor-name
      (protoc:builder-naming-context-extension-accessor-name 
       builder-naming-context))
    (define extension-mutator-name
      (protoc:builder-naming-context-extension-mutator-name
       builder-naming-context))
    (define extension-has-name
      (protoc:builder-naming-context-extension-has-name builder-naming-context))
    (define extension-clear-name
      (protoc:builder-naming-context-extension-clear-name
       builder-naming-context))

    (define field-internal-mutators
      (make-hashtable (lambda (f) (protoc:field-definition-ordinal f))
		      (lambda (f1 f2)
			(eqv? (protoc:field-definition-ordinal f1)
			      (protoc:field-definition-ordinal f2)))))

    (define (generate-field-clear message field)
      (let-values (((b0) (gensym-values 'b0)))
	`(define (,(field-clear-name message field) ,b0)
	   (protobuf:clear-field! 
	    (protobuf:message-builder-field
	     ,b0 ,(protoc:field-definition-ordinal field)))
	   (,(hashtable-ref field-internal-mutators field #f) ,b0 
	    ,(calc-field-default field enum-naming-context)))))

    (define (generate-field-has-predicate message field)
      (let-values (((b0) (gensym-values 'b0)))
	`(define (,(field-has-name message field) ,b0)
	   (protobuf:field-has-value?
	    (protobuf:message-builder-field 
	     ,b0 ,(protoc:field-definition-ordinal field))))))

    (define (generate-field-mutator message field)
      (let-values (((b0 b1) (gensym-values 'b0 'b1)))
	`(define (,(field-mutator-name message field) ,b0 ,b1)
	   (protobuf:set-field-value!
	    (protobuf:message-builder-field
	     ,b0 ,(protoc:field-definition-ordinal field)) ,b1)
	   (,(hashtable-ref field-internal-mutators field #f) ,b0 ,b1))))
    
    (let-values (((b0 b1 b2 b3) (gensym-values 'b0 'b1 'b2 'b3)))
      (let ((fields (list-sort field-less
			       (protoc:message-definition-fields message))))
	`((define-record-type (,(builder-type-name message)
			       ,(builder-constructor-name message)
			       ,(builder-predicate-name message))
	    (fields 
	     ,@(map (lambda (field)
		      (let-values (((m0) (gensym-values 'm0)))
			(hashtable-set! field-internal-mutators field m0)
			(let ((name (protoc:field-definition-name field)))
			  (list 'mutable
				(string->symbol name)
				(field-accessor-name message field)
				m0))))
		    fields))
	    (parent protobuf:message-builder)
	    (protocol
	     (lambda (,b1)
	       (lambda ()
		 (define ,b2 
		   (list ,@(map (lambda (field)
				  `(protobuf:make-field-descriptor
				    ,(protoc:field-definition-ordinal field)
				    ,(protoc:field-definition-name field)
				    ,(type-reference->type-descriptor-expr
				      (protoc:field-definition-type field)
				      naming-context)
				    ,(eq? (protoc:field-definition-rule field)
					  'repeated)
				    ,(eq? (protoc:field-definition-rule field)
					  'required)
				    ,(calc-field-default 
				      field enum-naming-context)))
				
				fields)))
		 
		 (let ((,b3 (,b1 ,(message-type-name message) ,b2)))
		   (apply ,b3 (map protobuf:field-descriptor-default ,b2))))))
	    (sealed #t))
	  
	  ,@(let loop ((fields fields)
		       (bindings (list)))
	      (if (null? fields)
		  bindings
		  (let ((f (car fields)))
		    (if (eq? (protoc:field-definition-rule f) 'repeated)
			(loop (cdr fields) 
			      (append (list (generate-field-mutator message f)
					    (generate-field-clear message f))
				      bindings))
			(loop (cdr fields)
			      (append 
			       (list (generate-field-mutator message f)
				     (generate-field-has-predicate message f)
				     (generate-field-clear message f))
			       bindings))))))
	  
	  (define (,(extension-accessor-name message) ,b0 ,b1)
	    (protobuf:message-builder-extension ,b0 ,b1))
	  (define (,(extension-mutator-name message) ,b0 ,b1 ,b2)
	    (protobuf:set-message-builder-extension! ,b0 ,b1 ,b2))
	  (define (,(extension-has-name message) ,b0 ,b1)
	    (protobuf:message-builder-has-extension? ,b0 ,b1))
	  (define (,(extension-clear-name message) ,b0 ,b1)
	    (protobuf:clear-message-builder-extension! ,b0 ,b1))
	  
	  (define (,(builder-build-name message) ,b0)
	    (protobuf:message-builder-build ,b0))))))
)
