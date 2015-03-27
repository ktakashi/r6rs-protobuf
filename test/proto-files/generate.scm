#!r6rs
(import (rnrs) (protobuf compile) (srfi :13))

(define (generate-libraries file)
  ;; There is no portable way to make directories so
  ;; let shell script handle it.
  ;; (library name) -> library.name
  ;; the shell (or Makefile) will handle this to
  ;; library/name.sls
  (define (->temporary-file name)
    (string-append (string-join (map symbol->string name) ".") ".tmp"))

  (define (dump-library library)
    (let* ((name (cadr library))
	   (out-file (->temporary-file name)))
      (display "Generating test library from ") (display file) (display " to ")
      (display out-file) (newline)
      (when (file-exists? out-file) (delete-file out-file))
      (call-with-output-file out-file
	(lambda (out)
	  (protoc:pretty-print library out)))))
  (let ((in (open-input-file file)))
    (let-values ((libs (protoc:generate-libraries (protoc:read-proto in))))
      (for-each dump-library libs))))

;; hope all implementation has this
(let ((files (cdr (command-line))))
  (for-each generate-libraries files))
  
