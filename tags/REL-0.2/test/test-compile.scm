;; test-compile.scm: compiler test routines for r6rs-protobuf
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

(import (rnrs))
(import (protobuf compile))

(define proto 
  (string-append "package org.scheme.r6rs.protobuf;\n"
		 "enum TestEnumType {\n"
		 "FOO = 1;\n"
		 "}"))
  
(protoc:generate-libraries (protoc:read-proto (open-input-string proto)))
