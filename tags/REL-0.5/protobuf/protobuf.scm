;; protobuf.scm: public API for r6rs-protobuf 
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

(library (protobuf protobuf)
  (export protobuf:message? protobuf:message-builder? protobuf:version)
  (import (protobuf private) 
	  (rnrs base))
  
  (define (protobuf:version) "0.5")
)
