;; conditions.scm: Condition types for the r6rs-protobuf compiler
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

(library (protobuf compile conditions)
  (export &protoc:location
	  protoc:location-condition?
	  protoc:make-location-condition
	  protoc:location-condition-location

	  &protoc:type-resolution
	  protoc:type-resolution-condition?
	  protoc:make-type-resolution-condition)
  (import (rnrs))

  (define-condition-type &protoc:location &condition 
    protoc:make-location-condition protoc:location-condition? 
    (location protoc:location-condition-location))

  (define-condition-type &protoc:type-resolution &condition
    protoc:make-type-resolution-condition protoc:type-resolution-condition?)
)
