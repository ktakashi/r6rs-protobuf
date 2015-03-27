;; pretty-print.sagittarius.scm: pretty printer for Sagittarius
;; Copyright (C) 2015 Takashi Kato

;; SCSS is free software: you can redistribute it and/or modify
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
(library (protobuf compile pretty-print)
    (export protoc:pretty-print)
    (import (rename (only (pp) pp) (pp protoc:pretty-print))))
