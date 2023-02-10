;;; tempo-extra-js.el --- Tempo templates for js     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Valeriy Litkovskyy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tempo templates for js

;;; Code:

(require 'tempo-extra)

(tempo-extra-define "switch" 'js-mode
  '("switch (" p ") {" n>
    (:while ("Pattern: " pat)
            "case " (s pat) ":" > n>
            p n>
            "break;" n>)
    "default:" > n>
    p n>
    "break;" n>
    "}" >))

(tempo-extra-define "function" 'js-mode
  '("function " p "(" p ") {" n>
    r> n>
    "}" >))

(tempo-extra-define "if" 'js-mode
  '("if (" p ") {" n>
    r> n>
    "}" >))

(tempo-extra-define "for" 'js-mode
  '("for (" p ") {" n>
    r> n>
    "}" >))

(tempo-extra-define "try" 'js-mode
  '("try {" n>
    r> n>
    "} catch (" p "error) {" > n>
    p n>
    "}" >))

(tempo-extra-define "clog" 'js-mode '("console.log(" r ")"))

(tempo-extra-define "ctime" 'js-mode
  '("console.time(\"" (P "Time name: " time) "\");" > n>
    r> n>
    "console.timeEnd(\"" (s time) "\");" >))

(provide 'tempo-extra-js)
;;; tempo-extra-js.el ends here
