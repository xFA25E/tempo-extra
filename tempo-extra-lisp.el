;;; tempo-extra-lisp.el --- Tempo templates for lisp  -*- lexical-binding: t; -*-

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

;; Tempo templates for Lisp

;;; Code:

(require 'tempo-extra)

(define-abbrev-table (abbrev-hook-abbrev-table 'lisp-mode)
  nil :enable-function #'tempo-extra-lisp-enable)

(tempo-extra-define "lambda" 'lisp-mode
  '((:with-parens "lambda (" p ") " r>)))

(tempo-extra-define "let" 'lisp-mode
  '((:with-parens
     "let ((" p "))" n>
     r>)))

(tempo-extra-define "defvar" 'lisp-mode
  '((:with-parens
     "defvar " p n>
     r> n>
     "\"" p "\"")))

(tempo-extra-define "defun" 'lisp-mode
  '((:with-parens
     "defun " p " (" p ")" n>
     "\"" p "\"" n>
     r>)))

(tempo-extra-define "defpackage" 'lisp-mode
  '((:with-parens
     "defpackage #:" (P "Package name: " package) n>
     "(:use #:cl)" n>
     (:when ("Nickname: " nickname)
            "(:nicknames #:" (s nickname)
            (:while ("Nickname: " nickname) " #:" (s nickname))
            ")" n>)
     (:when ("Local nickname: " local-nickname)
            (:when ("For package: " local-package)
                   "(:local-nicknames (#:" (s local-nickname) " #:" (s local-package) ")"
                   (:while ("Local nickname: " local-nickname)
                           (:when ("For package: " local-package)
                                  " (#:" (s local-nickname) " #:" (s local-package) ")"))
                   ")" n>))
     (:while ("Import from: " import-package)
             (:when ("Import symbol: " import-symbol)
                    "(:import-from #:" (s import-package) " #:" (s import-symbol)
                    (:while ("Import symbol: " import-symbol) " #:" (s import-symbol))
                    ")" n>))
     (:when ("Export: " export)
            "(:export #:" (s export)
            (:while ("Export: " export) " #:" (s export))
            ")" n>)
     "(:documentation \"" (P "Documentation: ") "\"))" n>
     "(in-package #:" (s package) ")" n>)))

(tempo-extra-define "defsystem" 'lisp-mode
  '((:with-parens
     "defsystem \"" (P "System: " system) "\"" n>
     (:when ("Long name: " long-name) ":long-name \"" (s long-name) "\"" n>)
     (:when ("Version: " version) ":version \""  (s version) "\"" n>)
     (:when ("Author: " author) ":author \"" (s author) "\"" n>)
     (:when ("Maintainer: " maintainer) ":maintainer \"" (s maintainer) "\"" n>)
     (:when ("Mailto: " mailto) ":mailto \"" (s mailto) "\"" n>)
     (:when ("License (ex: GPL3): " license) ":license \"" (s license) "\"" n>)
     (:when ("Homepage: " homepage) ":homepage \"" (s homepage) "\"" n>)
     (:when ("Bug tracker: " bug-tracker) ":bug-tracker \"" (s bug-tracker) "\"" n>)
     (:when ("Source control (ex: git): " source-control)
            (:when ("Link: " link) ":source-control (:" (s source-control) " \"" (s link) "\")" n>))
     (:when ("Description: " description) ":description \"" (s description) "\"" n>)
     ":long-description #.(let ((file (probe-file* (subpathname *load-pathname* \"README.md\")))) (when file (read-file-string file)))" n>
     (:when ("Dependency: " dependency)
            ":depends-on (" "\"" (s dependency) "\""
            (:while ("Dependency: " dependency) " \"" (s dependency) "\"")
            ")" n>)
     ":components ((:module \"src\" :components ((:file \"" (s system) "\"))))" n>
     ":in-order-to ((test-op (test-op \"" (s system) "/tests\"))))" n>
     n>
     "(defsystem \"" (s system) "/tests\"" n>
     ":depends-on (\"" (s system) "\" \"fiveam\")" n>
     ":components ((:module \"tests\" :components ((:file \"" (s system) "\"))))" n>
     ":perform (test-op (op c) (symbol-call '#:fiveam '#:run! (find-symbol* '#:" (s system) " '#:" (s system) ".tests)))")))

(provide 'tempo-extra-lisp)
;;; tempo-extra-lisp.el ends here