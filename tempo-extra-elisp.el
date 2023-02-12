;;; tempo-extra-elisp.el --- Tempo templates for elisp   -*- lexical-binding: t; -*-

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

;; Tempo templates for elisp

;;; Code:

(require 'tempo-extra-lisp)

(define-abbrev-table (abbrev-hook-abbrev-table 'emacs-lisp-mode)
  nil :enable-function #'te-lisp-enable)

(te-define "defvar" 'emacs-lisp-mode
  '((:with-parens
     "defvar " :elisp-prefix p n>
     r> n>
     "\"" p "\"")))

(te-define "defun" 'emacs-lisp-mode
  '((:with-parens
     "defun " :elisp-prefix p " (" p ")" n>
     "\"" p "\"" n>
     r>)))

(te-define "defgroup" 'emacs-lisp-mode
  '((:with-parens
     "defgroup " :elisp-group " nil" n>
     "\"" p "\"" n>
     ":group " p "nil")))

(te-define "defcustom" 'emacs-lisp-mode
  '((:with-parens
     "defcustom " :elisp-prefix p n>
     r> n>
     "\"" p "\"" n>
     ":type " p "nil" n>
     ":group '" :elisp-group)))

(te-define "defface" 'emacs-lisp-mode
  '((:with-parens
     "defface " :elisp-prefix p n>
     "'((t :inherit " p "nil))" n>
     "\"" p "\"" n>
     ":group '" :elisp-group)))

(te-define "ert-deftest" 'emacs-lisp-mode
  '((:with-parens
     "ert-deftest " :elisp-prefix p " ()" n>
     r>)))

(provide 'tempo-extra-elisp)

;; Local Variables:
;; read-symbol-shorthands: (("te-" . "tempo-extra-"))
;; End:

;;; tempo-extra-elisp.el ends here
