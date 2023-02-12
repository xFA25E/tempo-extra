;;; tempo-extra.el --- Extra configuration for tempo  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/tempo-extra
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (abbrev-hook "0.0.1"))
;; Keywords: abbrev, convenience

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

;; This package provides tempo templates for various modes.

;;;; Usage and Example Configuration

;; (with-eval-after-load 'csharp-mode (require 'tempo-extra-csharp))
;; (with-eval-after-load 'elisp-mode (require 'tempo-extra-elisp))
;; (with-eval-after-load 'js (require 'tempo-extra-js))
;; (with-eval-after-load 'lisp-mode (require 'tempo-extra-lisp))
;; (with-eval-after-load 'nix-mode (require 'tempo-extra-nix))
;; (with-eval-after-load 'org (require 'tempo-extra-org))

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.

;;;;; Manual

;; Install these required packages:

;; + abbrev-hook

;; Then put this file in your load-path

;;; Code:

;;;; Requirements

(require 'abbrev-hook)
(require 'subr-x)
(require 'tempo)

(defun tempo-extra-define (name mode elements)
  "Define tempo template with abbrev-hook.
NAME is an abbrev name as in `define-abbrev'.  MODE can be a
symbol of some major mode or nil.  If MODE is nil, a global
template will be defined.  ELEMENTS is a tempo template as in
`tempo-define-template'."
  (declare (indent 2))
  (let* ((mode-part (if mode (string-remove-suffix "mode" (symbol-name mode))))
         (tempo-name (concat mode-part name))
         (hook (tempo-define-template tempo-name elements)))
    (cl-callf byte-compile (symbol-function hook))
    (abbrev-hook-define name mode hook)))

(defun tempo-extra-user-elements (element)
  "Additional user elements.
For ELEMENT see `tempo-define-template'."
  (pcase element
    ;; Skeleton constructs
    (`(:if (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) ,then ,else)
     (let ((input (read-from-minibuffer prompt)))
       (if (string-empty-p input)
           else
         (tempo-save-named var input)
         then)))

    (`(:when (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:if (,prompt ,var) (l ,@body) (l)))

    (`(:while (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:when (,prompt ,var) ,@body ,element))

    ;; Lisp
    (`(:with-parens . ,body)
     (if (or (not (eql (char-before) ?\()) (use-region-p))
         `(l "(" ,@body ")")
       `(l ,@body)))

    (:elisp-namespace
     (thread-last (if-let ((bfn (buffer-file-name)))
                      (file-name-nondirectory bfn)
                    (buffer-name))
       downcase
       (string-remove-suffix ".el")
       (replace-regexp-in-string (rx (+ (not (any "a-z")))) "-")
       (replace-regexp-in-string (rx (* "-") eos) "")
       (replace-regexp-in-string (rx bos (* "-")) "")))

    (:elisp-prefix
     (let ((prefix (concat (tempo-extra-user-elements :elisp-namespace) "-")))
       (or (when (bound-and-true-p read-symbol-shorthands)
             (car (cl-find prefix read-symbol-shorthands
                           :key #'cdr :test #'string=)))
           prefix)))

    (:elisp-group
     (thread-last :elisp-namespace
       tempo-extra-user-elements
       (string-remove-suffix "-mode")))

    ;; Nix
    (:nix-hash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=")

    ;; Org
    (:changelog-last-version
     (save-excursion
       (save-match-data
         (goto-char (point-min))
         (search-forward-regexp (rx bol "** [" (group (+ (not (in "]\n")))) "]"))
         (substring-no-properties (match-string 1)))))

    (:date (format-time-string "%Y-%m-%d"))))

(add-hook 'tempo-user-elements #'tempo-extra-user-elements)

;;;; Footer

(provide 'tempo-extra)

;;; tempo-extra.el ends here
