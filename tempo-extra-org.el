;;; tempo-extra-org.el --- Tempo templates for org   -*- lexical-binding: t; -*-

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

;; Tempo templates for org

;;; Code:

(require 'tempo-extra)

(defun te-org-changelog-file-p ()
  "Check whether current buffer is associated with changelog file."
  (when-let ((file-name (buffer-file-name)))
    (equal "CHANGELOG.org" (file-name-nondirectory file-name))))

(defun te-org-set-enable-function (hook name enable-function)
  "Set ENABLE-FUNCTION of abbrev with NAME.
HOOK is used to find abbrev table."
  (let* ((abbrev-table (symbol-value (get hook 'abbrev-hook-table)))
         (abbrev (obarray-get abbrev-table name)))
    (abbrev-put abbrev :enable-function enable-function)))

(let* ((name "changelog")
       (hook (te-define name 'org-mode
               '("** [" :changelog-last-version p "] - Unreleased" n n
                 "*** Added" n n
                 "*** Changed" n n
                 "*** Deprecated" n n
                 "*** Removed" n n
                 "*** Fixed" n n
                 "*** Security"))))
  (te-org-set-enable-function hook name #'tempo-extra-org-changelog-file-p))

(let* ((name "Unreleased")
       (hook (te-define name 'org-mode '(:date))))
  (te-org-set-enable-function hook name #'tempo-extra-org-changelog-file-p))

(provide 'tempo-extra-org)

;; Local Variables:
;; read-symbol-shorthands: (("te-" . "tempo-extra-"))
;; End:

;;; tempo-extra-org.el ends here
