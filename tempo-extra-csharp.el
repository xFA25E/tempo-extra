;;; tempo-extra-csharp.el --- Templates for csharp   -*- lexical-binding: t; -*-

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

;; Templates for csharp

;;; Code:

(require 'tempo)

(tempo-define-template
 "csharp-PropertyChangedProperty"
 '((P "Type: " type noinsert) (P "Property name: " name noinsert)
   (ignore
    (let ((name (tempo-lookup-named 'name)))
      (tempo-save-named
       'cname
       (if (zerop (length name))
           name
         (concat (capitalize (substring name 0 1))
                 (substring name 1))))))

   (s type) " " (s name) ";" n>
   "public " (s type) " " (s cname) n>
   "{" n>
   (l "get => " (s name) ";" n>
      "set" n>
      "{" n>
      (l "if (!" (s name) ".Equals(value))" n>
         "{" n>
         (l (s name) " = value;" n>
            "PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(nameof(" (s cname) ")));" n>)
         "}" n>)
      "}" n>)
   "}"))

(provide 'tempo-extra-csharp)
;;; tempo-extra-csharp.el ends here
