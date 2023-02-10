;;; tempo-extra-nix.el --- Tempo templates for nix   -*- lexical-binding: t; -*-

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

;; Tempo templates for nix

;;; Code:

(require 'tempo-extra)

(tempo-extra-define "fetchurl" 'nix-mode
  '("fetchurl {" n>
    "url = \"" p "\";" n>
    "hash = \"" p :nix-hash "\";" n>
    "}" p >))

(tempo-extra-define "fetchzip" 'nix-mode
  '("fetchzip {" n>
    "url = \"" p "\";" n>
    "hash = \"" p :nix-hash "\";" n>
    "}" p >))

(tempo-extra-define "fetchgit" 'nix-mode
  '("fetchgit {" n>
    "url = \"" p "\";" n>
    "rev = \"" p "\";" n>
    "hash = \"" p :nix-hash "\";" n>
    "}" p >))

(tempo-extra-define "fetchFromGitHub" 'nix-mode
  '("fetchFromGitHub {" n>
    "owner = \"" p "\";" n>
    "repo = \"" p "\";" n>
    "rev = \"" p "\";" n>
    "hash = \"" p :nix-hash "\";" n>
    "}" p >))

(provide 'tempo-extra-nix)
;;; tempo-extra-nix.el ends here
