;;; company-smart-elisp.el --- contextual code completion in elisp buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: lisp, completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a company backend that considers context. See README for
;; exmaples.

;;; TODO

;; * rx
;; * require
;; * &optional &rest in params
;; * let* using previous arguments
;; * (func) but only when not quoted
;; * PARAMETERS in docstrings
;; * ielm support
;; * setq and setq-local

;; We should also add a tasteful annotation (signature plus docstring)
;; and examples in the readme of using F1.

;;; Code:

;; Local testing.

(require 'cl-lib)
(setq-local company-backends (list #'company-smart-elisp))
;; TODO: this doesn't seem to be respected.
(setq-local company-minimum-prefix-length 2)
(setq company-minimum-prefix-length 2)

(defvar company-smart-elisp--quoted-fns nil)

;; TODO: update cache periodically.
(defun company-smart-elisp--functions (prefix)
  "Return a list of all function whose name starts with PREFIX."
  (unless company-smart-elisp--quoted-fns
    (mapatoms (lambda (sym)
                (when (functionp sym)
                  (push (format "#'%s" sym)
                        company-smart-elisp--quoted-fns)))
              obarray))
  (all-completions prefix company-smart-elisp--quoted-fns))

(defun company-smart-elisp (command &optional arg &rest ignored)
  "Context-aware code completion for Emacs Lisp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-smart-elisp))
    (prefix
     (when (looking-back
            (rx (group "#'" (0+ (or (syntax word) (syntax symbol))))))
       (match-string 1)))
    ;; TODO: only require a match for #'.
    (require-match t)
    (candidates
     (company-smart-elisp--functions arg))))

(provide 'company-smart-elisp)
;;; company-smart-elisp.el ends here
