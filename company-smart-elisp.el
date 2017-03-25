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

(defun company-smart-elisp--libraries (prefix)
  "Return a list of all libraries we might use with `require'."
  (let (res)
    (dolist (dir load-path)
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions prefix dir))
          (when (or (s-ends-with-p ".el.gz" file)
                    (s-ends-with-p ".el" file))
            (let ((filename (s-chop-suffix ".el" (s-chop-suffix ".gz" file))))
              (unless (or (s-ends-with-p "-pkg" filename)
                          (s-ends-with-p "-autoloads" filename)
                          (eq ".dir-locals" filename))
                (push filename res)))))))
    res))

(defun company-smart-elisp--fn-quote-prefix ()
  (when (looking-back
         (rx (group "#'" (0+ (or (syntax word) (syntax symbol))))))
    (match-string 1)))

(defun company-smart-elisp--require-prefix ()
  (interactive)
  ;; TODO: parse rather than a regexp.
  (when (looking-back (rx "(require" (+ (not (any ")")))))
    (match-string 0)))

(defun company-smart-elisp (command &optional arg &rest ignored)
  "Context-aware code completion for Emacs Lisp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-smart-elisp))
    (prefix
     (or
      (company-smart-elisp--fn-quote-prefix)
      (company-smart-elisp--require-prefix)))
    ;; TODO: require a match for #'... and (require #'...) but not for
    ;; (require "...")
    (require-match nil)
    (candidates
     (cond
      ((s-starts-with-p "#'" arg)
       (company-smart-elisp--functions arg))
      ((s-starts-with-p "(require" arg)
       (--map
        (format "require '%s" it)
        (company-smart-elisp--libraries
         (-last-item
          (s-match (rx "(require" (? " ") (? "'") (group (* anything)))
                   arg)))))))))

(provide 'company-smart-elisp)
;;; company-smart-elisp.el ends here
