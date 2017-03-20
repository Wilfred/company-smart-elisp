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

;;; Code:

(require 'cl-lib)
(require 'dash)

;; TODO: this is a little slow, cache it.
(defun company-smart-elisp--functions ()
  "Return a list of all functions currently defined."
  (let (symbols)
    (mapatoms (lambda (symbol)
                (when (and (functionp symbol)
                           ;; TODO: why is #'(setf ...) in the obarray?
                           (symbolp symbol))
                  (push symbol symbols))))
    symbols))

(defun company-smart-elisp (command &optional arg &rest ignored)
  "Context-aware code completion for Emacs Lisp."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-smart-elisp))
    (prefix
     (when (looking-back (rx "#'"))
       "#'"))
    (candidates
     (--map (format "#'%s" it) (company-smart-elisp--functions)))))

(provide 'company-smart-elisp)
;;; company-smart-elisp.el ends here
