;;; -*- lexical-binding: t -*-

;;; ob-dhall.el --- org-babel functions for dhall evaluation

;; Copyright 2018 (C) Christian Fischer

;; Author: Christian Fischer
;; Keywords: literate programming, reproducible research, dhall
;; Homepage: https://github.com/chfi/ob-dhall
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Calls the `dhall` binary to evaluate dhall source blocks.

;;; Requirements:

;; `dhall` installed
;; language be installed as well.



;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("dhall" . "dhall"))

;; optionally declare default header arguments for this language
;; (defvar org-babel-default-header-args:dhall '((:wrap . "SRC dhall")
;;                                               (:tangle . "no"))
(defvar org-babel-default-header-args:dhall '((:results . "output verbatim")))
                                              ;; (:tangle . "no")))

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:dhall' function below.
(defun org-babel-expand-body:dhall (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."

  (let ((vars (org-babel--get-vars params)))
        (concat
         (when (not (null vars))
           (mapconcat
            (lambda (var)
              (concat (format "let %S = " (car var)) (cdr var) "\nin " ))
            vars " "))
         body)))


;; (defun babel-format-dhall (body)
;;     "Format the Dhall code in the BODY string."
;;     (let ((tmp (org-babel-temp-file "dhall")))
;;       (with-temp-file tmp
;;         (do
;;             (insert body)
;;             (org-babel-eval (concat "dhall format") tmp)
;;             ))))


(defun org-babel-execute:dhall (body params)
  "Execute a block of Dhall code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Dhall source code block")
  (let* ((processed-params (org-babel-process-params params))
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:dhall'
         ;; (full-body (org-babel-expand-body:dhall
         ;;             body params processed-params)))
         (full-body (org-babel-expand-body:dhall
                     body params processed-params)))

      (org-babel-eval "dhall" full-body)))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
;; (defun org-babel-prep-session:dhall (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS."
;;   )

(defun org-babel-dhall-var-to-dhall (var)
  "Convert an elisp var into a string of dhall source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-dhall-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

;; (defun org-babel-dhall-initiate-session (&optional session)
;;   "If there is not a current inferior-process-buffer in SESSION then create.
;; Return the initialized session."
;;   (unless (string= session "none")
;;     ))

(provide 'ob-dhall)
;;; ob-dhall.el ends here
