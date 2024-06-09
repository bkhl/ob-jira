;;; ob-jira.el --- Babel Functions for Jira JQL Evaluation -*- lexical-binding: t; -*-

;; Copyright 2024  Björn Lindström <bkhl@elektrubadur.se>

;; Author: Björn Lindström
;; URL: https://git.sr.ht/~bkhl/ob-jira
;; Package-Requires: ((emacs "26.1"))
;; Keywords: literate programming

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating Jira JQL queries.

;;; Code:

(require 'ob)

(defcustom org-babel-jira-command "jira"
  "Name of command to use for executing JiraCLI (https://github.com/ankitpokhrel/jira-cli)."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-jira-page-size 100
  "Number of results to fetch in each call to JiraCLI.
It is limited by JiraCLI to 100."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:jql (body params)
  "Execute a Jira JQL query with Babel."
  (let ((args (org-babel-jira--args body params)))
    (with-temp-buffer
      (org-babel-jira--execute args 0 (alist-get :limit params))
      (buffer-string))))

(defun org-babel-jira--execute (args start limit)
  (let* ((point-start (point))
         (next-start (+ start org-babel-jira-page-size))
         (page-size (if limit
                        (min org-babel-jira-page-size (- limit start))
                      org-babel-jira-page-size))
         (result (apply #'call-process org-babel-jira-command nil t nil
                        `(,@args
                          "--paginate" ,(format "%d:%d" start page-size)))))
    (cond ((not (eq result 0)) (error "error from %S:\n%s"
                                      `(,org-babel-jira-command . ,args)
                                      (string-trim (buffer-substring point-start (point-max)))))
          ((and limit (< limit (line-number-at-pos (point)))))
          ((< (line-number-at-pos (point)) next-start))
          (t (org-babel-jira--execute args next-start limit)))))

(defun org-babel-jira--args (body params)
  `("issue" "list" "--plain" "--no-headers"
    ,@(org-babel-jira--string-args
       params
       '((:config-file-path "--config")
         (:type "--type")
         (:resolution "--resolution")
         (:priority "--priority")
         (:reporter "--reporter")
         (:assignee "--assignee")
         (:component "--component")
         (:parent "--parent")
         (:created "--created")
         (:updated "--updated")
         (:created-after "--created-after")
         (:updated-after "--updated-after")
         (:created-before "--created-before")
         (:updated-before "--updated-before")
         (:order-by "--order-by" "rank")
         (:columns "--columns" "TYPE,KEY,SUMMARY,STATUS")))
    ,@(org-babel-jira--string-array-args
       params
       '((:status "--status")
         (:label "--label")))
    ,@(org-babel-jira--boolean-args
       params
       '((:history "--history")
         (:watching "--watching")))
    ,@(let ((projects (org-babel-jira--split-param (alist-get :project params))))
        (when (eq 1 (length projects))
          `("--project" ,(car projects))))
    ,@(let ((order (alist-get :order params "ascending")))
        (cond ((member-ignore-case order '("+" "asc" "ascending")) '("--reverse"))
              ((member-ignore-case order '("-" "desc" "descending")) nil)
              (t (error "invalid order value: %s" order))))
    "--jql" ,(org-babel-expand-body:jql body params)))

(defun org-babel-jira--string-args (params args)
  (mapcan (lambda (arg)
            (seq-let (key flag default) arg
              (when-let ((value (alist-get key params default)))
                `(,flag ,value))))
          args))

(defun org-babel-jira--boolean-args (params args)
  (mapcan (lambda (arg)
            (seq-let (key flag) arg
              (let ((value (alist-get key params)))
                (cond ((string= value "t") `(,flag))
                      ((string= value "nil") nil)
                      (t (error "invalid %s value: %s" key value))))))
          args))

(defun org-babel-jira--string-array-args (params args)
  (mapcan (lambda (arg)
            (seq-let (key flag) arg
              (when-let ((values (alist-get key params)))
                (mapcan (lambda (value)
                          `(,flag ,value))
                        (org-babel-jira--split-param values)))))
          args))

(defun org-babel-expand-body:jql (body params)
  (let* ((body (org-babel-jira--expand-vars body (org-babel--get-vars params)))
         (body (org-babel-jira--expand-project body (alist-get :project params)))
         (body (org-babel-expand-body:generic body params)))
    body))

(defun org-babel-jira--expand-vars (body vars)
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (rx bow (group (group (one-or-more alpha)) eow "()"))
                                nil t)
        (when-let ((value (alist-get (intern (match-string 2)) vars)))
          (replace-match value t t nil 1))))
    (buffer-string)))

(defun org-babel-jira--expand-project (body project)
  (let ((projects (org-babel-jira--split-param project)))
    (cond ((not projects) (format "project IS NOT EMPTY AND (%s)" body))
          ((< 1 (length projects)) (format "project IN (%s) AND (%s)"
                     (string-join projects ", ")
                     body))
          (t body))))

(defun org-babel-jira--split-param (param)
  (unless (string= param "nil")
    (split-string param "," t (rx (one-or-more whitespace)))))

(provide 'ob-jira)

;;; ob-jira.el ends here
