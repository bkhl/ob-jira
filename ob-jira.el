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

(defcustom org-babel-jira-command "jira"
  "Name of command to use for executing JiraCLI (https://github.com/ankitpokhrel/jira-cli)."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:jql (body params)
  "Execute a Jira JQL query with Babel."
  (let ((args (org-babel-jira--args body params)))
    (with-temp-buffer
      (let ((result (apply #'call-process org-babel-jira-command nil t nil
                           "issue" "list" "--plain" "--no-headers"
                           "--paginate" (format "%d:%d"
                                                0 (alist-get :limit params 100))
                           args)))
        (when (not (eq result 0))
          (error "error from %s: %s" org-babel-jira-command (buffer-string))))
      (buffer-string))))

(defun org-babel-jira--args (body params)
  `(,@(org-babel-jira--string-args
       params
       '((:config-file-path "--config")
         (:project "--project")
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
    ,@(let ((order (alist-get :order params "ascending")))
        (cond ((member-ignore-case order '("+" "asc" "ascending")) '("--reverse"))
              ((member-ignore-case order '("-" "desc" "descending")) nil)
              (t (error "invalid order value: %s" order))))
    "--jql" ,(org-babel-expand-body:generic body params)))

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
                        (split-string values "," t (rx (one-or-more whitespace)))))))
          args))

(provide 'ob-jira)

;;; ob-jira.el ends here
