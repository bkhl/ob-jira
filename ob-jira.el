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
  (let* ((body (org-babel-expand-body:jql body params))
         (args (org-babel-jira--args body params)))
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
    (cond ((not (and (numberp result) (= result 0))) (error "error from %S:\n%s"
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
         (:order-by "--order-by" "rank")))
    ,@(org-babel-jira--string-array-args
       params
       '((:status "--status")
         (:label "--label")))
    ,@(org-babel-jira--boolean-args
       params
       '((:history "--history")
         (:watching "--watching")))
    ,@(let ((projects (org-babel-jira--split-param (alist-get :project params))))
        (when (= 1 (length projects))
          `("--project" ,(car projects))))
    ,@(let ((columns (org-babel-jira--split-param
                      (upcase
                       (alist-get :columns params "TYPE,KEY,SUMMARY,STATUS")))))
        (when columns `("--columns" ,(string-join columns ","))))
    ,@(let ((order (org-babel-jira--normalize-order (alist-get :order params "ascending"))))
        (when (string= "ASC" order)
          '("--reverse")))
    ,@(unless (string= "" body)
        `("--jql" ,body))))

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
  (let* ((body (string-trim body))
         (body (org-babel-jira--expand-vars body (org-babel--get-vars params)))
         (body (org-babel-jira--expand-project body (alist-get :project params)))
         (body (org-babel-jira--expand-order body params))
         (body (org-babel-expand-body:generic body params)))
    body))

(defun org-babel-jira--expand-vars (body vars)
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward (rx word-start (group-n 1
                                                 (group-n 2
                                                   (one-or-more alpha))
                                                 word-end "()"))
                                nil t)
        (when-let ((value (alist-get (intern (match-string 2)) vars)))
          (replace-match value t t nil 1))))
    (buffer-string)))

(defun org-babel-jira--expand-project (body project)
  (let ((projects (org-babel-jira--split-param project)))
    (if (= 1 (length projects))
        body
      (string-join `(,(if projects
                          (format "project IN (%s)"
                                  (string-join projects ", "))
                        "project IS NOT EMPTY")
                     . ,(unless (string= "" body) `(,(format "(%s)" body))))
                   " AND "))))

(defun org-babel-jira--parse-order-clause (body)
  (when (let ((case-fold-search t))
          (string-match (rx bos
                            (group-n 1
                              (zero-or-more any))
                            (one-or-more space)
                            "order" (one-or-more space)
                            "by" (one-or-more space)
                            (group-n 2
                              (zero-or-more (one-or-more alpha)
                                            (zero-or-more space)
                                            ","
                                            (zero-or-more space))
                              (one-or-more alpha))
                            (one-or-more space)
                            (group-n 3 (or "asc" "desc"))
                            (zero-or-more space)
                            eos)
                        body))
    `((:rest . ,(match-string 1 body))
      (:keys . ,(match-string 2 body))
      (:order . ,(match-string 3 body)))))

(defun org-babel-jira--expand-order (body params)
  "Parse any ORDER BY clause. Will modify `params' in place to apply ordering,
or throw errors if the clause is inconsistent with the corresponding parameters.
The return value is `body' with the ORDER BY clause removed."
  (if-let ((match (org-babel-jira--parse-order-clause body)))
      (let ((rest (string-trim (alist-get :rest match)))
            (clause-keys (string-join
                          (org-babel-jira--split-param (alist-get :keys match))
                          ","))
            (clause-order (upcase (alist-get :order match)))
            (param-keys (when-let ((param (alist-get :order-by params)))
                          (unless (string= "nil" param)
                            (string-join (org-babel-jira--split-param param) ","))))
            (param-order (when-let ((param (alist-get :order params)))
                           (unless (string= "nil" param)
                             (org-babel-jira--normalize-order param)))))
        (if param-keys
            (unless (string-equal-ignore-case clause-keys param-keys)
              (error ":order-by value %S in conflict with ORDER BY keys %S"
                     param-keys
                     clause-keys))
          (push '(:order-by . clause-keys) params))
        (if param-order
            (unless (string= clause-order param-order)
              (error ":order value %S is in conflict with ORDER BY clause"
                     param-keys
                     clause-keys))
          (push '(:order . clause-order) params))
        rest)
    body))

(defun org-babel-jira--normalize-order (order)
  (cond ((member-ignore-case order '("+" "asc" "ascending")) "ASC")
        ((member-ignore-case order '("-" "desc" "descending")) "DESC")
        (t (error "invalid order value: %s" order))))

(defun org-babel-jira--split-param (param)
  (unless (string= param "nil")
    (split-string param "," t (rx (one-or-more whitespace)))))

(provide 'ob-jira)

;;; ob-jira.el ends here
