;;; ob-jira.el --- Babel Functions for Jira JQL Evaluation -*- lexical-binding: t; -*-

;; Copyright 2024  Björn Lindström <bkhl@elektrubadur.se>

;; Author: Björn Lindström
;; URL: https://git.sr.ht/~bkhl/ob-jira
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: literate programming, outlines, comm

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

(defcustom ob-jira-command "jira"
  "Name of command to use for executing JiraCLI (https://github.com/ankitpokhrel/jira-cli)."
  :group 'org-babel
  :type 'string)

(defcustom ob-jira-page-size 100
  "Number of results to fetch in each call to JiraCLI.
It is limited by JiraCLI to 100."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:jql (body params)
  "Execute a Jira JQL query with Babel.

BODY is the content of the source block, i.e. the JQL query.

PARAMS are settings from the source block headers."
  (seq-let (body params) (ob-jira--expand-body body params)
    (let ((args (ob-jira--args body params)))
      (with-temp-buffer
        (ob-jira--execute args 0 (alist-get :limit params))
        (buffer-string)))))

(defun ob-jira--execute (args start limit)
  "Execute JiraCLI with arguments.

ARGS are the command line arguments.  START is the index of the first result to
produce, and LIMIT is the maximum number of results to produce.  The output of
the command is written to the current buffer.  This function is called
recursively if LIMIT exceeds `ob-jira-page-size'."
  (let* ((point-start (point))
         (next-start (+ start ob-jira-page-size))
         (page-size (if limit
                        (min ob-jira-page-size (- limit start))
                      ob-jira-page-size))
         (result (apply #'call-process ob-jira-command nil t nil
                        `(,@args
                          "--paginate" ,(format "%d:%d" start page-size)))))
    (cond ((not (and (numberp result) (= result 0))) (error "error from %S:\n%s"
                                      `(,ob-jira-command . ,args)
                                      (string-trim (buffer-substring point-start (point-max)))))
          ((and limit (< limit (line-number-at-pos (point)))))
          ((< (line-number-at-pos (point)) next-start))
          (t (ob-jira--execute args next-start limit)))))

(defun ob-jira--args (body params)
  "Convert block into JiraCLI command line arguments.

BODY is the JQL query, and PARAMS are the parameters set on the source block."
  `("issue" "list" "--plain" "--no-headers"
    ,@(ob-jira--string-args
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
    ,@(ob-jira--string-array-args
       params
       '((:status "--status")
         (:label "--label")))
    ,@(ob-jira--boolean-args
       params
       '((:history "--history")
         (:watching "--watching")))
    ,@(let ((projects (ob-jira--split-param (alist-get :project params))))
        (when (= 1 (length projects))
          `("--project" ,(car projects))))
    ,@(let ((columns (ob-jira--split-param
                      (upcase
                       (alist-get :columns params "TYPE,KEY,SUMMARY,STATUS")))))
        (when columns `("--columns" ,(string-join columns ","))))
    ,@(let ((order (ob-jira--normalize-order (alist-get :order params "ascending"))))
        (when (string= "ASC" order)
          '("--reverse")))
    ,@(unless (string= "" body)
        `("--jql" ,body))))

(defun ob-jira--string-args (params args)
  "Processs string type arguments.

PARAMS are the parameters provided for the source block, and ARGS is a property
list of parameters and their corresponding JiraCLI command line flag."
  (mapcan (lambda (arg)
            (seq-let (key flag default) arg
              (when-let ((value (alist-get key params default)))
                `(,flag ,value))))
          args))

(defun ob-jira--boolean-args (params args)
  "Process boolean type arguments.

PARAMS are the parameters provided for the source block, and ARGS is a property
list of parameters and their corresponding JiraCLI command line flag."
  (mapcan (lambda (arg)
            (seq-let (key flag) arg
              (let ((value (alist-get key params)))
                (cond ((string= value "t") `(,flag))
                      ((string= value "nil") nil)
                      (t (error "Invalid %s value: %s" key value))))))
          args))

(defun ob-jira--string-array-args (params args)
  "Process string array type arguments.

PARAMS are the parameters provided for the source block, and ARGS is a property
list of parameters and their corresponding JiraCLI command line flag."
  (mapcan (lambda (arg)
            (seq-let (key flag) arg
              (when-let ((values (alist-get key params)))
                (mapcan (lambda (value)
                          `(,flag ,value))
                        (ob-jira--split-param values)))))
          args))

(defun ob-jira--expand-body (body params)
  "Helper function to expand JQL query.

BODY is the JQL query and PARAMS any source block parameters set.  This function
calls other functions that perform various parts of the expansion process."
  (let* ((body (string-trim body))
         (body (ob-jira--expand-vars body (org-babel--get-vars params))))
    (seq-let (body params) (ob-jira--expand-order body params)
      (let* ((body (ob-jira--expand-project body (alist-get :project params)))
             (body (org-babel-expand-body:generic body params)))
        `(,body ,params)))))

(defun ob-jira--expand-vars (body vars)
  "Expand variables in query.

BODY is the JQL query.  If VARS is provided, it will contain variables from
block parameters to be expanded.  They should appear as function calls in the
query, since that is the closest thing to variables present in JQL."
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

(defun ob-jira--expand-project (body project)
  "Modify query to apply :project parameter.

BODY is the JQL query from the source block.

PROJECT is the value of any :project parameters.  If is not provided, modify
query to include `project IS NOT EMPTY', which is how in JiraCLI you can list
tickets from all projects.

Otherwise if the parameter contains multiple projects, modify the query
accordingly.

If there is only one project the query is left unmodified, since we can in that
case use the `--project' command line parameter to JiraCLI instead."
  (let ((projects (ob-jira--split-param project)))
    (if (= 1 (length projects))
        body
      (string-join `(,(if projects
                          (format "project IN (%s)"
                                  (string-join projects ", "))
                        "project IS NOT EMPTY")
                     . ,(unless (string= "" body) `(,(format "(%s)" body))))
                   " AND "))))

(defun ob-jira--parse-order-clause (body)
  "Parse out ORDER clause from BODY, if present."
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

(defun ob-jira--expand-order (body params)
  "Parse any ORDER BY clause.

Will modify PARAMS to apply ordering, or throw errors if the clause is
inconsistent with the corresponding parameters.  The return value is BODY with
the ORDER BY clause removed."
  (if-let ((match (ob-jira--parse-order-clause body)))
      (let ((rest (string-trim (alist-get :rest match)))
            (clause-keys (string-join
                          (ob-jira--split-param (alist-get :keys match))
                          ","))
            (clause-order (upcase (alist-get :order match)))
            (param-keys (when-let ((param (alist-get :order-by params)))
                          (unless (string= "nil" param)
                            (string-join (ob-jira--split-param param) ","))))
            (param-order (when-let ((param (alist-get :order params)))
                           (unless (string= "nil" param)
                             (ob-jira--normalize-order param)))))
        (if param-keys
            (unless (string-equal-ignore-case clause-keys param-keys)
              (error ":order-by value %S in conflict with ORDER BY keys %S"
                     param-keys
                     clause-keys))
          (push `(:order-by . ,clause-keys) params))
        (if param-order
            (unless (string= clause-order param-order)
              (error ":order value %S is in conflict with ORDER BY clause"
                     param-keys
                     clause-keys))
          (push `(:order . ,clause-order) params))
        `(,rest ,params))
    `(,body ,params)))

(defun ob-jira--normalize-order (order)
  "ORDER is a value set for the :order parameter.

Normalize it into ASC or DESC."
  (cond ((member-ignore-case order '("+" "asc" "ascending")) "ASC")
        ((member-ignore-case order '("-" "desc" "descending")) "DESC")
        (t (error "Invalid order value: %s" order))))

(defun ob-jira--split-param (param)
  "Split comma-separated PARAM values into a list of values."
  (unless (string= param "nil")
    (split-string param "," t (rx (one-or-more whitespace)))))

(provide 'ob-jira)

;;; ob-jira.el ends here
