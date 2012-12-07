;;; erl-parse.el --- Parsing of Erlang forms and functions

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Module for parsing and operating on Erlang forms.

;; Code:

;;;_+ erlang forms =============================================================

;;;_+ collect all forms in module ----------------------------------------------

(defconst erl--module-form-re
  "-\\([_a-z0-9]+\\)\\s *(\\([\"_a-z0-9/\.]+\\|[^.]+\\))"
  "Regular expression to match out the name of a form and the
  value of the form enclosed with the parentheses.")

;;;_+ API ======================================================================

(defun erl-module-forms (module &optional type)
  "Return forms of `module'. Optionally provide `type' of
forms. Type being the canonical symbol of the form name."
  (let* ((forms   (erl--parse-module module))
         (type-re (if type (format "%s*" (symbol-name type)) "[^.]+")))
    (loop for f in forms
          when (string-match-p type-re (first f))
          collect f)))

;;;_+ Internal =================================================================

;;;_+ raw ----------------------------------------------------------------------
(defun erl--file-forms-raw (file)
  "Find all forms in module and return them in a kv list on the
form: `((key1 . val1) (key2 . val2))'.
E.g. `((\"module\" . \"modname\") (\"behaviour\" . \"gen_server\"))'"
  (save-excursion
    (let* ((buffer (get-buffer (file-name-nondirectory file)))
           (live   (buffer-live-p buffer))
           (re     (concat "^-\\(" erlang-atom-regexp "\\)\\s *("))
           (forms  nil))
      (find-file file)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let ((point (point))
              (key   (match-string-no-properties 1))
              (value (progn
                       (while (not (looking-at "("))
                         (backward-char))
                       (erl--thing-at-point-no-properties-op 1))))
          (push (list key value point) forms)))
      (unless live
        (kill-buffer buffer))
      (nreverse forms))))

(defun erl--file-specs-raw (file)
  (let* ((re    "-\\(spec\\|opaque\\|type\\)\\s *\\([^.]+\\)\.")
         (specs nil))
    (with-temp-buffer
      (insert-file-contents file)
      (erlang-mode)
      (while (re-search-forward re nil t)
        (let ((key    (match-string-no-properties 1))
              (value  (match-string-no-properties 2))
              (bounds (point)))
          (push (list key value bounds) specs))))
    (nreverse specs)))

(defun erl--file-functions-raw (file)
  "Compose list of function definitions and their locations in
cells on the form: ((function1 . `point'))."
  (save-excursion
    (let ((live (buffer-live-p (get-buffer (file-name-nondirectory file)))))
      (find-file file)
      (goto-char (point-min))
      (let* ((re (concat "^" erlang-atom-regexp "\\s-*("))
             (functions nil))
        (while (re-search-forward re nil t)
          (backward-char)
          (let* ((module   (erl--module-name))
                 (function (match-string-no-properties 1))
                 (arity    (erl--function-arity))
                 (cell     (list module function arity))
                 (location (save-excursion (beginning-of-line) (point))))
            (push (cons cell location) functions)))
        (unless live
          (kill-buffer (current-buffer)))
        (nreverse functions)))))

(defun erl--module-header-file-paths (file)
  "Return list of filepaths to all header files included by module."
  (save-excursion
    (let ((live (buffer-live-p (get-buffer (file-name-nondirectory file)))))
      (find-file file)
      (goto-char (point-min))
      (let* ((raw (mapcar #'cadr (erl-module-forms (erl--file-name) 'include)))
             (headers (loop for r in raw nconc (last (split-string r "/"))))
             (files        (cdr (erl-modules)))
             (header-paths (loop for file in headers
                                 collect (cdr (assoc file files)))))
        header-paths))))

;;;_* Form parsing -------------------------------------------------------------

(defun erl--parse-module (module)
  (mapcar #'erl--parse-form (erl--file-forms-raw module)))

(defun erl--parse-form (form)
  (let* ((type   (first form))
         (value  (first (butlast (rest form))))
         (point  (first (last  form)))
         (parsed (cond ((null value) nil)
                       ((string= type "export")
                        (erl--parse-export-form value))
                       ((string= type "import")
                        (erl--parse-import-form value))
                       ((string= type "ignore_xref")
                        (erl--parse-ignore-xref-form value))
                       ((string= type "define")
                        (erl--parse-macro-form value))
                       ((string= type "record")
                        (erl--parse-record-form value))
                       ((string= type "include")
                        (erl--parse-include-form value))
                       ((string= type "include_lib")
                        (erl--parse-include-form value))
                       ((string= type "type")
                        (erl--parse-type-form value))
                       (t value))))
    (list type parsed point)))

(defun erl--parse-export-form (form)
  (erl--function-arity-cell-list form))

(defun erl--parse-import-form (form)
  (let* ((mf        (erl--destructure-comma-form form))
         (module    (first mf))
         (functions (erl--function-arity-cell-list (cadr mf))))
    (list module functions)))

(defun erl--parse-ignore-xref-form (form)
  (erl--function-arity-cell-list form))

(defun erl--parse-macro-form (form)
  (erl--destructure-comma-form form))

(defun erl--parse-record-form (form)
  (erl--destructure-comma-form form))

(defun erl--parse-include-form (form)
  "Strip off double-quote marks from path string."
  (substring form 1 (- (length form) 1)))

(defun erl--parse-type-form (form)
  (let* ((name       (first (split-string form "::")))
         (definition (substring form (+ (length name) 2) (length form))))
    (list name definition)))

(defun erl--destructure-comma-form (form)
  "Split a form on the first `,' character."
  (let* ((name       (first (split-string form ",")))
         (definition (substring form (+ (length name) 2) (length form))))
    (list name definition)))

;;;_+ convert string to cell ---------------------------------------------------

(defun erl--function-arity-cell-list (form)
  "Extract list of (function . arity) cells from Erlang form list."
  (let* ((elements (split-string form ","))
         (stripped (mapcar #'erl--extract-function-arity elements))
         (fas (unless (every #'null stripped)
                (mapcar #'erl--function-and-arity-from-entry stripped))))
    fas))

;; BUG breaks on some exports lists
(defun erl--extract-function-arity (string)
  (let* ((stripped nil)
         (re "\\([_a-z0-9]+/[0-9]+\\)"))
    (string-match re (or stripped string))
    (match-string-no-properties 1 string)))

(defun erl--function-and-arity-from-entry (function-arity)
  "Convert Erlang 'function/1' to (function . 1)."
  (let* ((separated (split-string function-arity "/"))
         (function  (first separated))
         (arity     (erlang-string-to-int (first (rest separated))))
         (cell      (cons function arity)))
    cell))

;;;_+ Helpers ------------------------------------------------------------------

(defun erl--thing-at-point-no-properties-op (offset)
  "Wrapper around thing-at-point to extract the content of Erlang
forms. For the form: -form(content) it will return 'content'."
  (let* ((thing (thing-at-point 'sexp))
         (beg   offset)
         (end   (- (length thing) offset)))
    (if (null thing)
        nil ;; instead of failing parsing a module return nil
      (substring-no-properties thing beg end))))

(defun erl--form-bounds ()
  "Find beginning and end `point' of form or function definition."
  (let ((beg (point))
        (end (save-excursion
               (end-of-defun)
               (backward-char)
               (point))))
    (cons beg end)))

(defun erl--end-of-function ()
  (save-excursion
    (erlang-end-of-function)
    (point)))

(defun erl--point-commented-out-p ()
  "Is `point' on line commented out."
  (erl--point-after-syntax-p "%"))

(defun erl--point-after-syntax-p (symbol)
  "Is `point' on line located after `symbol' out."
  (save-excursion
    (let* ((bol     (save-excursion (beginning-of-line) (point)))
           (orig    (point))
           (comment (if (re-search-backward symbol bol t) (point) nil)))
      (when comment
        (> orig comment)))))


(provide 'erl-parse)

;;; erl-parse.el ends here
