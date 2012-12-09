;;; erl-complete.el --- Completion backend

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Completion backend to be used by various frontends

;; Code:

;;;_+ API ----------------------------------------------------------------------
(defun erl-candidates ()
  "List completion candidates for the current context and point."
  (destructuring-bind (type symbol)
      (erl--complete-type-symbol-before-point)
    (let* ((candidates (erl--complete-candidates type symbol))
           (matches    (all-completions (or (second symbol) "") candidates)))
      matches)))

;;;_+ Internal -----------------------------------------------------------------

;;;_+ Completion type and symbol -----------------------------------------------
(defun erl--complete-type-symbol-before-point ()
  "Determine type of completion before `point'."
  (cond ((erl--complete-type-macro-p)
         (list 'define (erl--complete-type-macro-symbol)))
        ((erl--complete-type-record-p)
         (list 'record (erl--complete-type-record-symbol)))
        ((erl--complete-type-mf-p)
         (list 'external-function (erl--complete-type-mf-symbol)))
        ((erl--complete-type-typedef-p)
         (list 'type))
        (t
         'internal-function)))

(defun erl--complete-type-macro-p ()
  (or (looking-back "?")
      (looking-back (concat "?" erlang-atom-regexp))))

(defun erl--complete-type-record-p ()
  (or (looking-back "#")
      (looking-back (concat "#" erlang-atom-regexp))))

(defun erl--complete-type-mf-p ()
  "Determine if symbol before point is a `mod:func'."
  (or (looking-back (concat erlang-atom-regexp "\\s-*:\\s-*" erlang-atom-regexp))
      (looking-back (concat erlang-atom-regexp "\\s-*:\\s-*"))))

(defun erl--complete-type-mf-symbol ()
  "Return names of module and function under point."
  (let* ((module (if (looking-back ":")
                     (save-excursion
                       (backward-char)
                       (erl--thing-at-point-no-properties-op 0))
                   (save-excursion
                     (beginning-of-thing 'symbol)
                     (when (looking-back ":")
                       (backward-char)
                       (beginning-of-thing 'symbol)
                       (erl--thing-at-point-no-properties-op 0)))))
         (function (erl--thing-at-point-no-properties-op 0)))
    (list module function)))

(defun erl--complete-type-typedef-p ()
  "Determine if a typedef completion is possible."
  (looking-back "::[\\s *]+"))

(defun erl--complete-type-macro-symbol ()
  (erl--complete-symbol))

(defun erl--complete-type-record-symbol ()
  (erl--complete-symbol))

(defun erl--complete-symbol ()
  (list (erl--module-name) (erl--thing-at-point-no-properties-op 0)))

;;;_+ Candidates ---------------------------------------------------------------
(defun erl--complete-candidates (type symbol)
  "List of possible completions for `type'."
  (let ((module (first symbol)))
    (case type
      ('define            (erl--module-macro-names  module))
      ('record            (erl--module-record-names module))
      ('internal-function (erl--module-internal-function-names module))
      ('external-function (erl--module-exported-function-names module)))))

;;;_* Function candidates ------------------------------------------------------

;;;_+ External functions candidates --------------------------------------------
(defun erl--module-exported-function-names (module)
  "List of exported functions in `file'."
  (if (string= "erlang" module)
      (erl--module-external-bifs)
    (let* ((file (erl--module-file-location (erl--module-file-name module)))
           (exports    (erl-module-forms file 'export))
           (functions  (loop for l in exports nconc (second l)))
           (candidates (loop for f in functions collect (first f))))
      (sort candidates 'string-lessp))))

(defun erl--module-external-bifs ()
  "List of external bifs."
  erlang-ext-bifs)

;;;_+ Internal functions candidates --------------------------------------------
(defun erl--module-internal-function-names (module)
  "List of internal (locally defined, imported and internal bifs)
;; function candidates matching `symbol'."
  (let* ((file (erl--module-file-location (erl--module-file-name module)))
         (internal     (erl--module-internal-functions file))
         (imported     (erl--module-imported-functions file))
         (internal-bif (erl--module-internal-bifs))
         (all          (nconc imported internal internal-bif)))
    all))

(defun erl--module-internal-functions (file)
  "List of internal functions in `file'."
  (let* ((functions (mapcar #'first (erl--file-functions-raw file)))
         (trimmed   (loop for f in functions collect (second f))))
    trimmed))

(defun erl--module-imported-functions (file)
  "List of imported functions in `file'."
  (let* ((imports   (erl-module-forms file 'import))
         (functions (loop for i in imports nconc (cadadr i)))
         (trimmed   (mapcar #'car functions)))
    trimmed))

(defun erl--module-internal-bifs ()
  "List of internal bifs."
  erlang-int-bifs)

;;;_+ Macro and record candidates ----------------------------------------------
(defun erl--module-macro-names (module)
  "List of macros in `module'"
  (erl--module-symbol-names module 'define))

(defun erl--module-record-names (module)
  "List of records in `module'"
  (erl--module-symbol-names module 'record))

;;; Pass in `define', `record', etc.
(defun erl--module-symbol-names (module type)
  "List of symbols of `type' in `module' and its included headers."
  (let* ((filepath (erl--module-file-location (erl--module-file-name module)))
         (local    (erl--symbol-names-in-file filepath type))
         (headers  (erl--module-header-file-paths filepath))
         (included (loop for h in headers
                         nconc (erl--symbol-names-in-file h type))))
    (nconc local included)))

(defun erl--symbol-names-in-file (file type)
  "List of symbols of `type' in `file'."
  (let* ((symbols (erl-module-forms file type))
         (names   (loop for s in symbols collect (first (second s)))))
    names))

;;;_+ Module candidates --------------------------------------------------------
(defun erl--modules-candidates ()
  "List of modules in project.")

;;;_+ Variable candidates ------------------------------------------------------

;;;_+ Helpers ------------------------------------------------------------------

(provide 'erl-complete)

;;; erl-complete.el ends here
