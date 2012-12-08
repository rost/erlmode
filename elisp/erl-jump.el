;;; erl-jump.el --- Code navigation

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Module for navigating Erlang code.

;; Code:

;;;_+ code ---------------------------------------------------------------------

(defvar erl--re-function-definition
  (concat "^" erlang-atom-regexp "\\s-*(")
  "Regex for matching function definitions")

(defvar erl--symbol-include-form
  "-include\\(_lib(\\|(\\)\""
  "Regex for matching `-include' and `-include_lib' forms.")

(defvar erl--symbol-record-form
  "\\#"
  "Regex for matching `-record' forms.")

;;;_* jump ring ----------------------------------------------------------------

(defvar erl-jump-ring (make-ring 20)
  "History ring for tracing jumps around the source.")

(defun erl--jump-store-location (buffer-name)
  "Store current buffer and point in jump ring."
  (let ((live (buffer-live-p (get-buffer buffer-name))))
    (ring-insert-at-beginning erl-jump-ring
                              (cons (copy-marker (point-marker)) live))))

(defun erl-jump-back ()
  "Jump back to latest stored location."
  (interactive)
  (unless (ring-empty-p erl-jump-ring)
    (let* ((entry  (ring-remove erl-jump-ring))
           (marker (car entry))
           (live   (cdr entry))
           (buffer (marker-buffer   marker))
           (point  (marker-position marker)))
      (if (not (buffer-live-p buffer))
          (erl--jump-back-from-location)
        (unless live
          (kill-buffer (current-buffer)))
        (switch-to-buffer buffer)
        (goto-char point)))))

;;;_* jumping ==================================================================

(defun erl-jump-keys-hook ()
  (define-key erlang-mode-map (kbd "M-.") 'erl-jump-to-definition)
  (define-key erlang-mode-map (kbd "M-,") 'erl-jump-back))

(add-hook 'erlang-mode-hook 'erl-jump-keys-hook)

(defun erl-jump-to-definition ()
  "Top-level interactive function for jumping to code definitions."
  (interactive)
  (destructuring-bind (type symbol) (erl--symbol-under-point)
    (cond ((or (eq type 'fun-arity-function)
               (eq type 'int-function)
               (eq type 'ext-function)
               (eq type 'imp-function)) (erl--jump-to-function symbol))
          ((eq type 'bif-function) (erl--jump-to-builtin-function symbol))
          ((eq type 'header)       (erl--jump-to-header-file symbol))
          ((eq type 'macro)        (erl--jump-to-macro-definition symbol))
          ((eq type 'record)       (erl--jump-to-record-definition symbol))
          ((eq type 'module)       (erl--jump-to-module symbol))
          ((eq type 'type)         (erl--jump-to-type-definition symbol))
          ('other))))

;;;_* modules ------------------------------------------------------------------

(defun erl--jump-to-module (module)
  "Takes the name of a file `module' and jumps to the file."
  (let* ((file (erl--module-file-location module)))
    (erl--jump-store-location file)
    (find-file file)
    (goto-char (point-min))))

;;;_* functions ----------------------------------------------------------------

(defun erl--jump-to-function (symbol)
  "Takes an mfa cell (`mod' `func' `1') and jumps to the definition."
  (destructuring-bind (module function arity) symbol
    (let* ((filename  (concat module ".erl"))
           (filepath  (erl--module-file-location filename))
           (locations (erl--file-functions-raw filepath))
           (location  (assoc symbol locations)))
      (if (not location)
          (message "Could not locate function definition")
        (erl--jump-store-location filename)
        (find-file filepath)
        (goto-char (cdr location))))))

(defun erl--jump-to-builtin-function ()
  'bif-function)

;;;_* function jump helpers ----------------------------------------------------

(defun erl--function-arity ()
  "Collect the arity of a function."
  (save-excursion
    (while (not (looking-at "\\(/\\|(\\)"))
      (forward-char))
    (cond ((string= "/" (match-string-no-properties 1))
           (forward-char)
           (substring-no-properties (thing-at-point 'char)))
          ((string= "(" (match-string-no-properties 1))
           (erl--parse-list
            (erl--thing-at-point-no-properties-op 1))))))

;; TODO: build function to parse and tokenize lists properly
(defun erl--parse-list (list)
  "Count number of top-level elements in an argument list."
  (number-to-string
   (save-excursion
     (looking-at "[\n\r ]*(")
     (goto-char (match-end 0))
     (let ((res 0)
           (cont t))
       (while cont
         (cond ((eobp)
                (setq res nil) (setq cont nil))
               ((looking-at "\\s *)")
                (setq cont nil))
               ((looking-at "\\s *\\($\\|%\\)")
                (forward-line 1))
               ((looking-at "\\s *,")
                (incf res)
                (goto-char (match-end 0)))
               (t
                (when (zerop res)
                  (incf res))
                (forward-sexp 1))))
       res))))

;;;_+ headers ------------------------------------------------------------------

(defun erl--header-under-point-p ()
  "Check if we are standing on a header include."
  (save-excursion
    (beginning-of-line)
    (looking-at erl--symbol-include-form)))

(defun erl--header-under-point ()
  "Return header path under point."
  (when (erl--header-under-point-p)
    (let* ((path (erl--header-path)))
      (list 'header path))))

(defun erl--header-path ()
  "Return header path from include form."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "(")
    (erl--thing-at-point-no-properties-op 1)))

;;; TODO: handle case where headers are generated during build. I.e. check
;;; Makefile or rebar.config for header filename.
(defun erl--jump-to-header-file (path)
  "Takes the path of an `-include' or `-include_lib' form and
jumps to the file."
  (let* ((header (file-name-nondirectory path))
         (file   (erl--module-file-location header)))
    (if (not file)
        (message "Can't find path for header: %s" header)
      (erl--jump-store-location file)
      (find-file file)
      (goto-char (point-min)))))

;;;_+ macros -------------------------------------------------------------------

(defun erl--jump-to-macro-definition (symbol)
  "Jump to the definition of macro `symbol'."
  (erl--jump-to-definition 'define symbol))

;;;_+ records ------------------------------------------------------------------

(defun erl--jump-to-record-definition (symbol)
  "Jump to the definition of record `symbol'."
  (erl--jump-to-definition 'record symbol))

(defun erl--jump-to-definition (type symbol)
  "Jump to the definition of `symbol' of type `type'."
  (let* ((location (erl--find-symbol-location type symbol (erl--file-name)))
         (file     (car location))
         (point    (cdr location)))
    (if (not location)
        nil
      (erl--jump-store-location file)
      (find-file file)
      (goto-char point))))

;;;_+ symbol lookup ------------------------------------------------------------

;; TODO: make it recursively look through header files
(defun erl--find-symbol-location (type symbol module)
  "Find location of the definition of `symbol' of type `type' in
`module'. The types for now are macros and record.  Searches the
current module and if it can't find the definition there looks
through the include files of the module."
  (let* ((filepath (erl--module-file-location module))
         (internal (erl--find-symbol-in-module type symbol module))
         (external (unless internal
                     (let* ((paths    (erl--module-header-file-paths filepath))
                            (location (erl--find-symbol-in-modules
                                       type symbol paths)))
                       location))))
    (or internal external)))

(defun erl--find-symbol-in-modules (type symbol paths)
  "Find location of `symbol' in `paths' list of files."
  (loop for path in paths
        when   (erl--find-symbol-in-module type symbol path)
        return (erl--find-symbol-in-module type symbol path)))

(defun erl--find-symbol-in-module (type symbol module)
  "Find location of `symbol' in file `path'."
  ;; (let* ((symbols (erl--module-symbols type path))
  (let* ((symbols (erl-module-forms module type))
         (point   (erl--symbol-location symbol symbols)))
    (if point
        (cons module point)
      nil)))

(defun erl--symbol-location (sym symbols)
  "Match `sym' in `symbols' and return location if found."
  (loop for s in symbols
        when (assoc-string sym (second s))
        return (caddr s)))

(defun erl--symbol-name (&optional type)
  "Extract a default symbol when a specific one can't be identified."
  (cond ('other (save-excursion
                  (beginning-of-thing 'symbol)
                  (erl--thing-at-point-no-properties-op 0)))))

;;;_* symbol under point -------------------------------------------------------

;; TODO: this should be able to recognize funs as well
(defun erl--symbol-under-point ()
  "Determine what type of symbol point is on."
  (interactive)
  (cond ((erl--header-under-point))
        ((erl--record-under-point))
        ((erl--macro-under-point))
        ((erl--module-under-point))
        ((erl--function-under-point))))

;; modules
(defun erl--module-under-point ()
  (when (erl--module-under-point-p)
    (list 'module
          (concat (erl--thing-at-point-no-properties-op 0) ".erl"))))

(defun erl--module-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-at (concat erlang-atom-regexp "\\s-*:\\s-*"))))

;; headers
(defun erl--header-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-back (concat erl--symbol-include-form ".*"))))

;; records
(defun erl--record-under-point ()
  (when (erl--record-under-point-p)
    (list 'record (erl--symbol-name))))

(defun erl--record-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (or (looking-back erl--symbol-record-form)
        (looking-back "#[a-z_]+\\."))))

;; macros
;; TODO: Handle case where macro is on the form ?macro(Arg)
(defun erl--macro-under-point ()
  (when (erl--macro-under-point-p)
    (list 'macro (erl--symbol-name))))

(defun erl--macro-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-back "\\?")))

;;;_* function type under point ------------------------------------------------
(defun erl--function-under-point ()
  "Check what type of function we're standing on."
  (cond ((erl--imported-function-under-point))
        ((erl--external-function-under-point))
        ((erl--internal-function-under-point))
        ((erl--fun-arity-function-under-point))
        ((erl--builtin-function-under-point-p) 'bif-function)
        (t 'unrecognized)))

(defun erl--builtin-function-p (function)
  "Is `function' in an internal or external builtin function."
  (or (loop for f in erlang-int-bifs when (string= f function) return t)
      (loop for f in erlang-ext-bifs when (string= f function) return t)))

(defun erl--builtin-function-under-point-p ()
  "Check if function under point is a builtin."
  (save-excursion
    (if (erl--internal-function-under-point-p)
        (erl--builtin-function-p (thing-at-point 'symbol))
      nil)))

(defun erl--imported-function-under-point ()
  (when (erl--imported-function-under-point-p)
    (let* ((function (erl--thing-at-point-no-properties-op 0))
           ;; (imports  (erl--module-imports))
           (imports  (erl-module-forms (erl--file-name) 'import))
           (module   (erl--module-of-imported-function function imports))
           (arity    (erl--function-arity)))
      (list 'imp-function (list module function arity)))))

;;; TODO: make this work for functions inside the list of the import form
(defun erl--imported-function-under-point-p ()
  "Check if function under point is imported."
  (save-excursion
    (if (erl--internal-function-under-point-p)
        (erl--imported-function-p (thing-at-point 'symbol))
      nil)))

(defun erl--imported-function-p (function)
  "Is `function' in an imported function in the current module."
  ;; (let ((imports (erl--module-imports)))
  (let ((imports (erl-module-forms (erl--file-name) 'import)))
    (when (erl--module-of-imported-function function imports)
      t)))

(defun erl--module-of-imported-function (function imports)
  (loop for i in imports
        when (assoc function (second (second i)))
        return (caadr i)))

(defun erl--fun-arity-function-under-point ()
  (when (erl--fun-arity-function-under-point-p)
    (save-excursion
      (beginning-of-thing 'symbol)
      (let* ((filename (erl--file-name))
             (module   (or (when (looking-back ":")
                             (save-excursion
                               (backward-char)
                               (beginning-of-thing 'symbol)
                               (erl--thing-at-point-no-properties-op 0)))
                           (substring-no-properties filename
                                                    0 (- (length filename) 4))))
             (function (erl--thing-at-point-no-properties-op 0))
             (arity    (erl--function-arity)))
        (list 'fun-arity-function (list module function arity))))))

(defun erl--fun-arity-function-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-at (concat erlang-atom-regexp "\\s-*/[0-9]+\\(\n\\|[\n^.]*\\)"))))

(defun erl--internal-function-under-point ()
  (when (erl--internal-function-under-point-p)
    (let* ((filename (buffer-name))
           (module   (substring-no-properties filename 0 (- (length filename) 4)))
           (function (erl--thing-at-point-no-properties-op 0))
           (arity    (erl--function-arity)))
      (list 'int-function (list module function arity)))))

(defun erl--internal-function-under-point-p ()
  "Check if function under point is internal."
  (erl--local-function-under-point-p))

(defun erl--local-function-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-at (concat erlang-atom-regexp "\\s-*("))))

(defun erl--external-function-under-point ()
  (when (erl--external-function-under-point-p)
    (save-excursion
      (beginning-of-thing 'filename)
      (let ((re (format "%s\\s-*:\\s-*%s\\s-*("
                        erlang-atom-regexp
                        erlang-atom-regexp))
            (module   nil)
            (function nil)
            (arity    nil))
        (re-search-forward re nil t)
        (setq module   (match-string-no-properties 1))
        (setq function (match-string-no-properties 2))
        (backward-char)
        (setq arity (erl--function-arity))
        (list 'ext-function (list module function arity))))))

(defun erl--external-function-under-point-p ()
  "Check if function under point is external."
  (save-excursion
    (beginning-of-thing 'symbol)
    (if (not (looking-back ":"))
        nil
      (backward-char)
      (backward-char)
      (beginning-of-thing 'symbol)
      (looking-at (concat erlang-atom-regexp "\\s-*:\\s-*"
                          erlang-atom-regexp "\\s-*(")))))

;;;_* context, conditions ------------------------------------------------------

(defun erl--function-definition-under-point-p ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (looking-at (concat "^" erlang-atom-regexp "\\s *("))))

;;;_* miscellaneous ------------------------------------------------------------
(defun erl-file-candidates (modules)
  (loop for m in modules when (consp m) collect (car m)))

(defun erl-find-project-file ()
  (interactive)
  (let* ((all        (erl-modules))
         (candidates (erl-file-candidates all))
         (candidate  (ido-completing-read "File: " candidates))
         (file       (cdr (assoc candidate all))))
    (find-file file)))

(define-key erlang-mode-map (kbd "C-c C-f") 'erl-find-project-file)


(provide 'erl-jump)

;;; erl-jump.el ends here
