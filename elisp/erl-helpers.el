;;; erl-helpers.el --- Helper functions

;;;_+ mfa cell handling --------------------------------------------------------
(defun erl--mfa-cell-to (type cell)
  "Convert mfa cell or string to format specified by `type'."
  (destructuring-bind (m f a)
      (erl--mfa-split (if (listp cell) (erl--mfa-cell-to-string cell) cell))
    (case type
      ('m   (format "%s" m))
      ('mf  (format "%s:%s" m f))
      ('mfa (format "%s:%s/%s" m f a))
      ('f   (format "%s" f))
      ('fa  (format "%s/%s" f a)))))

(defun erl--mfa-string-to-cell (mfa)
  "Convert string `mod:func/1' to (`mod' `func' `1')."
  (destructuring-bind (m f a) (erl--mfa-split mfa)
    (list m f a)))

(defun erl--mfa-cell-to-string (cell)
  "Convert cell (`mod' `func' `1') to `mod:func/1'."
  (destructuring-bind (m f a) cell
    (format "%s:%s/%s" m f a)))

(defun erl--mfa-split (mfa)
  "Split string `mod:func/1' to (`mod' `func' `1')"
  (destructuring-bind (m f a) (split-string mfa ":\\|/")
    (list m f (string-to-int a))))

;;;_+ module handling ----------------------------------------------------------
(defvar erl--file-name-extension-re "\\.[eh]rl$"
  "Regex matching Erlang source file extensions.")

(defun erl--module-name ()
  (file-name-nondirectory (substring-no-properties (buffer-name) 0 -4)))

(defun erl--module-name-trim-extension (module)
  (substring-no-properties module 0 -4))

(defun erl--module-file-name (&optional module)
  (concat (or module (erl--module-name)) ".erl"))

(defun erl--module-name-to-filepath (module)
  (erl--module-file-location (erl--module-file-name module)))

(defun erl--file-name ()
  (file-name-nondirectory (buffer-name)))


(provide 'erl-helpers)

;;; erl-helpers.el ends here
