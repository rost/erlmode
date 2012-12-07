;;; erlmode-index.el --- Index code base

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Module for project related aspects of erlmode.

;; Try to automagically guess a project root among parent directories of a
;; module. From that guessed project root search the filesystem for erlang
;; files and cache their locations.

;; Cache the modules of the OTP install directory found through the
;; `erl-otp.el' module.

;; Code:

(defvar erl-modules-index-cache nil
  "Cache where we store paths to all files in project.")

(defvar erl-otp-modules-index-cache nil
  "Cache where we store paths to OTP modules.")

(defvar erl-otp-c-modules-index-cache nil
  "Cache where we store paths to OTP C-files.")

;;;_+ API ----------------------------------------------------------------------
(defun erl-modules ()
  "Return list of modules in cache for current project."
  (or (assoc (intern (erl--find-project-root)) erl-modules-index-cache)
      (erl--update-modules-index-cache)))

(defun erl-modules-cache-p ()
  "Do we have a cache for current project."
  (if (cdr (assoc (intern (erl--find-project-root)) erl-modules-index-cache))
      t
    nil))

;;; TODO: This just prepends a new index to the cache list, remove the old
;;; index as well
(defun erl--update-modules-index-cache ()
  "Update module cache for current project."
  (interactive)
  (message "Updating module cache ...")
  (let* ((prj-dir      (erl--find-project-root))
         (key          (intern prj-dir))
         (prj-file-re  "\\(.eterm\\|.app.src\\|.[eh]rl$\\)")
         (otp-dir      (erl--otp-root-dir))
         (otp-file-re  ".[eh]rl$")
         (cache    (nconc (erl-module-index-under-path prj-dir prj-file-re)
                          (erl-module-index-under-path otp-dir otp-file-re))))
    (when (assoc key erl-modules-index-cache)
      (setq erl-modules-index-cache
            (assq-delete-all key erl-modules-index-cache)))
    (message "Done.")
    (setq erl-modules-index-cache
          (cons (cons key cache) erl-modules-index-cache))))

(defun erl-otp-c-modules ()
  "Return list of all indexed OTP C modules."
  (or erl-otp-c-modules-index-cache
      (setq erl-otp-c-modules-index-cache
            (erl-module-index-under-path erlang-root-dir ".[ch]$"))))

(defun erl--module-file-location (module)
  "Return full path of `module'. E.g. `lists.erl' passed in will
result in `/home/user/otp_install/r15/lib/stdlib/src/lists.erl'."
  (cdr (assoc module (erl-modules))))

(defun erl--path-of-module (module)
  "Return full path of `module'."
  (let* ((path (assoc module (erl-modules))))
    (rest path)))

;;;_+ Internal -----------------------------------------------------------------
(defun erl-module-index-under-path (path file-re)
  "Return list of all files matching `file-re' under `path'."
  (let* ((root-dir path)
         (files    (erl--find-files-in-tree root-dir file-re))
         (pairs    (loop for f in files collect (erl--path-to-kv-pair f))))
    pairs))

(defun erl--path-to-kv-pair (path)
  "Convert path to cons cell with filename of the path as key:
`/path/to/module.erl' becomes (`module.erl' . `/path/to/module.erl')"
  (cons (file-name-nondirectory path) path))

;;;_+ filesystem traversal -----------------------------------------------------

(defun erl--find-project-root ()
  "Find project root for current location."
  (let* ((dirs (erl--parents-of-dir default-directory)))
    (or (find-if #'erl--dir-is-project-root dirs)
        (find-if #'erl--dir-is-otp-root dirs))))

(defvar erl-project-root-file-re
  "\\(rebar.config\\|Makefile\\|Emakefile\\|\.gitignore\\)"
  "Regex used to determine if a directory is to be considered a
  project root.")

(defun erl--dir-is-project-root (dir)
  "Determine if `dir' contains a project root file."
  (directory-files dir nil erl-project-root-file-re))

(defun erl--dir-is-otp-root (dir)
  (string-equal erlang-root-dir dir))

;;; TODO: Make it fallback to ../ as project dir if we are standing in an
;;; otp dir layout dir, src/, test/, priv/, etc.
(defun erl--parents-of-dir (start-dir)
  "Construct a list containing `start-dir' and all of its parent
dirs."
  (let* ((real-dir (expand-file-name start-dir))
         (dirs     (loop for s in (cons "/" (split-string real-dir "/" t))
                         collect (file-name-as-directory s)))
         (paths    (loop for d on (reverse dirs) by #'cdr
                         collect (file-name-as-directory
                                  (apply #'concat (reverse d))))))
    (nreverse paths)))

;;; TODO: check if dired will make this faster
(defun erl--find-files-in-tree (directory file-re)
  "Find all files directory tree matching `file-re'."
  (loop for file in (erl--directory-content directory)
        when (and (not (file-directory-p file))
                  (string-match file-re file))
        collect file into files
        when (and (file-directory-p file)
                  ;; needed to partially fix issue when extra symlink in otp
                  ;; install dir shows up. Fix so that issue does not show up
                  ;; instead. But there should be some type of
                  ;; short-circuitry in here somewhere as well.
                  (not (file-symlink-p file)))
        collect file into directories
        finally return
        (nconc files
               (loop for dir in directories
                     nconc (erl--find-files-in-tree dir file-re)))))

(defun erl--directory-content (directory)
  "Return list of content in `directory'."
  (directory-files (file-name-as-directory directory) t "^[^.]" t))

;;;_+ setup --------------------------------------------------------------------
(defun erl--setup-project-module-index ()
  "Create index for project modules unless we already have one."
  (unless (erl-modules-cache-p)
    (erl--update-modules-index-cache)))

;; Setup module index for current project when activating erlang-mode
(add-hook 'erlang-mode-hook 'erl--setup-project-module-index)

(provide 'erl-index)

;;; erlmode-index.el ends here
