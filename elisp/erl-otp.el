;;; erl-otp.el --- OTP specific module

;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; Module for OTP related aspects of erlmode.
;; Try to automagically figure out where the otp root-dir is located.

;;; Code:
;; Definitions
(defvar erlang-otp-erl-bin nil
  "OTP `erl' binary path.")

(defvar erlang-otp-root-dir nil
  "OTP root directory path.")

(defvar erlang-otp-man-dir nil
  "OTP man directory path.")

(defvar erl-login-shell-rc-files '("~/.profile" "~/.bashrc" "~/.zshrc")
  "Default shell rc files to look for paths in.")

;; Functions
(defun erl--find-erl-executable (&optional retry)
  "Find OTP erl executable and RETRY with new paths unless found."
  (if retry
      (executable-find "erl")
    (let ((bin (executable-find "erl")))
      (if bin
          bin
        (erl--add-shell-paths-to-exec-path)
        (erl--find-erl-executable 'retry)))))

(defun erl--find-otp-root-dir ()
  "Find OTP root directory."
  (erl--find-dir 'root))

(defun erl--find-otp-man-dir ()
  "Find OTP man directory."
  (erl--find-dir 'man))

(defun erl--find-dir (dir)
  "Find path or type DIR."
  (let ((erl-bin (or erlang-otp-erl-bin (erl--find-erl-executable))))
    (if (null erl-bin)
        nil
      (case dir
        (root (erl--erl-bin-to-otp-root-path erl-bin))
        (man (erl--erl-bin-to-otp-man-path erl-bin))))))

(defun erl--erl-bin-to-otp-root-path (erl-bin)
  "Translate ERL-BIN location to OTP root dir path."
  (let* ((dir (file-name-directory (file-truename erl-bin)))
         (otp-dir (file-name-as-directory
                   (expand-file-name (concat dir "..")))))
    otp-dir))

(defun erl--erl-bin-to-otp-man-path (erl-bin)
  "Translate ERL-BIN location to OTP man dir path."
  (let* ((otp-dir (erl--erl-bin-to-otp-root-path erl-bin))
         (man-dir (file-name-as-directory
                   (expand-file-name (concat otp-dir "man")))))
    man-dir))

(defun erl--add-shell-paths-to-exec-path ()
  "Add paths from shell config files to `exec-path'."
  (let* ((files erl-login-shell-rc-files)
         (raw (loop for file in files
                    nconc (erl--extract-shell-file-path file)))
         (paths (remove-duplicates raw :test 'string-equal)))
    (loop for path in paths
          do (add-to-list 'exec-path path))))

(defun erl--extract-shell-file-path (file)
  "Extract login shell $PATH for FILE and convert to list of paths."
  (let* ((cmd  (format "source %s &>/dev/null && printf $PATH" file))
         (path (shell-command-to-string cmd)))
    (if (string-equal path "")
        nil
      (split-string path ":"))))

;; setup
(defun erl--set-otp-erl-bin ()
  "Set OTP `erl' binary path."
  (setq erlang-otp-erl-bin (erl--find-erl-executable)))

(defun erl--set-otp-root-dir ()
  "Set the root path of the found OTP install."
  (setq erlang-otp-root-dir (erl--find-otp-root-dir))
  (setq erlang-root-dir erlang-otp-root-dir))

(defun erl--set-otp-man-dir ()
  "Set the man path of the found OTP install."
  (setq erlang-otp-man-dir (erl--find-otp-man-dir))
  (setq erlang-man-root erlang-otp-man-dir))

(defun erl--setup-otp-dir-paths ()
  "Setup OTP related paths."
  (erl--set-otp-erl-bin)
  (erl--set-otp-root-dir)
  (erl--set-otp-man-dir))

(erl--setup-otp-dir-paths)

(provide 'erl-otp)
;;; erl-otp.el ends here
