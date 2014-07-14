;;; erlmode-otp.el --- OTP specific module

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Module for OTP related aspects of erlmode.
;; Try to automagically figure out where the otp root-dir is located.

;; Code:
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
        (erl--add-path-string-to-exec-path (erl--collect-user-shell-paths))
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

(defun erl--add-path-string-to-exec-path (path)
  "SE: Split a $PATH string and add entries to exec-path."
  (let ((paths (split-string path ":")))
    (loop for p in paths
          do (add-to-list 'exec-path p))))

;;;_+ login shell path
(defvar erl--login-shell-rc-files '("~/.profile" "~/.bashrc" "~/.zshrc")
  "Default shell rc files to look for paths in.")

(defun erl--collect-user-shell-paths ()
  "SE: Hackishly get the user's path from the regular login
shell. Depending on how Emacs is started, we might not have
access to the full $PATH."
  (let* ((files erl--login-shell-rc-files)
         (paths (loop for file in files
                      collect (erl--extract-shell-file-path file)))
         (path  (erl--combine-shell-paths paths)))
    path))

(defun erl--extract-shell-file-path (file)
  "SE: Return login shell $PATH of file."
  (let* ((cmd  (format "source %s &>/dev/null && printf $PATH" file))
         (path (shell-command-to-string cmd)))
    (if (string-equal path "")
        nil
      path)))

(defun erl--combine-shell-paths (paths)
  "Combine a list of login shell $PATH strings into a single path
string with duplicates removed."
  (let* ((split    (loop for path in paths
                         nconc (split-string path ":")))
         (filtered (remove-duplicates split :test 'string-equal))
         (combined (mapconcat 'identity filtered ":")))
    combined))

;;;_+ setup --------------------------------------------------------------------
(defun erl--set-otp-erl-bin ()
  "Set OTP `erl' binary path."
  (setq erlang-otp-erl-bin (erl--find-erl-executable)))

(defun erl--set-otp-root-dir ()
  "SE: Set the root path of the found OTP install."
  (setq erlang-otp-root-dir (erl--find-otp-root-dir))
  (setq erlang-root-dir erlang-otp-root-dir))

(defun erl--set-otp-man-dir ()
  "SE: Set the man path of the found OTP install."
  (setq erlang-otp-man-dir (erl--find-otp-man-dir))
  (setq erlang-man-root erlang-otp-man-dir))

(defun erl--setup-otp-dir-paths ()
  "Setup OTP related paths."
  (erl--set-otp-erl-bin)
  (erl--set-otp-root-dir)
  (erl--set-otp-man-dir))

(erl--setup-otp-dir-paths)

;;;_+ unit tests ---------------------------------------------------------------
(ert-deftest erl-shell-path-for-file-op-test ()
  (let ((arg "~/.broken")
        (exp nil))
    (letf ((shell-command-to-string (string) nil))
      (should (eq exp (erl--shell-path-for-file-op arg)))))
  (let ((arg "~/.bashrc")
        (exp "/usr/bin:/usr/sbin"))
    (letf ((shell-command-to-string (string) "/usr/bin:/usr/sbin"))
      (should (string= exp (erl--shell-path-for-file-op arg))))))

(ert-deftest erl-shell-path-combine-test ()
  (let ((arg      (list "/usr/bin:/usr/sbin" "/usr/local/bin:/usr/bin"))
        (expected "/usr/sbin:/usr/local/bin:/usr/bin"))
    (should (string= expected (erl--shell-path-combine arg)))))

(provide 'erl-otp)

;;; erlmode-otp.el ends here
