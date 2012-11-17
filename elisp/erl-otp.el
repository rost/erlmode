;;; erlmode-otp.el --- OTP specific module

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Module for OTP related aspects of erlmode.
;; Try to automagically figure out where the otp root-dir is located.

;; Code:

(defun erl--otp-root-dir ()
  "Find OTP root dir. Look for true path of `erl' executable to
find the OTP install path."
  (let ((bin (erl--find-erl-executable-op)))
    (if bin
        (let ((dir (file-name-directory (file-truename bin))))
          (file-name-as-directory (expand-file-name (concat dir "..")))))))

(defun erl--otp-man-dir ()
  "Find OTP dir for man pages."
  (let ((basedir (erl--otp-root-dir)))
    (if basedir
        (file-name-as-directory (concat basedir "man")))))

(defun erl--find-erl-executable-op ()
  "Return path of `erl' executable or `nil'. If the executable
can't be found, add $PATH from the user's login shell to exec-path
and retry."
  (let ((bin (executable-find "erl")))
    (if (null bin)
        (let ((path (erl--user-login-shell-path-op)))
          (erl--add-path-string-to-exec-path-op path)
          (executable-find "erl"))
      bin)))

(defun erl--add-path-string-to-exec-path-op (path)
  "SE: Split a $PATH string and add entries to exec-path."
  (let ((paths (split-string path ":")))
    (loop for p in paths
          do (add-to-list 'exec-path p))))

;;;_+ login shell path
(defvar erl--login-shell-rc-files '("~/.profile" "~/.bashrc" "~/.zshrc")
  "Default shell rc files to look for paths in.")

(defun erl--user-login-shell-path-op ()
  "SE: Hackishly get the user's path from the regular login
shell. Depending on how Emacs is started, we might not have
access to the full $PATH."
  (let* ((files erl--login-shell-rc-files)
         (paths (loop for file in files
                      collect (erl--shell-path-for-file-op file)))
         (path  (erl--shell-path-combine paths)))
    path))

(defun erl--shell-path-for-file-op (file)
  "SE: Return login shell $PATH of file."
  (let* ((cmd  (format "source %s &>/dev/null && printf $PATH" file))
         (path (shell-command-to-string cmd)))
    (if (string-equal path "")
        nil
      path)))

(defun erl--shell-path-combine (paths)
  "Combine a list of login shell $PATH strings into a single path
string with duplicates removed."
  (let* ((split    (loop for path in paths
                         nconc (split-string path ":")))
         (filtered (remove-duplicates split :test 'string-equal))
         (combined (mapconcat 'identity filtered ":")))
    combined))

;;;_+ setup --------------------------------------------------------------------
(defun erl--set-otp-root-dir ()
  "SE: Set the root path of the found OTP install."
  (setq erlang-root-dir (erl--otp-root-dir)))

(defun erl--set-otp-man-dir ()
  "SE: Set the man path of the found OTP install."
  (setq erlang-man-root-dir (erl--otp-man-dir)))

(defun erl--setup-otp-dir-paths ()
  "Setup OTP related paths."
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
