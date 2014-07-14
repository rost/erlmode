;;; erl-log.el --- erlmode logging

;;; Commentary:
;; Log functionality modelled after yasnippet logging.

;;; Code:
;; Definitions
(defvar erl-log-level 2
  "Default log level for `erl--message'.")

(defvar erl--log-levels
  '((0 . off)
    (1 . error)
    (2 . warn)
    (3 . info)
    (4 . debug))
  "Log levels of erlmode.")

;; API
(defun erl-log-debug (message)
  "Log MESSAGE on debug level."
  (erl--log 'debug message))

(defun erl-log-info (message)
  "Log MESSAGE on info level."
  (erl--log 'info message))

(defun erl-log-warn (message)
  "Log MESSAGE on warn level."
  (erl--log 'warn message))

(defun erl-log-error (message)
  "Log MESSAGE on error level."
  (erl--log 'error message))

;; Internal
(defun erl--log (level message &rest args)
  "When LEVEL larger than `erl-log-level' log MESSAGE and ARGS."
  (when (> (1+ erl-log-level) (car (rassoc level erl--log-levels)))
    (erl--write-log (format "%s" (apply #'erl--format level message args)))))

(defvar erl--log-file (concat user-emacs-directory "erlmode_debug.log")
  "Location of erlmode log file.")

(defun erl--write-log (message)
  "Write MESSAGE to erlmode default log file."
  (with-temp-buffer
    (insert message "\n")
    (append-to-file (point-min)
                    (point-max)
                    erl--log-file)))

(defun erl--format (level message &rest format-args)
  "Format log message containing LEVEL, MESSAGE and FORMAT-ARGS."
  (apply #'format (format "erl [%s] %s" level (erl--format-crop message))
         format-args))

(defun erl--format-crop (message)
  "Limit MESSAGE to 1000 characters."
  (let ((limit 1000))
    (if (> (length message) limit)
        (format "%s [...]" (substring message 0 999))
      message)))

(defun erl--setup-log ()
  "Add newline to log file on startup."
  (erl--write-log ""))

(erl--setup-log)

(defun erl-show-log ()
  "Open debug log file."
  (interactive)
  (find-file erl--log-file)
  (auto-revert-tail-mode))

(provide 'erl-log)
;;; erl-log.el ends here
