;;; erlmode.el --- Major mode for editing and running Erlang

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; A new and in time hopefully better major-mode for editing Erlang.

;;; Code:

(eval-when-compile (require 'cl))

(require 'erl-helpers)

;; add contrib dir to load-path
(add-to-list 'load-path (concat (erl--path-of-library "erlmode") "contrib"))

;;;_* erlmode ==================================================================
;; setup of erlang-mode
(require 'erl-legacy)

;; the regexes defined in here are needed by a couple of modules
(require 'erlang)

;;; otp related setup
(require 'erl-otp)

;;; module location indexing
(require 'erl-index)

;;; module content parsing
(require 'erl-parse)

;;; source code jumping
(require 'erl-jump)

;;; code completion backend
(require 'erl-complete)
(require 'he-erlmode)

(define-derived-mode erlmode
  erlang-mode "Erlmode"
  "Enhanced mode for Erlang based on Erlang-mode"

  (if (fboundp 'run-mode-hooks)
      (progn
          (run-mode-hooks 'erlmode-mode-hook))
    (run-hooks 'erlmode-mode-hook)))

;;
;; Associate files extensions ".erl" and ".hrl" with erlmode.
;;

(let ((a '("\\.erl\\'" . erlmode))
      (b '("\\.hrl\\'" . erlmode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist)))
  (or (assoc (car b) auto-mode-alist)
      (setq auto-mode-alist (cons b auto-mode-alist))))

;;
;; Associate files using interpreter "escript" with Erlang mode.
;;

(add-to-list 'interpreter-mode-alist (cons "escript" 'erlmode))


(provide 'erlmode)

;;; erlmode.el ends here
