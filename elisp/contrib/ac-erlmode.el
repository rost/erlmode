;;; ac-erlmode.el --- auto-complete-mode setup for erlmode

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Setup of auto-complete-mode using erlmode as backend

;; Code:

(require 'auto-complete)

;;;_+ External functions -------------------------------------------------------
(defvar ac-source-external-functions-all
  '((candidates . ac-erlmode-candidates)
    (prefix     . ac-erlmode-looking-at-module-p)
    (symbol     . "e")
    (requires   . 0)
    (cache)))

(defvar ac-source-external-functions
  '((candidates . ac-erlmode-candidates)
    (symbol     . "e")
    (cache)))

;;;_+ Macros -------------------------------------------------------------------
(defvar ac-source-macros-all
  '((candidates . ac-erlmode-candidates)
    (prefix     . ac-erlmode-looking-at-macro-p)
    (symbol     . "m")
    (requires   . 0)
    (cache)))

(defvar ac-source-macros
  '((candidates . ac-erlmode-candidates)
    (symbol     . "m")
    (cache)))

;;;_+ Records ------------------------------------------------------------------
(defvar ac-source-records-all
  '((candidates . ac-erlmode-candidates)
    (prefix     . ac-erlmode-looking-at-record-p)
    (symbol     . "r")
    (requires   . 0)
    (cache)))

(defvar ac-source-records
  '((candidates . ac-erlmode-candidates)
    (symbol     . "r")
    (cache)))

;;;_+ Predicates ---------------------------------------------------------------
(defun ac-erlmode-looking-at-module-p ()
  (ac-erlmode-looking-at-symbol-p ":"))

(defun ac-erlmode-looking-at-macro-p ()
  (ac-erlmode-looking-at-symbol-p "?"))

(defun ac-erlmode-looking-at-record-p ()
  (ac-erlmode-looking-at-symbol-p "#"))

(defun ac-erlmode-looking-at-symbol-p (sym)
  (when (looking-back sym)
    (point)))

;;;_+ Candidates ---------------------------------------------------------------
(defun ac-erlmode-candidates ()
  (erl-candidates))

;;;_+ Setup --------------------------------------------------------------------
(defun ac-erlmode-setup ()
  (setq ac-sources '(ac-source-external-functions-all
                     ac-source-external-functions
                     ac-source-macros-all
                     ac-source-macros
                     ac-source-records-all
                     ac-source-records))
  (auto-complete-mode))

(add-hook 'erlang-mode-hook 'ac-erlmode-setup)

(provide 'ac-erlmode)

;;; ac-erlmode.el ends here
