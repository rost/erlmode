;;; ac-erlmode.el --- auto-complete-mode setup for erlmode

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; Setup of auto-complete-mode using erlmode as backend

;; Code:

(require 'auto-complete)

;;;_+ External functions -------------------------------------------------------
(defvar ac-source-erl-external-functions-all
  '((candidates . erl-candidates)
    (prefix     . ac-erlmode-looking-at-module-p)
    (symbol     . "e")
    (requires   . 0)
    (cache)))

(defvar ac-source-erl-external-functions
  '((candidates . erl-candidates)
    (symbol     . "e")
    (cache)))

(defun ac-erlmode-looking-at-module-p ()
  "Omni-completion predicate for external functions."
  (when (looking-back ":")
    (point)))

;;;_+ Macros -------------------------------------------------------------------
(defvar ac-source-erl-macros-all
  '((candidates . erl-candidates)
    (prefix     . ac-erlmode-looking-at-macro-p)
    (symbol     . "m")
    (requires   . 0)
    (cache)))

(defvar ac-source-erl-macros
  '((candidates . erl-candidates)
    (symbol     . "m")
    (cache)))

(defun ac-erlmode-looking-at-macro-p ()
  "Omni-completion predicate for macros."
  (when (looking-back "?")
    (point)))

;;;_+ Records ------------------------------------------------------------------
(defvar ac-source-erl-records-all
  '((candidates . erl-candidates)
    (prefix     . ac-erlmode-looking-at-record-p)
    (symbol     . "r")
    (requires   . 0)
    (cache)))

(defvar ac-source-erl-records
  '((candidates . erl-candidates)
    (symbol     . "r")
    (cache)))

(defun ac-erlmode-looking-at-record-p ()
  "Omni-completion predicate for records."
  (when (looking-back "#")
    (point)))

;;;_* Modules ------------------------------------------------------------------
(defvar ac-source-erl-modules
  '((candidates . ac-erlmode-module-candidates)
    (symbol     . "M")
    (requires   . 0)))

(defun ac-erlmode-module-candidates ()
  (when (erl--complete-type-generic-p)
    (erl--modules-candidates)))

;;;_+ Internal functions -------------------------------------------------------
(defvar ac-source-erl-local-functions
  '((candidates . ac-erlmode-local-function-candidates)
    (symbol     . "l")
    (requires   . 0)))

(defvar ac-source-erl-imported-functions
  '((candidates . ac-erlmode-imported-function-candidates)
    (symbol     . "i")
    (requires   . 0)))

(defvar ac-source-erl-bif-functions
  '((candidates . ac-erlmode-bif-function-candidates)
    (symbol     . "b")
    (requires   . 0)))

(defun ac-erlmode-local-function-candidates ()
  (when (erl--complete-type-generic-p)
    (let ((filepath (erl--module-name-to-filepath (erl--module-name))))
      (erl--module-internal-functions filepath))))

(defun ac-erlmode-imported-function-candidates ()
  (when (erl--complete-type-generic-p)
    (let ((filepath (erl--module-name-to-filepath (erl--module-name))))
      (erl--module-imported-functions filepath))))

(defun ac-erlmode-bif-function-candidates ()
  (when (erl--complete-type-generic-p)
    (erl--module-internal-bifs)))

;;;_* Language constructs ------------------------------------------------------
(defvar ac-source-erl-keyword
  '((candidates . ac-erlmode-keyword-candidates)
    (symbol     . "k")
    (requires   . 0)))

(defvar ac-source-erl-operator
  '((candidates . ac-erlmode-operator-candidates)
    (symbol     . "o")
    (requires   . 0)))

(defun ac-erlmode-keyword-candidates ()
  (when (erl--complete-type-generic-p)
    (erl--erlang-keywords)))

(defun ac-erlmode-operator-candidates ()
  (when (erl--complete-type-generic-p)
    (erl--erlang-operators)))

;;;_+ Setup --------------------------------------------------------------------
(defun ac-erlmode-setup ()
  (setq ac-sources '(ac-source-erl-external-functions-all
                     ac-source-erl-external-functions
                     ac-source-erl-macros-all
                     ac-source-erl-macros
                     ac-source-erl-records-all
                     ac-source-erl-records
                     ac-source-erl-local-functions
                     ac-source-erl-imported-functions
                     ac-source-erl-bif-functions
                     ac-source-erl-keyword
                     ac-source-erl-operator
                     ac-source-erl-modules))
  (define-key erlang-mode-map (kbd "C-M-i") 'auto-complete)
  (auto-complete-mode))

(add-hook 'erlang-mode-hook 'ac-erlmode-setup)

(provide 'ac-erlmode)

;;; ac-erlmode.el ends here
