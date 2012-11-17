;;; erlmode.el --- Major mode for editing and running Erlang

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; A new and in time hopefully better major-mode for editing Erlang.

;;; Code:

(eval-when-compile (require 'cl))

(require 'erl-helpers)

;;;_* erlmode ==================================================================
;; setup of erlang-mode
(require 'erl-legacy)

;; the regexes defined in here are needed by a couple of modules
(require 'erlang)

;;; otp related setup
(require 'erl-otp)

;;; module location indexing
(require 'erl-index)


(provide 'erlmode)

;;; erlmode.el ends here
