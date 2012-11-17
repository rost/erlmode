;;; erl-legacy.el --- Major mode for editing and running Erlang

;; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:

;;;_* erlang-mode ==============================================================
(add-to-list 'load-path (concat (erl--path-of-library "erlmode") "erlang-mode"))

(require 'erlang-start)

;; use +debug_info
(setq erlang-compile-extra-opts '(debug_info))

;; turn on some electric commands
(setq erlang-electric-commands '(erlang-electric-comma
                                 erlang-electric-semicolon
                                 erlang-electric-newline))

;; default to two spaces of indentation
(setq erlang-indent-level 2)

;; associate various file extensions with erlang-mode
(add-to-list 'auto-mode-alist '("\\.[ehyx]rl$"      . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.escript?$"      . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$"          . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app$"           . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.appSrc$"        . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app.src$"       . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.rel$"           . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.es$"            . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.eterm$"         . erlang-mode))
(add-to-list 'auto-mode-alist '("\\reltool.config$" . erlang-mode))

;; the rebar config is a list of erlang terms so add it to erlang-mode
(add-to-list 'auto-mode-alist '("rebar\\.config\\'" . erlang-mode))

(provide 'erl-legacy)

;;; erl-legacy.el ends here
