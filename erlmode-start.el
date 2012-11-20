;;; erlmode-start.el --- Major mode for editing and running Erlang

;; -*- coding: utf-8; lexical-binding: t -*-

;; Commentary:
;; A new and in time hopefully better major-mode for editing Erlang.

;; Installation:
;; (add-to-list 'load-path "~/path/to/erlmode-root")
;; (require 'erlmode-start)

;;; Code:

;;;_+ utils --------------------------------------------------------------------

(defun erl--path-of-library (lib)
  (file-truename (file-name-directory (locate-library lib))))

;;;_+ initial setup ------------------------------------------------------------

(add-to-list 'load-path (concat (erl--path-of-library "erlmode-start")
                                "elisp"))

(require 'thingatpt)
(require 'erlmode)

(provide 'erlmode-start)

;;; erlmode-start.el ends here
