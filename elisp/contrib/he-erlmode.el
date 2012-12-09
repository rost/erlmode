;;; he-erlmode.el --- hippie-expand source using erlmode

;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;; This module provide a completion source for hippie-expand using erlmode
;; completion as backend.

;;; Code:

;;;_+ try functions ------------------------------------------------------------
(defun try-expand-erlmode-complete-external-call (old)
  "Complete Erlang module and function names using erlmode as backend."
  (when (and (looking-at ")") (looking-back "("))
    (backward-char))
  (when (erl--complete-type-mf-p)
    (unless old
      (let* ((end (point))
             (beg (save-excursion (while (not (looking-at ":"))
                                    (backward-char)) (point))))
        (he-init-string beg end))
      (setq he-expand-list (erl-candidates)))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))
    (if (null he-expand-list)
        (progn
          (when old (he-reset-string))
          nil)
      (he-substitute-string (concat ":" (car he-expand-list) "()"))
      (backward-char)
      (setq he-tried-table  (cons (car he-expand-list) (cdr he-tried-table)))
      (setq he-expand-list  (cdr he-expand-list))
      (setq he-num 0)
      t)))

;;;_+ setup --------------------------------------------------------------------
(defun turn-on-hippie-expand-erlmode ()
  "Add erlmode completion functions to `hippie-expand-try-functions-list'"
  (set (make-local-variable 'hippie-expand-try-functions-list)
       hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-expand-erlmode-complete-external-call))

(add-hook 'erlang-mode-hook 'turn-on-hippie-expand-erlmode)

(provide 'he-erlmode)

;;; he-erlmode.el ends here
