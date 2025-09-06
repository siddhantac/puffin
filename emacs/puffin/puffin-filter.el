;;; puffin-filter.el --- Filters UX for Puffin -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Provides simple minibuffer-based filter editing for account/start/end/description
;; and a separate command for register-specific filtering.

;;; Code:

(require 'puffin-core)

(defun puffin-edit-filters ()
  "Edit filters via minibuffer prompts and refresh."
  (interactive)
  (let* ((st (puffin--ensure-state))
         (f (puffin--state-filters st))
         (account (read-string "Account (acct:): " (or (puffin--filters-account f) "")))
         (start (read-string "Start date (YYYY[-MM[-DD]]): " (or (puffin--filters-start-date f) "")))
         (end   (read-string "End date (YYYY[-MM[-DD]]): " (or (puffin--filters-end-date f) "")))
         (desc  (read-string "Description contains: " (or (puffin--filters-description f) ""))))
    (setf (puffin--filters-account f) (unless (string-empty-p account) account)
          (puffin--filters-start-date f) (unless (string-empty-p start) start)
          (puffin--filters-end-date f) (unless (string-empty-p end) end)
          (puffin--filters-description f) (unless (string-empty-p desc) desc))
    (puffin-refresh)))

(defun puffin-edit-register-filter ()
  "Edit register table inline filter (applies to register report only)."
  (interactive)
  (let* ((st (puffin--ensure-state))
         (f (puffin--state-filters st))
         (rf (read-string "Register filter: " (or (puffin--filters-register-filter f) ""))))
    (setf (puffin--filters-register-filter f) (unless (string-empty-p rf) rf))
    (puffin-refresh)))

(provide 'puffin-filter)
;;; puffin-filter.el ends here

