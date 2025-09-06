;;; puffin-ui.el --- UI layout and keybindings -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Creates the Puffin dashboard layout (sidebar + main) and binds keys
;; similar to Puffin V2.

;;; Code:

(require 'puffin-core)
(require 'puffin-faces)
(require 'puffin-hledger)
(require 'puffin-reports)
(require 'puffin-filter)

(defvar puffin-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Navigation in reports list
    (define-key m (kbd "<up>") #'puffin--select-prev)
    (define-key m (kbd "<down>") #'puffin--select-next)
    ;; Refresh
    (define-key m (kbd "r") #'puffin-refresh)
    ;; Filters
    (define-key m (kbd "f") #'puffin-edit-filters)
    (define-key m (kbd "x") #'puffin-reset-filters)
    ;; Settings
    (define-key m (kbd "+") (lambda () (interactive) (puffin-change-account-depth +1) (puffin-refresh)))
    (define-key m (kbd "-") (lambda () (interactive) (puffin-change-account-depth -1) (puffin-refresh)))
    (define-key m (kbd "t") (lambda () (interactive) (puffin-toggle-tree-view) (puffin-refresh)))
    (define-key m (kbd "s") (lambda () (interactive) (puffin-toggle-sort) (puffin-refresh)))
    (define-key m (kbd "T") (lambda () (interactive) (puffin-next-theme) (puffin--render-sidebar)))
    (define-key m (kbd "w") (lambda () (interactive) (puffin-set-period 'weekly) (puffin-refresh)))
    (define-key m (kbd "m") (lambda () (interactive) (puffin-set-period 'monthly) (puffin-refresh)))
    (define-key m (kbd "u") (lambda () (interactive) (puffin-set-period 'quarterly) (puffin-refresh)))
    (define-key m (kbd "y") (lambda () (interactive) (puffin-set-period 'yearly) (puffin-refresh)))
    ;; Graph placeholder toggle
    (define-key m (kbd "g") #'puffin--toggle-graph)
    ;; Quit
    (define-key m (kbd "q") #'puffin-quit)
    m)
  "Keymap for `puffin-mode'.")

(define-derived-mode puffin-mode special-mode "Puffin"
  "Major mode for Puffin dashboard."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq cursor-type nil)
  (use-local-map puffin-mode-map))

(defun puffin--open-ui ()
  (let* ((root (get-buffer-create "*puffin*")))
    (with-current-buffer root
      (puffin-mode)
      (let ((inhibit-read-only t)) (erase-buffer))
      (insert (propertize (format " Puffin for Emacs\n") 'face 'puffin-header-face)))
    (delete-other-windows)
    (switch-to-buffer root)
    (let ((sidebar (get-buffer-create "*puffin-sidebar*"))
          (main (get-buffer-create "*puffin-main*")))
      (display-buffer-in-side-window sidebar '((side . left) (slot . 0) (window-width . 35)))
      (set-window-buffer (get-buffer-window sidebar) sidebar)
      (set-window-buffer (get-buffer-window main t) main)
      (unless (get-buffer-window main t)
        (select-window (split-window-right 35))
        (other-window 1)
        (switch-to-buffer main)
        (other-window -1))
      (with-current-buffer sidebar
        (puffin-mode)
        (setq-local cursor-type nil)
        (setq-local buffer-read-only t))
      (puffin--render-sidebar))))

(defun puffin-quit ()
  (interactive)
  (when-let ((w (get-buffer-window "*puffin*" t)))
    (quit-window t w))
  (dolist (b '("*puffin*" "*puffin-sidebar*" "*puffin-main*"))
    (when (get-buffer b) (kill-buffer b)))
  (message "Puffin closed"))

(defun puffin--select-prev () (interactive) (puffin--set-current-index -1) (puffin--render-sidebar) (puffin-refresh))
(defun puffin--select-next () (interactive) (puffin--set-current-index +1) (puffin--render-sidebar) (puffin-refresh))

(defun puffin--render-sidebar ()
  (let* ((st (puffin--ensure-state))
         (rep (puffin--state-reports st))
         (idx (puffin--state-current-index st))
         (f (puffin--state-filters st))
         (s (puffin--state-settings st))
         (buf (get-buffer-create "*puffin-sidebar*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize " REPORTS\n" 'face 'puffin-accent-face))
        (cl-loop for r in rep for i from 0 do
                 (let ((face (if (= i idx) 'puffin-tab-active-face 'puffin-tab-inactive-face)))
                   (insert (propertize (format " %s\n" (puffin--report-name r)) 'face face))))
        (insert (propertize "\n FILTERS\n" 'face 'puffin-accent-face))
        (insert (format "  account: %s\n" (or (puffin--filters-account f) "-")))
        (insert (format "  start:   %s\n" (or (puffin--filters-start-date f) "-")))
        (insert (format "  end:     %s\n" (or (puffin--filters-end-date f) "-")))
        (insert (format "  desc:    %s\n" (or (puffin--filters-description f) "-")))
        (insert (propertize "\n SETTINGS\n" 'face 'puffin-accent-face))
        (insert (format "  tree:   %s\n" (if (puffin--settings-tree-view s) "on" "off")))
        (insert (format "  depth:  %d\n" (or (puffin--settings-account-depth s) 1)))
        (insert (format "  sort:   %s\n" (if (puffin--settings-sort-amount s) "amt" "acct")))
        (insert (format "  period: %s\n" (pcase (puffin--settings-period s)
                                         ('weekly "W")('monthly "M")('quarterly "Q")('yearly "Y")(_ "-"))))
        (insert (format "  theme:  %s\n" (symbol-name (puffin--settings-theme s))))
        (insert (propertize "\n GRAPHS\n" 'face 'puffin-accent-face))
        (insert (propertize "  g: toggle graph (placeholder)\n" 'face 'puffin-dim-face))))
    (when-let ((w (get-buffer-window buf))) (set-window-buffer w buf))))

(defvar-local puffin--graph-visible nil)
(defun puffin--toggle-graph ()
  (interactive)
  (setq puffin--graph-visible (not puffin--graph-visible))
  (puffin--render-graph))

(defun puffin--render-graph ()
  (let* ((main (get-buffer-create "*puffin-main*")))
    (when-let ((w (get-buffer-window main)))
      (when puffin--graph-visible
        (with-current-buffer main
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n[graph placeholder]\n")))))))

(provide 'puffin-ui)
;;; puffin-ui.el ends here

