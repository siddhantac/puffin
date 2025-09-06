;;; puffin-faces.el --- Faces and theme management -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Defines Puffin faces and theme switching (Dracula, Nord, Gruvbox).

;;; Code:

(require 'puffin-core)

(defface puffin-header-face '((t :weight bold))
  "Header face." :group 'puffin)
(defface puffin-accent-face '((t :weight bold))
  "Accent for section titles." :group 'puffin)
(defface puffin-dim-face '((t :inherit shadow))
  "Dimmed text." :group 'puffin)
(defface puffin-tab-active-face '((t :weight bold))
  "Active tab face." :group 'puffin)
(defface puffin-tab-inactive-face '((t))
  "Inactive tab face." :group 'puffin)
(defface puffin-border-face '((t))
  "Border/box face." :group 'puffin)
(defface puffin-table-header-face '((t :weight bold))
  "Table header face." :group 'puffin)
(defface puffin-table-selected-face '((t :inverse-video t))
  "Selected row face." :group 'puffin)

(defvar puffin--palette nil)

(defun puffin--palette-for (theme)
  (pcase theme
    ('dracula
     '((bg . "#282a36") (fg . "#f8f8f2") (dim . "#6272a4") (accent . "#ff79c6") (border . "#44475a")
       (selbg . "#bd93f9") (selfg . "#1e1f29")))
    ('nord
     '((bg . "#2e3440") (fg . "#e5e9f0") (dim . "#88c0d0") (accent . "#8fbcbb") (border . "#4c566a")
       (selbg . "#88c0d0") (selfg . "#2e3440")))
    ('gruvbox
     '((bg . "#282828") (fg . "#ebdbb2") (dim . "#928374") (accent . "#fe8019") (border . "#3c3836")
       (selbg . "#b8bb26") (selfg . "#1d2021")))
    (_ '((bg . nil) (fg . nil) (dim . nil) (accent . nil) (border . nil) (selbg . nil) (selfg . nil)))))

(defun puffin--apply-theme (theme)
  (setq puffin--palette (puffin--palette-for theme))
  (let* ((bg (cdr (assq 'bg puffin--palette)))
         (fg (cdr (assq 'fg puffin--palette)))
         (dim (cdr (assq 'dim puffin--palette)))
         (accent (cdr (assq 'accent puffin--palette)))
         (border (cdr (assq 'border puffin--palette)))
         (selbg (cdr (assq 'selbg puffin--palette)))
         (selfg (cdr (assq 'selfg puffin--palette))))
    (custom-set-faces
     `(puffin-header-face ((t (:foreground ,fg :background ,bg :weight bold))))
     `(puffin-accent-face ((t (:foreground ,accent :weight bold))))
     `(puffin-dim-face ((t (:foreground ,dim))))
     `(puffin-tab-active-face ((t (:foreground ,selfg :background ,selbg :weight bold))))
     `(puffin-tab-inactive-face ((t (:foreground ,dim :background ,bg))))
     `(puffin-border-face ((t (:foreground ,border))))
     `(puffin-table-header-face ((t (:foreground ,fg :background ,border :weight bold))))
     `(puffin-table-selected-face ((t (:foreground ,fg :background ,selbg)))))))

;; Initialize theme on load
(eval-after-load 'puffin-core
  '(let* ((st (puffin--ensure-state))
          (theme (puffin--settings-theme (puffin--state-settings st))))
     (puffin--apply-theme theme)))

(provide 'puffin-faces)
;;; puffin-faces.el ends here

