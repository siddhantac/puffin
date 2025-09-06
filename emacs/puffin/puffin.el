;;; puffin.el --- Puffin for Emacs: personal finance dashboard  -*- lexical-binding: t; -*-
;; Author: Puffin Contributors
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: finance, tools
;; URL: https://github.com/Abu-Daud/puffin
;; License: MIT

;;; Commentary:
;; Entry point for Puffin in Emacs. Provides `puffin-start` which launches
;; the dashboard using your hledger data and Puffin-style reports.

;;; Code:

(require 'puffin-core)
(require 'puffin-faces)
(require 'puffin-hledger)
(require 'puffin-reports)
(require 'puffin-ui)
(require 'puffin-filter)

;;;###autoload
(defun puffin-start (&optional config-file)
  "Start Puffin dashboard. Optional CONFIG-FILE overrides `puffin-config-file'."
  (interactive)
  (when config-file
    (setq puffin-config-file config-file))
  (puffin--ensure-state)
  (puffin--open-ui)
  (puffin-refresh))

(provide 'puffin)
;;; puffin.el ends here

