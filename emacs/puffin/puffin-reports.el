;;; puffin-reports.el --- Render report output as table or pager -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Convert hledger output to Emacs views: tabulated-list for CSV, plain text for others.

;;; Code:

(require 'puffin-core)
(require 'cl-lib)

;; Simple CSV parser that handles quoted fields and commas in quotes
(defun puffin--parse-csv-line (line)
  (let ((i 0) (len (length line)) (field "") (fields '()) (in-quote nil))
    (while (< i len)
      (let ((ch (aref line i)))
        (cond
         ((and (eq ch ?\") (not in-quote)) (setq in-quote t))
         ((and (eq ch ?\") in-quote)
          (if (and (< (1+ i) len) (eq (aref line (1+ i)) ?\"))
              (progn (setq field (concat field "\"")) (setq i (1+ i)))
            (setq in-quote nil)))
         ((and (eq ch ?,) (not in-quote))
          (push field fields) (setq field ""))
         (t (setq field (concat field (string ch))))))
      (setq i (1+ i)))
    (push field fields)
    (nreverse fields)))

(defun puffin--buffer-string (buf)
  (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))

(defun puffin--csv-rows (buf)
  (let* ((s (puffin--buffer-string buf))
         (lines (split-string s "\n" t)))
    (mapcar #'puffin--parse-csv-line lines)))

(defun puffin--render-table (report buf)
  (let* ((rows (puffin--csv-rows buf))
         (hdr (car rows))
         (data (cdr rows))
         (win (get-buffer-window (get-buffer-create "*puffin-main*")))
         (target (get-buffer-create "*puffin-main*")))
    (with-current-buffer target
      (setq buffer-read-only nil)
      (erase-buffer)
      (puffin-table-mode)
      ;; Compute column widths (simple: equal split)
      (let* ((cols (length hdr))
             (fmt (vconcat (mapcar (lambda (h)
                                      (list (substring h 0 (min 30 (length h))) 20 t))
                                    hdr))))
        (setq tabulated-list-format fmt)
        (setq tabulated-list-entries
              (cl-loop for r in data
                       for idx from 0
                       collect (list idx (vconcat (mapcar (lambda (c) (list (substring c 0 (min 120 (length c))) nil)) r)))))
        (tabulated-list-init-header)
        (tabulated-list-print t)))
    (when win (set-window-buffer win target))))

(defun puffin--render-pager (report buf)
  (let* ((win (get-buffer-window (get-buffer-create "*puffin-main*")))
         (target (get-buffer-create "*puffin-main*")))
    (with-current-buffer target
      (setq buffer-read-only nil)
      (erase-buffer)
      (puffin-pager-mode)
      (insert (puffin--buffer-string buf))
      (goto-char (point-min)))
    (when win (set-window-buffer win target))))

(defun puffin--maybe-render (report buf ok)
  (if (not ok)
      (puffin--render-pager report buf)
    (pcase (puffin--report-type report)
      ((or 'balance 'register) (puffin--render-table report buf))
      (_ (puffin--render-pager report buf)))))

;; Minor modes for main buffer views
(define-derived-mode puffin-table-mode tabulated-list-mode "Puffin-Table"
  (setq truncate-lines t)
  (setq-local header-line-format nil)
  (setq-local cursor-type nil)
  (setq-local tabulated-list-padding 1)
  (hl-line-mode 1))

(define-derived-mode puffin-pager-mode special-mode "Puffin-Pager"
  (setq truncate-lines nil))

(provide 'puffin-reports)
;;; puffin-reports.el ends here

