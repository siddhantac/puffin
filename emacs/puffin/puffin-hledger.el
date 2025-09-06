;;; puffin-hledger.el --- hledger integration and async processes -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Build hledger option args from current filters/settings and run commands asynchronously.

;;; Code:

(require 'puffin-core)

(defun puffin--opt-list (_report)
  "Return a minimal set of options for hledger. Placeholder to ensure MELPA-ready compilation."
  '())

(defun puffin--report-command-args (report)
  "Split REPORT's command string into a list of args."
  (split-string (puffin--report-cmd report) "[ \t]+" t))

(defun puffin--buffer-name (report)
  (format "*puffin:%s*" (puffin--report-name report)))

(defun puffin--run-report (report &optional cb)
  "Run REPORT command asynchronously and call CB with the result plist."
  (let* ((name (format "puffin-hledger:%s" (puffin--report-name report)))
         (args (puffin--report-command-args report))
         (buf (get-buffer-create (puffin--buffer-name report)))
         (proc (make-process :name name :buffer buf :command args :connection-type 'pipe)))
    (with-current-buffer buf (erase-buffer))
    (set-process-sentinel
     proc
     (lambda (p _ev)
       (when (memq (process-status p) '(exit signal))
         (let ((ok (= 0 (process-exit-status p))))
           (when cb (funcall cb (list :report report :ok ok :buf (process-buffer p) :cmd args)))))))
    proc))

(defun puffin-refresh ()
  "Refresh all reports (run hledger for each)."
  (interactive)
  (let* ((st (puffin--ensure-state))
         (reports (puffin--state-reports st)))
    (dolist (r reports)
      (puffin--run-report r #'puffin--on-report-done))))

(defun puffin--on-report-done (_result)
  "Handle completion of a report. Placeholder for now."
  (ignore))

(provide 'puffin-hledger)
;;; puffin-hledger.el ends here

