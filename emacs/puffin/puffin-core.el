;;; puffin-core.el --- Core state, config, and utilities -*- lexical-binding: t; -*-
;; License: MIT

;;; Commentary:
;; Core data structures and config loader for Puffin Emacs package.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)

(defgroup puffin nil
  "Puffin finance dashboard."
  :group 'tools)

(defcustom puffin-config-file
  (let* ((root (locate-dominating-file default-directory "config.json")))
    (when root (expand-file-name "config.json" root)))
  "Path to Puffin v2-style config.json. If nil, defaults are used."
  :type '(file :must-match t)
  :group 'puffin)

(cl-defstruct puffin--report
  name cmd locked type) ;; type: 'balance 'register 'accounts 'unknown

(cl-defstruct puffin--filters
  account start-date end-date description register-filter)

(cl-defstruct puffin--settings
  tree-view account-depth sort-amount period theme) ; period: 'weekly 'monthly 'quarterly 'yearly; theme: 'dracula 'nord 'gruvbox

(cl-defstruct puffin--state
  reports current-index filters settings buffers running)

(defvar puffin--state nil)

(defun puffin--ensure-state ()
  (unless puffin--state
    (setq puffin--state
          (make-puffin--state
           :reports (puffin--load-reports)
           :current-index 0
           :filters (make-puffin--filters)
           :settings (make-puffin--settings
                      :tree-view t
                      :account-depth 3
                      :sort-amount nil
                      :period 'yearly
                      :theme 'dracula)
           :buffers (make-hash-table :test 'equal)
           :running (make-hash-table :test 'equal))))
  puffin--state)

(defun puffin--default-reports ()
  (list
   (make-puffin--report :name "register" :cmd "hledger register" :locked nil :type 'register)
   (make-puffin--report :name "expenses" :cmd "hledger balance type:x" :locked nil :type 'balance)
   (make-puffin--report :name "assets" :cmd "hledger balance type:a" :locked nil :type 'balance)
   (make-puffin--report :name "revenue" :cmd "hledger balance type:r" :locked nil :type 'balance)
   (make-puffin--report :name "liabilities" :cmd "hledger balance type:l" :locked nil :type 'balance)
   (make-puffin--report :name "income statement" :cmd "hledger incomestatement" :locked nil :type 'unknown)
   (make-puffin--report :name "balance sheet" :cmd "hledger bs" :locked nil :type 'unknown)
   (make-puffin--report :name "accounts" :cmd "hledger accounts --tree" :locked nil :type 'accounts)))

(defun puffin--infer-type (cmd)
  (let* ((parts (split-string cmd "[ \t]+" t))
         (sub (cadr parts)))
    (pcase sub
      ((or "balance" "bal") 'balance)
      ((or "register" "reg") 'register)
      ((or "accounts" "acc") 'accounts)
      (_ 'unknown))))

(defun puffin--load-reports ()
  (condition-case _
      (when (and puffin-config-file (file-readable-p puffin-config-file))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (json-key-type 'string)
               (cfg (json-parse-file puffin-config-file :object-type 'alist :array-type 'list))
               (reports (alist-get "reports" cfg)))
          (if (and reports (listp reports))
              (mapcar (lambda (r)
                        (let* ((name (alist-get "name" r))
                               (cmd (alist-get "cmd" r))
                               (locked (and (alist-get "locked" r) t)))
                          (make-puffin--report :name name
                                               :cmd cmd
                                               :locked locked
                                               :type (puffin--infer-type (or cmd "")))))
                      reports)
            (puffin--default-reports))))
    (error (puffin--default-reports)))
  ;; fallback
  (or (ignore-errors (puffin--default-reports)) (puffin--default-reports)))

(defun puffin--current-report ()
  (let* ((st (puffin--ensure-state))
         (idx (puffin--state-current-index st)))
    (nth idx (puffin--state-reports st))))

(defun puffin--set-current-index (delta)
  (let* ((st (puffin--ensure-state))
         (len (length (puffin--state-reports st)))
         (new (mod (+ (puffin--state-current-index st) delta) len)))
    (setf (puffin--state-current-index st) new)
    new))

(defun puffin-reset-filters ()
  (interactive)
  (let ((st (puffin--ensure-state)))
    (setf (puffin--filters-account (puffin--state-filters st)) nil
          (puffin--filters-start-date (puffin--state-filters st)) nil
          (puffin--filters-end-date (puffin--state-filters st)) nil
          (puffin--filters-description (puffin--state-filters st)) nil
          (puffin--filters-register-filter (puffin--state-filters st)) nil)))

(defun puffin-toggle-tree-view ()
  (interactive)
  (let* ((st (puffin--ensure-state))
         (s (puffin--state-settings st)))
    (setf (puffin--settings-tree-view s) (not (puffin--settings-tree-view s)))))

(defun puffin-toggle-sort ()
  (interactive)
  (let* ((st (puffin--ensure-state))
         (s (puffin--state-settings st)))
    (setf (puffin--settings-sort-amount s) (not (puffin--settings-sort-amount s)))))

(defun puffin-change-account-depth (delta)
  (interactive)
  (let* ((st (puffin--ensure-state))
         (s (puffin--state-settings st))
         (d (max 1 (+ (or (puffin--settings-account-depth s) 1) delta))))
    (setf (puffin--settings-account-depth s) d)))

(defun puffin-set-period (p)
  (interactive)
  (let* ((st (puffin--ensure-state)))
    (setf (puffin--settings-period (puffin--state-settings st)) p)))

(defun puffin-next-theme ()
  (interactive)
  (let* ((st (puffin--ensure-state))
         (s (puffin--state-settings st))
         (cycle '(dracula gruvbox nord))
         (cur (or (puffin--settings-theme s) 'dracula))
         (idx (cl-position cur cycle))
         (next (nth (mod (1+ (or idx 0)) (length cycle)) cycle)))
    (setf (puffin--settings-theme s) next)
    (puffin--apply-theme next)))

(provide 'puffin-core)
;;; puffin-core.el ends here

