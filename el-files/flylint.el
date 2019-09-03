;; -*- Emacs-Lisp -*-
;; 
;; A minor mode for on-the-fly syntax check.
;; Copyright (c) 2016-2019, Hiroyuki Ohsaki.
;; All rights reserved.
;; 
;; $Id: flylint.el,v 1.24 2019/02/26 14:57:49 ohsaki Exp ohsaki $
;; 

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this code, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; add the following lines to ~/.emacs:
;;
;; (autoload 'flylint-mode "flylint" nil t)
;; (add-hook 'python-mode-hook
;; 	  '(lambda ()
;; 	     (flylint-mode 1)))
;; (add-hook 'perl-mode-hook
;; 	  '(lambda ()
;; 	     (flylint-mode 1)))
;; (add-hook 'c-mode-hook
;;   	  '(lambda ()
;; 	     (flylint-mode 1)))
;; (add-hook 'emacs-lisp-mode-hook
;;   	  '(lambda ()
;; 	     (flylint-mode 1)))

(defvar-local flylint-timer nil)
(defvar-local flylint-temp-file nil)
(defvar-local flylint-process nil)
(defvar-local flylint-lighter-string " FlyLint")

(defface flylint-error-face '((t :background "#800000")) "")
(defface flylint-warning-face '((t :background "#006000")) "")

(define-minor-mode flylint-mode
  "Toggle on-the-fly syntax checking in the current buffer."
  :lighter flylint-lighter-string
  (cond
   (flylint-mode
    ;; activate only when visiting a file
    (when (buffer-file-name)
      (add-hook 'after-save-hook 'flylint-start-check nil t)
      (add-hook 'post-command-hook 'flylint-post-command-hook nil t)
      (add-hook 'pre-command-hook 'flylint-pre-command-hook nil t)))
   (t
    (remove-hook 'after-save-hook 'flylint-start-check t)
    (remove-hook 'post-command-hook 'flylint-post-command-hook t)
    (remove-hook 'pre-command-hook 'flylint-pre-command-hook t))))

(defun flylint-post-command-hook ()
  (if (and flylint-timer
	   (memq flylint-timer timer-idle-list))
      ;; do not create a timer if already present
      nil
    (setq flylint-timer (run-with-idle-timer 0.25 nil 'flylint-timer-event))))

(defun flylint-timer-event ()
  ;; display error/warning message in the minibuffer
  (dolist (ov (overlays-at (point)))
    (let ((msg (overlay-get ov 'flylint)))
      (message msg))))
	
(defun flylint-temp-file-name (name)
  "Generate temporalry filename for NAME."
  (let ((ext (file-name-extension name)))
    (concat (file-name-sans-extension name)
	    "_flylint"
	    (if ext
		(concat "." ext)))))

(defun flylint-start-check ()
  "Start syntax checking of the current buffer, and display errors as overlays."
  (let* ((file (buffer-file-name))
	 (base (file-name-nondirectory file))
	 (buf (get-buffer-create (concat "flylint:" base))))
    ;; prepare temporary file for syntax check
    (setq flylint-temp-file (flylint-temp-file-name file))
    (if (file-exists-p flylint-temp-file)
	(delete-file flylint-temp-file))
    (write-region nil nil flylint-temp-file nil 'no-display)

    ;; terminate the check process if already running
    (when flylint-process
      (set-process-sentinel flylint-process nil)
      (delete-process flylint-process))
    ;; asynchronously start check process
    (with-current-buffer buf
      (erase-buffer))
    (setq flylint-process
	  (start-process "pylint" buf "pylint" flylint-temp-file))
    (set-process-sentinel flylint-process 'flylint-process-sentinel)))

(defun flylint-process-sentinel (process event)
  (when (memq (process-status process) '(exit))
    ;; discard the temporary file
    (if (file-exists-p flylint-temp-file)
	(delete-file flylint-temp-file))
    (let ((errors (flylint-parse-output (process-buffer process))))
      (flylint-display-overlays errors)
      (setq flylint-lighter-string
	    (format " FlyLint/%d" (length errors))))))

(defun flylint-parse-output (buf)
  "Parse the output from pylint command and returns the results as a list."
  (let ((lst))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward "^\\([^\n:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)" nil t)
	(setq lst (cons 
		   (list
		    (match-string 1)
		    (string-to-number (match-string 2))
		    (string-to-number(match-string 3))
		    (match-string 4))
		   lst))))
    lst))

(defun flylint-pick-face (str)
  (let ((case-fold-search t))
    (cond
     ((string-match "\\(but unused\\|never used\\|warning\\)" str)
      'flylint-warning-face)
     (t
      'flylint-error-face))))

(defun flylint-display-overlays (errors)
  "Display results of syntax checking as overlays."
  (save-excursion
    (remove-overlays)
    (dolist (entry errors)
      (let* ((file (nth 0 entry))
	     (line (nth 1 entry))
	     (col (nth 2 entry))
	     (msg (nth 3 entry))
	     (face (flylint-pick-face msg)))
	(goto-char (point-min))
	(forward-line (1- line))
	(flylint-put-overlay (point) (line-end-position) face msg)))))

(defun flylint-put-overlay (beg end face msg)
  "Put overlay on the text between BEG and END with face FACE and text MSG."
  (let  ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'flylint msg)
    ov))

(defun flylint-pre-command-hook ()
  nil)

(provide 'flylint)
