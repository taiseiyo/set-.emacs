;; -*- Emacs-Lisp -*-
;; 
;; A Major-mode for editing and viewing annotations for an image file.
;; Copyright (c) 2018-2019, Hiroyuki Ohsaki.
;; All rights reserved.
;; 
;; $Id: doc-annotate.el,v 1.19 2019/02/26 05:37:05 ohsaki Exp ohsaki $
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

;; Prerequisites:
;;   Emacs, sh, bc, and ImageMagick.
;;
;; Installation:
;;
;; Add these lines to your ~/.emacs.
;;
;; (setq doc-view-scale-internally nil)
;; (add-hook 'doc-view-mode-hook
;;   	  '(lambda ()
;; 	     (local-set-key "c" 'doc-annotate-add-annotation)
;; 	     (local-set-key [mouse-1] 'doc-annotate-add-annotation)))
;; (autoload 'doc-annotate-mode "doc-annotate")
;; (autoload 'doc-annotate-add-annotation "doc-annotate")
;; (add-to-list 'auto-mode-alist '("\\.ant$" . doc-annotate-mode))
;;
;; Download a shell script `doc-annotate-clip' from
;; http://www.lsnl.jp/~ohsaki/software/elisp/doc-annotate-clip, and save it as
;; /usr/local/bin/doc-annotate-clip (or anywhere in your search path).
;; Do not foget to run `chmod +x' to make it executable.

;; Usage:
;;
;; 1. Add the above lines to your ~/.emacs, and restart the Emacs (or
;; reload ~/.emacs with `eval-region').
;; 
;; 2. Open an image file (e.g., paper.pdf) with Emacs.  With recent
;; Emacsen, image files (PDF, PS, and DVI files) are loaded in the
;; DocView mode.  Check the document of the DocView mode for its basic
;; operations.
;;
;; 3. In the DocView mode, move the mouse cursor, and type the 'c' key
;; or click the left mouse button at which you want to record an
;; annotation.  An annotation file corresponding to the image file
;; (e.g., paper.ant) is opened (or newly created if it does not exist)
;; and it pops up in the lower window.  Insert you comment at the
;; point.
;;
;; 4. When you are visiting an annotation file, you can use the
;; following commands.
;;  - Navigate annotations
;;   - doc-annotate-next-annotation (M-p)
;;   - doc-annotate-previous-annotation (M-n)
;; - Display annotation at the point
;;   - doc-annotate-visit-current-annotation (C-cC-c)
;;   - doc-annotate-display-current-clip (C-cC-i)
;; - Close annotation at the point
;;   - doc-annotate-record-done (C-cC-t)

(require 'doc-view)

;;; keymap
(defvar doc-annotate-mode-map (make-sparse-keymap))
(define-key doc-annotate-mode-map "\C-c\C-c" 'doc-annotate-visit-current-annotation)
(define-key doc-annotate-mode-map "\C-c\C-i" 'doc-annotate-display-current-clip)
(define-key doc-annotate-mode-map "\M-n" 'doc-annotate-next-annotation)
(define-key doc-annotate-mode-map "\M-p" 'doc-annotate-previous-annotation)
(define-key doc-annotate-mode-map "\C-c\C-t" 'doc-annotate-record-done)

(defvar doc-annotate-font-lock-keywords
      '(("^ *#.*$" . font-lock-comment-face)
	("DONE" . font-lock-keyword-face)
	("\\[[0-9/]+\\]" . font-lock-string-face)
	("(\\?)" . font-lock-warning-face)))

(defvar-local doc-annotate-image-buffer nil)

(defun doc-annotate-mode ()
  "Major mode for editing and viewing annotations for an image file.

DocAnnotate mode enables to write annotations to any document
(PDF, PS, and DVI files) readable with the DocView mode.

You can use
\\<doc-annotate-mode-map>\\[doc-annotate-visit-current-annotation]
to display the corresponing image in the other window at which
the annotation at the point (i.e., cursor) is recorded.

\\{doc-annotate-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'doc-annotation-mode)
  (setq mode-name "DocAnnotate")
  (use-local-map doc-annotate-mode-map)
  ;; local variables
  (setq doc-annotate-image-buffer
	(find-file-noselect (doc-annotate-image-file))) ;load accompanying image file
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-end "")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(doc-annotate-font-lock-keywords nil nil))
  (font-lock-mode 1)
  (run-hooks 'doc-annotation-mode-hook))

; ----------------------------------------------------------------
(defun doc-annotate-image-file ()
  "Return the filename of a corresponding image file."
  (let (file found)
    ;; FIXME: image file type should be configurable
    (dolist (ext '(".pdf" ".ps" ".dvi"))
      (setq file (concat (file-name-sans-extension (buffer-file-name)) ext))
      (if (and (not found)
	       (file-exists-p file))
	  (setq found file)))
    found))

(defun doc-annotate-annotation-file ()
  "Return the filename of corresponding ANT file."
  (concat (file-name-sans-extension (buffer-file-name)) ".ant"))

(defun doc-annotate-convert-to-relative (pos)
  "Convert pixel geometry POS to relative image geometry.  POS
is a cons cell representing (X . Y)."
  (let* ((x (car pos))
	 (y (cdr pos))
	 (edges (window-inside-pixel-edges))
	 (imgsize (image-size (doc-view-current-image) t)))
    ;; convert frame coordinate (pixel) to window coordinate (pixel)
    (setq x (- x (car edges)))
    (setq y (- y (car (cdr edges))))
    ;; convert window coordinate (pixel) to image coordinate (pixel)
    (when (image-mode-window-get 'hscroll)
      (setq x (+ x (* (image-mode-window-get 'hscroll)
		      (frame-char-width)))))
    (when (image-mode-window-get 'vscroll)
      (setq y (+ y (* (image-mode-window-get 'vscroll)
		      (frame-char-height)))))
    ;; convert image coordinate (pixel) to relative image coordinate (fraction)
    (setq x (/ (float x) (car imgsize)))
    (setq y (/ (float y) (cdr imgsize)))
    (cons x y)))

(defun doc-annotate-convert-to-absolute (relpos)
  "Convert relative image geometry RELPOS to pixel geometry.  POS
is a cons cell representing (X . Y)."
  (let* ((x (car relpos))
	 (y (cdr relpos))
	 (edges (window-inside-pixel-edges))
	 (imgsize (image-size (doc-view-current-image) t)))
    ;; convert relative image coordinate (fraction) to image coordinate (pixel)
    (setq x (* (float x) (car imgsize)))
    (setq y (* (float y) (cdr imgsize)))
    ;; image coordinate (pixel) to convert window coordinate (pixel)
    (when (image-mode-window-get 'hscroll)
      (setq x (- x (* (image-mode-window-get 'hscroll)
		      (frame-char-width)))))
    (when (image-mode-window-get 'vscroll)
      (setq y (- y (* (image-mode-window-get 'vscroll)
		      (frame-char-height)))))
    ;; convert window coordinate (pixel) to frame coordinate (pixel)
    (setq x (+ x (car edges)))
    (setq y (+ y (car (cdr edges))))
    (cons (round x) (round y))))

; ----------------------------------------------------------------
(defun doc-annotate-display-buffer ()
  "Display the annotation buffer (current buffer) and its
  corresponding image buffer."
  (interactive)
  (let* ((ant-buf (current-buffer))
	 (img-buf doc-annotate-image-buffer))
    (delete-other-windows)
    (split-window-vertically -8)
    (switch-to-buffer img-buf)
    (switch-to-buffer-other-window ant-buf)))
  
(defun doc-annotate-current-annotation ()
  "Return a list describing the annotation at the point; the
  point at which the annotation starts, the page number, and the
  geometry."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^#\\+annotation: *\\([0-9.]+\\) +\\([0-9.]+\\) +\\([0-9.]+\\)" nil t)
      (let* ((page (string-to-number (match-string 1)))
	     (x (string-to-number (match-string 2)))
	     (y (string-to-number (match-string 3))))
	(beginning-of-line)
	(list (point) page x y)))))

(defun doc-annotate-next-annotation ()
  "Move to the next annotation."
  (interactive)
  (let ((pnt))
    (save-excursion
      (forward-line 1)
      (when (re-search-forward "^#\\+annotation:" nil t)
	(beginning-of-line)
	(setq pnt (point))))
    (when pnt
      (goto-char pnt)
      (doc-annotate-visit-current-annotation))))

(defun doc-annotate-previous-annotation (&optional count)
  "Move to the previous annotation."
  (interactive)
  (let ((pnt))
    (save-excursion
      (when (re-search-backward "^#\\+annotation:" nil t)
	(setq pnt (point))))
    (when pnt
      (goto-char pnt)
      (doc-annotate-visit-current-annotation))))

(defun doc-annotate-visit-current-annotation ()
  "Parse the annotation at the point and display the
  corresponding image in other window."
  (interactive)
  (let* ((annot (doc-annotate-current-annotation))
	 (pnt (nth 0 annot))
	 (page (nth 1 annot))
	 (x (nth 2 annot))
	 (y (nth 3 annot))
	 pos)
    (doc-annotate-display-buffer)
    (other-window 1)
    (doc-view-fit-width-to-window)
    (doc-view-goto-page page)
    (image-bob)
    ;; FIXME: this code assumes image height is less than two window heights
    (if (>= y 0.5)
	(image-scroll-up 30))
    ;; redisplay the window
    (sit-for 0)
    (setq pos (doc-annotate-convert-to-absolute (cons x y)))
    (other-window 1)
    ;; FIXME: mouse color sould change temporarily
    (set-frame-parameter (window-frame) 'mouse-color "red")
    (set-mouse-pixel-position (window-frame) (car pos) (cdr pos))))

(defun doc-annotate-display-current-clip ()
  "Display an inline image showing where the annotation at the
point is recorded.  The exact location of the annotation is
pointed by the small red rectangular.  This function executes an
external program `doc-annotate-clip'."
  (interactive)
  (let* ((annot (doc-annotate-current-annotation))
	 (pnt (nth 0 annot))
	 (page (nth 1 annot))
	 (x (nth 2 annot))
	 (y (nth 3 annot))
	 ;; NOTE: the following code depends on how DocView mode works
	 ;; internally --- PNG files are stored as page-<n>.png in the
	 ;; cache directory.
	 (cachedir (with-current-buffer doc-annotate-image-buffer
		     (doc-view--current-cache-dir)))
	 (pngfile (format "%spage-%d.png" cachedir page))
	 (clipfile (format "%s/clip-%d.png" cachedir pnt)))
    (call-process "doc-annotate-clip" nil nil nil 
		  pngfile (number-to-string x) (number-to-string y) clipfile)
    ;; FIXME: probhibit duplicate displays
    ;; FIXME: support clip removal
    (put-image (create-image clipfile)
	       (save-excursion
		 (goto-char pnt)
		 (forward-line -1)
		 (point)))))

(defun doc-annotate-record-done ()
  "Record that the annotation at the point has beeen processed.
Leave a tag (DONE) followed by your login name and the curre time."
  (interactive)
  (let ((lst (doc-annotate-current-annotation)))
    (goto-char (nth 0 lst))
    (forward-line 1)
    (insert (format "DONE -%s [%s]\n"
		    (user-login-name)
		    (format-time-string "%Y/%m/%d")))
    (search-backward (concat "-" (user-login-name)))))

; ----------------------------------------------------------------
(defun doc-annotate-add-annotation ()
  "Open (or create) the corresponding annotation file, and add a
  new annotation for the image under the mouse pointer."
  (interactive)
  (let* ((img-buf (current-buffer))
	 (page (doc-view-current-page))
	 (relpos (doc-annotate-convert-to-relative
		  (cdr (mouse-pixel-position))))
	 (ant-buf (find-file-noselect (doc-annotate-annotation-file))))
    (switch-to-buffer ant-buf)
    (doc-annotate-display-buffer)
    ;; FIXME: keep annotations sorted
    (goto-char (point-max))
    ;; delete space at the bottom
    (delete-region (point)
		   (save-excursion
		     (skip-chars-backward " \t\n")
		     (point)))
    (insert (format "\n\n#+annotation: %d %.3f %.3f -%s [%s]\n" 
		    page (car relpos) (cdr relpos)
		    (user-login-name)
		    (format-time-string "%Y/%m/%d")))))
