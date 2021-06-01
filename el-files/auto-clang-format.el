;;; auto-clang-format.el --- Minor mode that runs clang-format on save -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 Joel Rosdahl
;;
;; Author: Joel Rosdahl <joel@rosdahl.net>
;; Version: 1.0
;; License: BSD-3-clause
;; Package-Requires: ((emacs "24") (clang-format "20180406.1514"))
;; URL: https://github.com/jrosdahl/auto-clang-format
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; * Neither the name of the copyright holder nor the names of its contributors
;;   may be used to endorse or promote products derived from this software
;;   without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;; ============================================================================
;;
;;; Commentary:
;;
;; auto-clang-format defines is a minor mode (auto-clang-format-mode) that runs
;; clang-format-buffer before the buffer is saved to its file. Exception:
;; clang-format-buffer will not be run if a style hasn't been selected, i.e. if
;; there is no .clang-format file in the directory tree and clang-format-style
;; hasn't been set to an explicit style. If the minor mode will call
;; clang-format-buffer the mode line will include the indicator "ACF",
;; otherwise "!ACF".
;;
;; INSTALLATION
;; ============
;;
;; auto-clang-format depends on the clang-format package, which can be found
;; here:
;;
;;   https://melpa.org/#/clang-format
;;
;; To load and enable auto-clang-format-mode unconditionally, put something
;; like this in your Emacs configuration:
;;
;;   (require 'auto-clang-format)
;;   (add-hook 'c++-mode-hook #'auto-clang-format-mode)

;;; Code:

(require 'clang-format)

(defvar auto-clang-mode--enabled)
(make-variable-buffer-local 'auto-clang-mode--enabled)

;;;###autoload
(define-minor-mode auto-clang-format-mode
  "Toggle Auto-Clang-Format mode.

With a prefix argument ARG, enable Auto-Clang-Format mode if ARG
is positive, and disable it otherwise. If called from Lisp,
enable the mode if ARG is omitted or nil.

When Auto-Clang-Format mode is enabled, `clang-format-buffer'
will be run before the buffer is saved to its file if a
.clang-format file is found in the directory tree or if
`clang-format-style' is not \"file\"."
  :init-value nil
  :lighter (:eval (if auto-clang-mode--enabled " ACF" " !ACF"))
  (if auto-clang-format-mode
      (progn
        (setq auto-clang-mode--enabled
              (or (not (string= clang-format-style "file"))
                  (locate-dominating-file "." ".clang-format")))
        (when auto-clang-mode--enabled
                (add-hook 'before-save-hook #'clang-format-buffer nil t)))
    (remove-hook 'before-save-hook #'clang-format-buffer t)))

(provide 'auto-clang-format)

;;; auto-clang-format.el ends here
