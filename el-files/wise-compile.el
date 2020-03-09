(defun wise-compile()
  "This function compile quickly for specified program"
  (interactive)
  ;; (setq pro_name buffer-file-name)
  (if (string-equal ".py"
		    (substring (buffer-file-name) -3))
      (compile (concat "python3 "
		       (message (buffer-file-name))))
    (if (string-equal ".sh"
		      (substring (buffer-file-name) -3))
	(compile (concat "bash "
			 (message (buffer-file-name))))
      (message "Not compatible with this mode")
      )
    )
  )

(provide 'wise-compile)
