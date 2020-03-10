(defun wise-compile()
  "This function compile quickly for specified program"
  (interactive)
  
  (setq pro_name (buffer-file-name)
	py_name ".py"
	sh_name ".sh")
  
  (if (string-equal py_name
		    (substring pro_name -3))
      (compile (concat "python3 " pro_name))
  (if (string-equal sh_name
		      (substring pro_name -3))
	(compile (concat "bash " pro_name))
      (if (string-equal ".el"
		      (substring pro_name -3))
	  (byte-compile-file (file-name-nondirectory pro_name))
	(message "Not compatible with this mode")
	)
      )
  )
)
  
(provide 'wise-compile)

;;wise-compile.el is end here
