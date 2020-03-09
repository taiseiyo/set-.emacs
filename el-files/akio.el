(defun akio-mode ()
  "write akio's advice at the tmp file"
  (interactive)
  (setq mes kill-ring)
  (setq time
	(format-time-string "%Y-%m-%d(%a)"
				  (current-time)))
  (write-region
   (message "\n%s\n" time) nil "~/tmp/akio/adv.org" t)
  (write-region
   (message "%s" mes) nil "~/tmp/akio/adv.org" t)
  ;;(write-file .....)
  (pop kill-ring)
  )

(provide 'akio)
