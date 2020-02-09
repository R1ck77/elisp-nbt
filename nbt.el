(require 'nbt-tags)
(require 'bindat)


(defun nbt/read-uncompressed-file (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-min))
    
    )
  )

(provide 'nbt)
