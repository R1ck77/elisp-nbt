(require 'cl)
(require 'nbt-tags)
(require 'bindat)

(cl-defstruct nbt-compound name items)

(defun nbt/reduce-tags (raw-tags)
  
  )


(defun nbt/read-uncompressed-file (path)
  (with-temp-buffer
    (toggle-enable-multibyte-characters nil)
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (nbt/reduce-tags (nbt/read-all-raw-tags))))

(provide 'nbt)
