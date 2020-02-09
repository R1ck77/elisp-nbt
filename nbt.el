(require 'cl)
(require 'nbt-tags)
(require 'bindat)

(cl-defstruct nbt-compound name items)

;;; sketched only
(defun nbt/read-tags-list (raw-tags)
  (let ((result))
    (cond
     ((nbt-start-compound-p tag) (make-compound-tag (nbt/read-tags-list remaining-tags)))
     ((nbt-end-compound-p tag) exit-this-read-cycle)
     (t (setq result (cons result tag))))
    
    )
  ;; read list
  
  ;; treat everything but compound and begin like a list elment
  )


(defun nbt/read-uncompressed-file (path)
  (with-temp-buffer
    (toggle-enable-multibyte-characters nil)
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (nbt/reduce-tags (nbt/read-all-raw-tags))))

(provide 'nbt)
