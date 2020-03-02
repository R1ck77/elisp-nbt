(defmacro nbt/with-configured-buffer (path &rest forms)
  (declare (indent defun)
           (debug t))
  `(with-temp-buffer
     (toggle-enable-multibyte-characters nil)
     (insert-file-contents-literally ,path)
     (goto-char (point-min))
     ,@forms))

(provide 'nbt-utils)
