
(defmacro with-small-suite (&rest forms)
  (declare (indent defun)
           (debug t))
  `(with-temp-buffer
     (toggle-enable-multibyte-characters nil)
     (insert-file-contents-literally "test-data/hello_world.nbt")
     (goto-char (point-min))
    ,@forms))

(provide 'nbt-test-utils)
