
(defmacro with-small-suite (&rest forms)
  (declare (indent defun)
           (debug t))
  `(with-temp-buffer
     (toggle-enable-multibyte-characters nil)
     (insert-file-contents-literally "test-data/hello_world.nbt")
     (goto-char (point-min))
     ,@forms))

;;; get-byte, byte-to-string

(defun string-from-bytes (&rest bytes)
  (bindat-pack `((x vec ,(length bytes) byte))
               `((x . ,(apply #'vector bytes)))))

(defun to-file (filename string)
  (with-temp-buffer
    (toggle-enable-multibyte-characters nil)
    (insert string)
    (write-file filename)))

(provide 'nbt-test-utils)
