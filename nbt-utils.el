(defmacro nbt/with--configured-buffer (content-f &rest forms)
  (declare (indent defun)
           (debug t))
  `(with-temp-buffer
     (toggle-enable-multibyte-characters nil)
     (funcall ,content-f)
     (goto-char (point-min))
     ,@forms))


(defmacro nbt/with-raw-data (content &rest forms)
  (declare (indent defun)
           (debug t))
  (let ((evaluated-content (make-symbol "evaluated-content")))
    `(let ((,evaluated-content ,content))
       (nbt/with--configured-buffer (lambda ()
                                      (insert ,evaluated-content))
         ,@forms))))

(defmacro nbt/with-file (path &rest forms)
  (declare (indent defun)
           (debug t))
  `(nbt/with--configured-buffer (lambda ()
                                  (insert-file-contents-literally ,path))
     ,@forms))

(provide 'nbt-utils)
