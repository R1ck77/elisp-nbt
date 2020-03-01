(require 'bindat)
(require 'dash)
(require 'nbt-native)

(defun nbt/create--spec (type count)
  `((unused-name vec ,count ,type)))

(defun nbt/vector-to-list (vector)
  (append vector nil))

(defun nbt/read--binary-values (type count)
  (let* ((spec (nbt/create--spec type count))
         (spec-length (bindat-length spec nil)))
    (let ((result (bindat-unpack spec
                                 (buffer-substring-no-properties (point) (+ spec-length (point))))))
      (goto-char (+ spec-length (point)))
      (cdr (car result)))))

(defun nbt/read--binary-value (type &optional count)
  (nbt/read--binary-values type (or count 1)))

(defun nbt/read-byte ()
  (elt (nbt/read--binary-value 'byte) 0))

(defun nbt/read-short ()
  (elt (nbt/read--binary-value 'short) 0))

(defun nbt/read-int ()
  (elt (nbt/read--binary-value 'long) 0))

(defun nbt/read--bytes (count)
  (append (nbt/read--binary-value 'byte count) nil))

(defun nbt/read-long ()
  (nbt/convert-bytes-to-long (nbt/read--bytes 8)))

(defun nbt/read-float ()
  (nbt/convert-bytes-to-float (nbt/read--bytes 4)))

(defun nbt/read-double ()
  (nbt/convert-bytes-to-double (nbt/read--bytes 8)))

(defun nbt/read-string ()
  (let* ((string-length (nbt/read-short))
         (string-content (buffer-substring-no-properties (point) (+ string-length (point)))))
    (goto-char (+ string-length (point)))
    string-content))

(provide 'nbt-data)
