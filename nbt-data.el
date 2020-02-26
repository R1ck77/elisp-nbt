(require 'bindat)
(require 'dash)
(require 'nbt-ieee754)

(defun nbt/read--binary-value (size type)
  (let ((result (bindat-unpack (list (list 'unused-name type))
                               (buffer-substring-no-properties (point) (+ size (point))))))    
    (goto-char (+ size (point)))
    (cdr (car result))))

(defun nbt/read-byte ()
  (nbt/read--binary-value 1 'byte))

(defun nbt/read-short ()
  (nbt/read--binary-value 2 'short))

(defun nbt/read-int ()
  (nbt/read--binary-value 4 'long))

;;; TODO/FIXME can be probably read with a single bindat call
(defun nbt/read-long ()
  (nbt/convert-bytes-to-long (--map (nbt/read-byte) (number-sequence 1 8))))

;;; TODO/FIXME can be probably read with a single bindat call
(defun nbt/read-float ()
  (nbt/convert-bytes-to-float (--map (nbt/read-byte) (number-sequence 1 4))))

(defun nbt/read-double ()
  (nbt/convert-bytes-to-double (--map (nbt/read-byte) (number-sequence 1 8))))

(defun nbt/read-string ()
  (let* ((string-length (nbt/read-short))
         (string-content (buffer-substring-no-properties (point) (+ string-length (point)))))
    (goto-char (+ string-length (point)))
    string-content))

(provide 'nbt-data)
