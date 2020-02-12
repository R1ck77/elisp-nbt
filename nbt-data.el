(require 'bindat)

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

(defun nbt/read-long ()
  "long values are not supported in Emacs, so I'm returing the value of the lowest significant int until I figure something out"  
  (let ((bytes (list (nbt/read-int)
                     (nbt/read-int))))
    (cadr bytes)))

(defun nbt/undiscovered-magic (bytes)
  0.0)

(defun nbt/read-float ()
  "Hic sunt leones. Big ones."
  (nbt/undiscovered-magic (nbt/read--binary-value 4 'long)))

(defun nbt/read-double ()
  (nbt/undiscovered-magic (nbt/read--binary-value 8 'double)))

(provide 'nbt-data)
