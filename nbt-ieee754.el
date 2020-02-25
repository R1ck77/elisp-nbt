;;; C-supported routines for floating point conversion

(defun ensure-module-file-support ()
  (if (not module-file-suffix)
      (error "Emacs should support floating point modules for this package to work")))

(defun nbt/convert-bytes-to-float (bytes)
  (ensure-module-file-support)
  0.0)

(defun nbt/convert-bytes-to-double (bytes)
  (ensure-module-file-support)
  0.0)

(provide 'nbt-ieee754)
