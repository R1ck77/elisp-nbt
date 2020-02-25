;;; C-supported routines for floating point conversion

(defun nbt/ensure-module-file-support ()
  (if (not module-file-suffix)
      (error "Emacs should support floating point modules for this package to work")))

(defun nbt/ensure-module-file-present ()
  (module-load "./libnbt.so"))

(defun nbt/check-environment ()
  (nbt/ensure-module-file-support)
  (nbt/ensure-module-file-present))

(defun nbt/convert-bytes-to-float (bytes)
  (nbt/check-environment)
  0.0)

(defun nbt/convert-bytes-to-double (bytes)
  (nbt/check-environment)
  0.0)

(provide 'nbt-ieee754)
