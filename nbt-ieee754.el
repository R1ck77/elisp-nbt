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
  (apply 'nbt-convert-to-float bytes))

(defun nbt/convert-bytes-to-double (bytes)
  (nbt/check-environment)
  (apply 'nbt-convert-to-double bytes))

;;; TODO/FIXME either rename the module or move this (first option)
(defun nbt/convert-bytes-to-long (bytes)
  (nbt/check-environment)
  (apply 'nbt-convert-to-long bytes))

(provide 'nbt-ieee754)
