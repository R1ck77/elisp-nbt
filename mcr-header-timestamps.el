(require 'nbt-data)

(defconst timestamps-spec '((timestamps vec 1024 long)))

(defclass mcr-timestamp ()
  ((timestamp :initarg :timestamp
              :initvalue 0))
  "MCR timestamp")

(defun mcr/read--raw-timestamps ()
  (nbt/read-raw-chunk timestamps-spec))

(defun mcr/read--timestamps ()
  (--map (mcr-timestamp :timestamp it)
         (nbt/read-raw-chunk timestamps-spec)))

(provide 'mcr-header-timestamps)
