(require 'dash)
(require 'nbt-data)
(require 'nbt-utils)
(require 'mcr-header-locations)
(require 'mcr-header-timestamps)
(require 'mcr-chunk)

;;; TODO/FIXME EIEIO objects, symbols and other names are confusing at best
(defclass mcr-header-entry ()
  ((location :initarg :location
             :intvalue nil)
   (timestamp :initarg :timestamp
              :initvalue nil)))

(defmethod present-p ((this mcr-header-entry))
  (present-p (oref this location)))

(defmethod get-buffer-location ((this mcr-header-entry))
  (1+ (oref (oref this location) offset)))


;;; TODO/FIXME remove the let, used only for debugging
(defun mcr/read-header ()
  "Read the first 8192 bytes of minecraft header, filtering missing sectors"
  (let ((result (--filter (present-p it)
                          (--zip-with (mcr-header-entry :location it
                                                           :timestamp other)
                                         (mcr/read--locations)
                                         (mcr/read--timestamps)))))
    result))


(provide 'mcr-header)
