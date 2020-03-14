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

;;; TODO/FIXME name!
(defmethod present-p ((this mcr-header-entry))
  (present-p (oref this location)))

(defmethod mcr/entry-byte ((this mcr-header-entry))
  (oref (oref this location) byte))

;;; TODO/FIXME name!
(defmethod get-buffer-location ((this mcr-header-entry))
  "Returns the location in the buffer (1-based coordinate)"
  (1+ (oref (oref this location) offset)))

(defun mcr/read--headers ()
  "Read the first 8192 bytes of the minecraft header"
  (--zip-with (mcr-header-entry :location it
                                :timestamp other)
              (mcr/read--locations)
              (mcr/read--timestamps)))

(defun mcr/filtered--headers ()
  "Return the list of offset/timestamp entries that are not empty"
  (--filter (present-p it) (mcr/read--headers)))

(defun mcr/read-header-map ()
  "Return a hash table with all the headers"
  (let ((table (make-hash-table)))    
    (--each (mcr/filtered--headers)
      (puthash (mcr/entry-byte it) it table))
    table))

(defun mcr/read-header-table ()
  "Read the first 8192 bytes of minecraft header, return an hash-table"
  (mcr/read-header-map)) ;;TODO/FIXME overkill


(provide 'mcr-header)
