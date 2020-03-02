(require 'nbt-data)

(defconst location-spec '((offset u24) (sector-count byte)))
(defconst locations-spec '((locations repeat 1024 (struct location-spec))))

(defclass mcr-location ()
  ((offset :initarg :offset
           :initvalue 0)
   (count :initarg :count
          :initvalue 0))
  "MCR location")

(defmethod present-p ((this mcr-location))
  (not (and (= 0 (oref this offset))
            (= 0 (oref this count)))))

(defun mcr/read--raw-locations ()
  (nbt/read-raw-chunk locations-spec))

;;; TODO/FIXME static method?
(defun mcr/read--locations ()
  (--map (mcr-location :offset (* 4096 (cdr (assq 'offset it)))
                       :count  (* 4096 (cdr (assq 'sector-count it))))
         (mcr/read--raw-locations)))

(provide 'mcr-header-locations)
