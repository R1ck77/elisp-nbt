(require 'bindat)
(require 'dash)
(require 'nbt-native)

(defun nbt/create--spec (type count)
  `((unused-name vec ,count ,type)))

(defun nbt/vector-to-list (vector)
  (append vector nil))

(defun nbt/unpack-and-move (spec)
  (let* ((bytes (bindat-length spec nil))
         (start-pos (point))
         (end-pos (+ start-pos bytes))
         (contents (bindat-unpack spec (buffer-substring-no-properties start-pos end-pos))))
    (goto-char end-pos)
    contents))

(defun nbt/read--binary-values (type count)
  (let* ((spec (nbt/create--spec type count)))
    (cdar (nbt/unpack-and-move spec))))

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

;;; TODO/FIXME test
(defun nbt/read-data (spec)
  (nbt/unpack-and-move spec))

;;; TODO/FIXME test
(defun nbt/read-raw-chunk (spec)
  "Read a chunk of header"
  (cdar (nbt/read-data spec)))

(provide 'nbt-data)
