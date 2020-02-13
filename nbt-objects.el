(require 'eieio)
(require 'nbt-tags-id)
(require 'nbt-data)

(defclass nbt-tag ()
  ((id :initarg :id
       :documentation "the id of the tag"))
  :abstract t)

(defmethod nbt-id ((this nbt-tag))
  (oref this id))

(defclass nbt-valued-tag (nbt-tag)
  ((value :initarg :value
          :documentation "the tag content"))
  :abstract t)

(defmethod nbt-value ((this nbt-valued-tag))
  (oref this value))

(defgeneric nbt-write ((this nbt-valued-tag)))

(defclass nbt-named-tag (nbt-tag)
  ((name :initarg :name
         :type string
         :documentation "tag name"))
  :abstract t)

(defmethod nbt-name  ((this nbt-named-tag))
  (oref this name))

(defclass nbt-byte (nbt-valued-tag nbt-named-tag)
  ()
  "Byte tag representation")

(defun nbt/read-byte-tag (tag-id name)
  (assert (= tag-id byte-tag-id))
  (nbt-byte :id tag-id
            :name name
            :value (nbt/read-byte)))

(defclass nbt-string (nbt-valued-tag nbt-named-tag)
  ()
  "String tag representation")

(defun nbt/read-string-tag (tag-id name)
  (assert (= tag-id string-tag-id))
  (nbt-string :id tag-id
              :name name
              :value (nbt/read-string)))

;;; TODO/FIXME this may be named in some situations? :/
(defclass nbt-end (nbt-tag)
  ()
  "Ignoring the possibility of a named tag")

(defun nbt/read-end-tag (tag-id)
  (assert (= tag-id end-tag-id))
  (nbt-end :id end-tag-id))

(defclass nbt-raw-compound (nbt-named-tag)
  ()
  "Raw version of the compound tag \(no items\)")

(defun nbt/read-raw-compound-tag (tag-id name)
  (assert (= tag-id start-compound-tag-id))
  (nbt-raw-compound :id tag-id
                    :name name))

(defclass nbt-compound (nbt-valued-tag nbt-named-tag)
  ()
  "Compound tag: items is a \(possibly empty\) list of tags")

(defun nbt/create-tag ()
  (let ((tag-id (nbt/read-byte)))
    (cond
     ((= start-compound-tag-id tag-id)
      (nbt/read-raw-compound-tag tag-id (nbt/read-string)))
     ((= end-tag-id tag-id)
      (nbt/read-end-tag tag-id))
     ((= byte-tag-id tag-id)
      (nbt/read-byte-tag tag-id tag-id (nbt/read-string)))
     ((= string-tag-id tag-id)
      (nbt/read-string-tag tag-id (nbt/read-string)))
     (t (error (format "Unsupported tag %d" tag-id))))))

(defun nbt/read-all-raw-tags ()
  (let ((tags))
    (while (/= (point) (point-max))
      (setq tags (cons (nbt/create-tag) tags)))
    (nreverse tags)))

(provide 'nbt-objects)
