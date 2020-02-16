(require 'eieio)
(require 'dash)
(require 'nbt-tags-id)
(require 'nbt-data)

(defclass nbt-tag ()
  ()
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

(defmethod nbt-id ((this nbt-byte))
  byte-tag-id)

(defun nbt/read-byte-tag (name)
  (nbt-byte :name name
            :value (nbt/read-byte)))

(defclass nbt-short (nbt-valued-tag nbt-named-tag)
  ()
  "Short tag representation")

(defmethod nbt-id ((this nbt-short))
  short-tag-id)

(defun nbt/read-short-tag (name)
  (nbt-short :name name
             :value (nbt/read-short)))

(defclass nbt-int (nbt-valued-tag nbt-named-tag)
  ()
  "Long tag representation")

(defmethod nbt-id ((this nbt-int))
  int-tag-id)

(defun nbt/read-int-tag (name)
  (nbt-int :name name
           :value (nbt/read-int)))

(defclass nbt-long (nbt-valued-tag nbt-named-tag)
  ()
  "Long tag representation")

(defmethod nbt-id ((this nbt-long))
  long-tag-id)

(defun nbt/read-long-tag (name)
  (nbt-long :name name
            :value (nbt/read-long)))

(defclass nbt-float (nbt-valued-tag nbt-named-tag)
  ()
  "Float tag representation")

(defmethod nbt-id ((this nbt-float))
  float-tag-id)

(defun nbt/read-float-tag (name)
  (nbt-float :name name
             :value (nbt/read-float)))

(defclass nbt-double (nbt-valued-tag nbt-named-tag)
  ()
  "Double tag representation")

(defmethod nbt-id ((this nbt-double))
  double-tag-id)

(defun nbt/read-double-tag (name)
  (nbt-double :name name
              :value (nbt/read-double)))

(defclass nbt-string (nbt-valued-tag nbt-named-tag)
  ()
  "String tag representation")

(defmethod nbt-id ((this nbt-string))
  string-tag-id)

(defun nbt/read-string-tag (name)
  (nbt-string :name name
              :value (nbt/read-string)))

;;; TODO/FIXME this may be named in some situations? Documentation is not clear… :/
(defclass nbt-end (nbt-tag)
  ()
  "Ignoring the possibility of a named tag")

(defmethod nbt-id ((this nbt-end))
  end-tag-id)

(defun nbt/read-end-tag ()
  (nbt-end))

(defclass nbt-raw-compound (nbt-named-tag)
  ()
  "Raw version of the compound tag \(no items\)")

(defmethod nbt-id ((this nbt-raw-compound))
  start-compound-tag-id)

(defun nbt/read-raw-compound-tag (name)
  (nbt-raw-compound :name name))

(defclass nbt-compound (nbt-valued-tag nbt-named-tag)
  ()
  "Compound tag: items is a \(possibly empty\) list of tags")

(defmethod nbt-id ((this nbt-compound))
  start-compound-tag-id)

(defclass nbt-list (nbt-valued-tag nbt-named-tag)
  ()
  "List of unnamed tags")

(defmethod nbt-id ((this nbt-list))
  tag-list-tag-id)

(defun nbt/read-list-tag (name)
  (let* ((item-type (nbt/read-byte))
         (length (nbt/read-int)))
    (nbt-list :name name
              :value (--map (nbt/create-tag-from-id item-type (lambda () ""))
                            (number-sequence 1 length)))))

(defclass nbt-byte-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of bytes")

(defmethod nbt-id ((this nbt-byte-array))
  byte-array-tag-id)

(defun nbt/read-byte-array-tag (name)
  (let* ((length (nbt/read-int)))
    (nbt-list :name name
              :value (--map (nbt/read-byte) (number-sequence 1 length)))))

(defclass nbt-int-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of integers")

(defmethod nbt-id ((this nbt-int-array))
  int-array-tag-id)

(defun nbt/read-int-array-tag (name)
  (let* ((length (nbt/read-int)))
    (nbt-int-array :name name
                   :value (--map (nbt/read-int) (number-sequence 1 length)))))

(defclass nbt-long-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of long values")

(defmethod nbt-id ((this nbt-long-array))
  long-array-tag-id)

(defun nbt/read-long-array-tag (name)
  (let* ((length (nbt/read-int)))
    (nbt-long-array :name name
                    :value (--map (nbt/read-long) (number-sequence 1 length)))))

(defun nbt/create-tag-from-id (tag-id name-provider-f)
  (cond
   ((= start-compound-tag-id tag-id)
    (nbt/read-raw-compound-tag (funcall name-provider-f)))
   ((= end-tag-id tag-id)
    (nbt/read-end-tag))
   ((= byte-tag-id tag-id)
    (nbt/read-byte-tag (funcall name-provider-f)))
   ((= short-tag-id tag-id)
    (nbt/read-short-tag (funcall name-provider-f)))
   ((= int-tag-id tag-id)
    (nbt/read-int-tag (funcall name-provider-f)))
   ((= long-tag-id tag-id)
    (nbt/read-long-tag (funcall name-provider-f)))
   ((= float-tag-id tag-id)
    (nbt/read-float-tag (funcall name-provider-f)))
   ((= double-tag-id tag-id)
    (nbt/read-double-tag (funcall name-provider-f)))
   ((= string-tag-id tag-id)
    (nbt/read-string-tag (funcall name-provider-f)))
   ((= short-tag-id tag-id)
    (nbt/read-short-tag (funcall name-provider-f)))
   ((= tag-list-tag-id tag-id)
    (nbt/read-list-tag (funcall name-provider-f)))
   ((= byte-array-tag-id tag-id)
    (nbt/read-byte-array-tag (funcall name-provider-f)))
   ((= int-array-tag-id tag-id)
    (nbt/read-int-array-tag (funcall name-provider-f)))
   ((= long-array-tag-id tag-id)
    (nbt/read-long-array-tag (funcall name-provider-f)))
   (t (error (format "Unsupported tag %d" tag-id)))))

(defun nbt/create-tag (name-provider-f)
  (nbt/create-tag-from-id (nbt/read-byte) name-provider-f))

(defun nbt/create-named-tag ()
  (nbt/create-tag #'nbt/read-string))

(defun nbt/create-nameless-tag ()
  (nbt/create-tag (lambda () "")))

(defun nbt/read-all-raw-tags ()
  (let ((tags))
    (while (/= (point) (point-max))
      (setq tags (cons (nbt/create-named-tag) tags)))
    (nreverse tags)))

(provide 'nbt-objects)
