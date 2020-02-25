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

(defgeneric nbt-read ((class (subclass nbt-valued-tag)) name-reader-f))

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

(defmethod nbt-read ((class (subclass nbt-byte)) name-f)
  (create-class class name-f #'nbt/read-byte))

(defclass nbt-short (nbt-valued-tag nbt-named-tag)
  ()
  "Short tag representation")

(defmethod nbt-id ((this nbt-short))
  short-tag-id)

(defun create-class (class name-f value-f)
  (apply class (list :name (funcall name-f)
                     :value (funcall value-f))))

(defmethod nbt-read ((class (subclass nbt-short)) name-f)
  (create-class class name-f #'nbt/read-short))

(defun nbt/read-short-tag (name)
  )

(defclass nbt-int (nbt-valued-tag nbt-named-tag)
  ()
  "Long tag representation")

(defmethod nbt-id ((this nbt-int))
  int-tag-id)

(defmethod nbt-read ((class (subclass nbt-int)) name-f)
  (create-class class name-f #'nbt/read-int))

(defclass nbt-long (nbt-valued-tag nbt-named-tag)
  ()
  "Long tag representation")

(defmethod nbt-id ((this nbt-long))
  long-tag-id)

(defmethod nbt-read ((class (subclass nbt-long)) name-f)
  (create-class class name-f #'nbt/read-long))

(defclass nbt-float (nbt-valued-tag nbt-named-tag)
  ()
  "Float tag representation")

(defmethod nbt-id ((this nbt-float))
  float-tag-id)

(defmethod nbt-read ((class (subclass nbt-float)) name-f)
  (create-class class name-f #'nbt/read-float))

(defclass nbt-double (nbt-valued-tag nbt-named-tag)
  ()
  "Double tag representation")

(defmethod nbt-id ((this nbt-double))
  double-tag-id)

(defmethod nbt-read ((class (subclass nbt-double)) name-f)
  (create-class class name-f #'nbt/read-double))

(defclass nbt-string (nbt-valued-tag nbt-named-tag)
  ()
  "String tag representation")

(defmethod nbt-id ((this nbt-string))
  string-tag-id)

(defmethod nbt-read ((class (subclass nbt-string)) name-f)
  (create-class class name-f #'nbt/read-string))

;;; TODO/FIXME this may be named in some situations? Documentation is not clear… :/
(defclass nbt-end (nbt-tag)
  ()
  "Ignoring the possibility of a named tag")

(defmethod nbt-id ((this nbt-end))
  end-tag-id)

(defmethod nbt-read ((class (subclass nbt-end)) name-f)
  (nbt-end))

(defclass nbt-compound (nbt-valued-tag nbt-named-tag)
  ()
  "Compound tag: items is a \(possibly empty\) list of tags")

(defmethod nbt-id ((this nbt-compound))
  start-compound-tag-id)

(defun nbt/read--items-until-end ()
  (let ((next-tag)
        (items))
    (while (/= (nbt-id (setq next-tag (nbt/create-named-tag))) end-tag-id)
      (setq items (cons next-tag items)))
    (nreverse items)))

(defmethod nbt-read ((class (subclass nbt-compound)) name-f)
  (let ((name (funcall name-f)))
    (nbt-compound :name name
                  :value (nbt/read--items-until-end))))

(defclass nbt-list (nbt-valued-tag nbt-named-tag)
  ()
  "List of unnamed tags")

(defmethod nbt-id ((this nbt-list))
  tag-list-tag-id)

;;; TODO/FIXE a lot of repeated code
(defun nbt/read--list-tag (name-f)
)

(defmethod nbt-read ((class (subclass nbt-list)) name-f)
  (let* ((name (funcall name-f))
         (item-type (nbt/read-byte))
         (length (nbt/read-int)))
    (nbt-list :name name
              :value (--map (nbt/create-tag-from-id item-type (lambda () ""))
                            (number-sequence 1 length)))))

(defclass nbt-byte-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of bytes")

(defmethod nbt-id ((this nbt-byte-array))
  byte-array-tag-id)

(defun nbt/read--primitives-list (class name-f read-f)
  (let* ((name (funcall name-f))
         (length (nbt/read-int)))
    (apply class (list :name name
                       :value (--map (funcall read-f) (number-sequence 1 length))))))

(defmethod nbt-read ((class (subclass nbt-byte-array)) name-f)
  (nbt/read--primitives-list class name-f #'nbt/read-byte))

(defclass nbt-int-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of integers")

(defmethod nbt-id ((this nbt-int-array))
  int-array-tag-id)

(defun nbt/read--int-array-tag (name-f)
  (nbt/read--primitives-list nbt-int-array name-f #'nbt/read-int))

(defmethod nbt-read ((class (subclass nbt-int-array)) name-f)
  (nbt/read--int-array-tag name-f))

(defclass nbt-long-array (nbt-valued-tag nbt-named-tag)
  ()
  "List of long values")

(defmethod nbt-id ((this nbt-long-array))
  long-array-tag-id)

(defun nbt/read--long-array-tag (name-f)
  (nbt/read--primitives-list nbt-long-array name-f #'nbt/read-long))

(defmethod nbt-read ((class (subclass nbt-long-array)) name-f)
  (nbt/read--long-array-tag name-f))

(defvar class-for-tag-plist (list start-compound-tag-id nbt-compound
                                  end-tag-id nbt-end
                                  byte-tag-id nbt-byte
                                  short-tag-id nbt-short
                                  int-tag-id nbt-int
                                  long-tag-id nbt-long
                                  float-tag-id nbt-float
                                  double-tag-id nbt-double
                                  string-tag-id nbt-string
                                  short-tag-id nbt-short
                                  tag-list-tag-id nbt-list
                                  byte-array-tag-id nbt-byte-array
                                  int-array-tag-id nbt-int-array
                                  long-array-tag-id nbt-long-array))

(defun nbt/class-for-tag (tag-id)
  (or (plist-get class-for-tag-plist tag-id)
      (error (format "Unsupported tag %d" tag-id))))

(defun nbt/create-tag-from-id (tag-id name-provider-f)
  (apply #'nbt-read (list (nbt/class-for-tag tag-id)
                          name-provider-f)))

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
    (assert (= 1 (length tags)))
    (car tags)))

(provide 'nbt-objects)
