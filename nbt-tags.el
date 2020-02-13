(require 'cl)
(require 'dash)
(require 'nbt-data)
(require 'nbt-tags-id)

(defmacro comment (&rest forms))

(defun debug-print-read-tag (id name)
  (comment (if (= 0 id)
       (message "--> Reading End Tag")
     (if (= (length name) 0)
         (message "--> Reading nameless Tag")
       (message "--> Reading '%s' tag" name)))))

(cl-defstruct nbt-byte name value)
(cl-defstruct nbt-short name value)
(cl-defstruct nbt-int name value)
(cl-defstruct nbt-long name value)
(cl-defstruct nbt-float name value)
(cl-defstruct nbt-double name value)
(cl-defstruct nbt-byte-array name values)
(cl-defstruct nbt-int-array name values)
(cl-defstruct nbt-long-array name values)
(cl-defstruct nbt-float-array name values)
(cl-defstruct nbt-double-array name values)
(cl-defstruct nbt-tag-list name values) ;;; TODO/FIXME missing field for type?
(cl-defstruct nbt-string name value)
(cl-defstruct nbt-start-compound name)
(cl-defstruct nbt-end-compound)

(defun nbt/read--string-tag (name)
  (make-nbt-string :name name
                   :value (nbt/read-string)))

(defun nbt/read--start-compound-tag (name)
  (make-nbt-start-compound :name name))

(defun nbt/read--double-compound-tag (name)
  (make-nbt-double :name name
                   :value (nbt/read-double)))

;;; TODO/FIXME This may be reaaly really wrong if reading N compound tags is allowed
(defun nbt/read--n-tags (n reader)
  (--map (funcall reader) (number-sequence 0 (1- n))))

(defun nbt/read--tags-array (name constructor reader)
  (let ((length (nbt/read-int)))
    (apply constructor (list :name name
                             :values (nbt/read--n-tags length reader)))))

(defun nbt/read--tag-list-tag (name)
  (let ((item-type (nbt/read-byte))
        (n-items (nbt/read-int)))
    (make-nbt-tag-list :name name
                       :values (--map (nbt/read--nameless-tag item-type)
                                      (number-sequence 0 (1- n-items))))))

(defun nbt/read--byte-array-tag (name)
  (nbt/read--tags-array name #'make-nbt-byte-array #'nbt/read-byte))

(defun nbt/read--int-array-tag (name)
  (nbt/read--tags-array name #'make-nbt-int-array #'nbt/read-int))

(defun nbt/read--long-array-tag (name)
  (nbt/read--tags-array name #'make-nbt-long-array #'nbt/read-long))

(defun nbt/read--float-array-tag (name)
  (nbt/read--tags-array name #'make-nbt-float-array #'nbt/read-float))

(defun nbt/read--double-array-tag (name)
  (nbt/read--tags-array name #'make-nbt-double-array #'nbt/read-double))

(defun nbt/read--float-compound-tag (name)
  (make-nbt-float :name name
                  :value (nbt/read-float)))

(defun nbt/read--long-compound-tag (name)
  (make-nbt-long :name name
                 :value (nbt/read-long)))

(defun nbt/read--int-compound-tag (name)
  (make-nbt-int :name name
                :value (nbt/read-int)))

(defun nbt/read--short-compound-tag (name)
  (make-nbt-short :name name
                  :value (nbt/read-short)))

(defun nbt/read--byte-compound-tag (name)
  (make-nbt-byte :name name
                 :value (nbt/read-byte)))

(defun nbt/read--named-tag (tag-id name)
  (debug-print-read-tag tag-id name)
  (cond
    ((= tag-id byte-tag-id) (nbt/read--byte-compound-tag name))
    ((= tag-id short-tag-id) (nbt/read--short-compound-tag name))
    ((= tag-id int-tag-id) (nbt/read--int-compound-tag name))
    ((= tag-id long-tag-id) (nbt/read--long-compound-tag name))
    ((= tag-id float-tag-id) (nbt/read--float-compound-tag name))
    ((= tag-id double-tag-id) (nbt/read--double-compound-tag name))
    ((= tag-id byte-array-tag-id) (nbt/read--byte-array-tag name))
    ((= tag-id string-tag-id) (nbt/read--string-tag name))
    ((= tag-id tag-list-tag-id) (nbt/read--tag-list-tag name))
    ((= tag-id start-compound-tag-id) (nbt/read--start-compound-tag name))
    ((= tag-id int-array-tag-id) (nbt/read--int-array-tag name))
    ((= tag-id long-array-tag-id) (nbt/read--long-array-tag name))
    (t (error "unrecognized tag id"))))

(defun nbt/read--nameless-tag (tag-id)
  (nbt/read--named-tag tag-id ""))

(defun nbt/read-raw-tag ()
  (let ((tag-id (nbt/read-byte)))
    (if (= end-tag-id tag-id)
        (progn
          (debug-print-read-tag tag-id "")
          (make-nbt-end-compound))
      (nbt/read--named-tag tag-id (nbt/read-string)))))

(defun nbt/read-all-raw-tags ()
  (let ((tags))
    (while (/= (point) (point-max))
      (setq tags (cons (nbt/read-raw-tag) tags)))
    (nreverse tags)))

(provide 'nbt-tags)
