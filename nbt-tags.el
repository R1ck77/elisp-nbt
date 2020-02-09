(require 'cl)
(require 'nbt-data)

(cl-defstruct nbt-raw-compound
  name)

(defun nbt/read--string ()
  (let* ((string-length (nbt/read-short))
         (string-content (buffer-substring-no-properties (point) (+ string-length (point)))))
    (goto-char (+ string-length (point)))
    string-content))

(defun nbt/read--raw-compound ()
  (make-nbt-raw-compound :name (nbt/read--string)))

(defun nbt/read-raw-tag ()
  (case (nbt/read-byte)
    ((list 0) "TAG_End")
    ((list 1) "TAG_Byte")
    ((list 2) "TAG_Short")
    ((list 3) "TAG_Int")
    ((list 4) "TAG_Long")
    ((list 5) "TAG_Float")
    ((list 6) "TAG_Double")
    ((list 7) "TAG_Byte_Array")
    ((list 8) "TAG_String")
    ((list 9) "TAG_List")
    ((list 10) (nbt/read--raw-compound))
    ((list 11) "TAG_Int_Array")
    ((list 12) "TAG_Long_Array")))

(provide 'nbt-tags)
