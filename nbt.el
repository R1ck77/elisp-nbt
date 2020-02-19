(require 'cl)
(require 'bindat)
(require 'nbt-objects)

;;; 1f 8b - should be followed by 0x8 
(defconst nbt-zlib-magic-bytes '(31 139))

;;; TODO/FIXME nbt-utils? I should need a dependency 
(defun nbt/create-supplier (list)
  (lexical-let ((list list))
    (lambda ()
      (let ((next (car list)))
        (setq list (cdr list))
        next))))

;;; TODO/FIXME misnomer?
(defun nbt/get--next-tag (supplier tag)
  (cond
   ((nbt-raw-compound-p tag) (nbt-compound :name (nbt-name tag)
                                           :value (nbt/read--tags-from-supplier supplier)))
   ((nbt-end-p tag) nil)
   (t tag)))

(defun nbt/read--tags-from-supplier (supplier)
  (let ((next-tag (funcall supplier))
        (results))
    (while next-tag
      (let ((candidate (nbt/get--next-tag supplier next-tag)))
        (if candidate
            (progn
              (setq results (cons candidate results))
              (setq next-tag (funcall supplier)))
          (setq next-tag nil))))
    (progn results)))

(defun nbt/read-tags-list (raw-tags)
  (car (nbt/read--tags-from-supplier (nbt/create-supplier raw-tags))))

(defmacro nbt/with-configured-buffer (path &rest forms)
  (declare (indent defun)
           (debug t))
  `(with-temp-buffer
     (toggle-enable-multibyte-characters nil)
     (insert-file-contents-literally ,path)
     (goto-char (point-min))
     ,@forms))

(defun nbt/read-uncompressed-file (path)
  (nbt/with-configured-buffer path
    (nbt/read-tags-list (nbt/read-all-raw-tags))))

(defun nbt/exit-if-zlib-absent ()
  (if (not (zlib-available-p))
      (error "Unable to open compressed file: missing zlib support")))

(defun nbt/read-compressed-file (path)
  (nbt/with-configured-buffer path
    (goto-char (point-min))
    (zlib-decompress-region (point-min) (point-max))
    (nbt/read-tags-list (nbt/read-all-raw-tags))))

;;; TODO/FIXME quite slow
(defun nbt/file-compressed-p (path)
  (nbt/with-configured-buffer path
    (let ((magic-bytes (list (nbt/read-byte)
                             (nbt/read-byte))))
      (equal nbt-zlib-magic-bytes magic-bytes))))

(defun nbt/read-file (path)
  (if (nbt/file-compressed-p path)
      (nbt/read-compressed-file path)
    (nbt/read-uncompressed-file path)))

(provide 'nbt)
