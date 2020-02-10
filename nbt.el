(require 'cl)
(require 'nbt-tags)
(require 'bindat)

(cl-defstruct nbt-compound name items)

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
   ((nbt-start-compound-p tag) (make-nbt-compound :name (nbt-start-compound-name tag)
                                                  :items (nbt/read--tags-from-supplier supplier)))
   ((nbt-end-compound-p tag) nil)
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

(defun nbt/read-uncompressed-file (path)
  (with-temp-buffer
    (toggle-enable-multibyte-characters nil)
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (nbt/read-tags-list (nbt/read-all-raw-tags))))

(provide 'nbt)
