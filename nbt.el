(require 'cl)
(require 'bindat)
(require 'nbt-utils)
(require 'nbt-objects)

;;; Those two bytes identify zlib files
(defconst nbt-zlib-magic-bytes '(#x1f #x8b))

(defun nbt/read-uncompressed-file (path)
  (nbt/with-configured-buffer path
    (nbt/read-all-raw-tags)))

(defun nbt/exit-if-zlib-absent ()
  (if (not (zlib-available-p))
      (error "Unable to open compressed file: missing zlib support")))

(defun nbt/read-compressed-file (path)
  (nbt/with-configured-buffer path
    (goto-char (point-min))
    (zlib-decompress-region (point-min) (point-max))
    (nbt/read-all-raw-tags)))

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
