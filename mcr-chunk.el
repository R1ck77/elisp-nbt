;;; TODO/FIXME use an EIEIO object
(defconst chunk-header '((length long) (compression byte)))

(defun mcr/read-chunk (header-entry)
  (message "current chunk: %s" header-entry)
  (goto-char (get-buffer-location header-entry))
  (assert (= (point) (get-buffer-location header-entry)))
  (let ((chunk-header-content (nbt/read-data chunk-header)))
    (assert (= 2 (cdr (assq 'compression chunk-header-content))))
    (let* ((length (cdr (assq 'length chunk-header-content)))
          (content (buffer-substring-no-properties (point) (+ (point) length))))
      (nbt/with-raw-data content
        (write-file "/tmp/something.gz")
        (assert (zlib-decompress-region (point-min) (point-max)))
        

        (assert false)
        ))
    ;;; TODO/FIXME hic sunt leones. I can't get to uncompress the data
    ))


(provide 'mcr-chunk)
