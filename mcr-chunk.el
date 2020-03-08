;;; TODO/FIXME use an EIEIO object
(defconst chunk-header '((length long) (compression byte)))

(defun mcr/safe-seek (position)
  (goto-char position)
  (assert (= (point) position)))

(defun debug-sample-substring ()
  (goto-char 8193)
  (assert (= (point) 8193))
  (nbt/with-raw-data (buffer-substring-no-properties (point) (+ 8192 (point)))
    (write-file "/tmp/emacs-mytest")
    (delete-char 5)
    (write-file "/tmp/emacs-after-deletion")
    (assert (zlib-decompress-region (point-min) (point-max))))) ;;; TODO/FIXME this is ok

(defun mcr/read-chunk (header-entry)
  (debug-sample-substring)  
  (message "current chunk: %s" header-entry)
  (mcr/safe-seek 8193) ;; 8192 [5:] in python, that is 8192 + 6 in emacs
  (let ((chunk-header-content (nbt/read-data chunk-header)))
    (assert (= 2 (cdr (assq 'compression chunk-header-content))))
    (let* ((length (cdr (assq 'length chunk-header-content))))
      (nbt/with-raw-data (buffer-substring-no-properties (+ (point) 6) (1- (+ (point) length)))
        (message "Size: %s" (- (point-max) (point-min)))
        (write-file "/tmp/something.gz")
        (assert (zlib-decompress-region (point-min) (point-max)))
       
        ))
    ;;; TODO/FIXME hic sunt leones. I can't get to uncompress the data
    ))


(provide 'mcr-chunk)
