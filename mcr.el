(require 'dash)
(require 'nbt-data)
(require 'nbt-utils)
(require 'mcr-header)

(defconst chunks-per-region 32)

(defun mcr/region-bound-chunks (p)
  (let ((min-chunk (* p chunks-per-region)))
    (cons min-chunk (+ min-chunk (- chunks-per-region 1)))))

(defun mcr/compute-chunk-range (x z)
  "Return the chunks covered by the file with the specific x and z"
  (cons (mcr/region-bound-chunks x)
        (mcr/region-bound-chunks z)))

(defun mcr/region-for-chunk (c)
  (floor (/ (float c)
            chunks-per-region)))

(defun mcr/file--name-indices (path)
  (save-match-data
    (with-temp-buffer
      (insert path)
      (goto-char (point-min))
      (search-forward-regexp "\\br[.]\\(-?[[:digit:]]+\\)[.]\\(-?[[:digit:]]+\\)[.]mcr$")
      (assert (= (point) (point-max)))
      (cons (match-string 1)
            (match-string 2)))))

(defun mcr/parse-file-name (path)
  (let ((indices (mcr/file--name-indices path)))
    (cons (string-to-number (car indices))
          (string-to-number (cdr indices)))))

(defclass mcr-region ()
  ((header :initarg :header)
   (buffer :initarg :buffer)
   (path :initarg :path))
  "Region with on-demand chunk data decompression")

(defmethod mcr/read-file ((class (subclass mcr-region)) path)
  (let ((buffer (nbt/load-data "TEMP_BUFFER" path)))
    (with-current-buffer buffer
      (mcr-region :header (mcr/read-header)
                  :buffer buffer
                  :path path))))

(defun mcr/demo-something (path)
  (let* ((region (mcr/read-file mcr-region path))
         (selected-one (nbt/random-element (oref region header))))
    (--each (oref region header) (message "%s" it))
    (with-current-buffer (oref region buffer)
     (mcr/read-chunk selected-one))))


(provide 'mcr)
