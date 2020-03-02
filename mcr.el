(require 'dash)
(require 'nbt-data)
(require 'nbt-utils)
(require 'mcr-header)

(defconst chunks-per-region 32)

(defun mcr/parse-file-name (path)
  (error "not implemented"))

(defun mcr/region-bound-chunks (p)
  (let ((min-chunk (* p chunks-per-region)))
    (cons min-chunk (+ min-chunk (- chunks-per-region 1)))))

(defun mcr/compute-chunk-range (x z)
  "Return the chunks covered by the file with the specific x and z"
  (cons (mcr/region-bound-chunks x)
        (mcr/region-bound-chunks z)))

(defun mcr/read-file (path)
  (nbt/with-file path
    (let ((all-entries (mcr/read-header)))
      (mcr/read-chunk (car all-entries)))))

(provide 'mcr)
