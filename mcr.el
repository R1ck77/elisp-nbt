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

(defun mcr/read-file (path)
  (nbt/with-file path
    (let ((all-entries (mcr/read-header)))
      (mcr/read-chunk (car all-entries)))))

(provide 'mcr)
