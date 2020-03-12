(require 'buttercup)
(require 'dash)
(require 'nbt-utils)

(describe "nbt-utils"
  (describe "nbt/with-file"
    (it "contains the specified file with the correct bytes count"
      (nbt/with-file "test-data/multibyte-text.txt"
        (expect (point-max) :to-be 22))))
  (describe "nbt/with-raw-data"
    (it "contains the expected data"
      (let ((content))
        (nbt/with-file "test-data/multibyte-text.txt"
          (setq content (buffer-substring-no-properties (point-min) (point-max))))
        (nbt/with-raw-data content
          (expect (point-max) :to-be 22)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal content))))
    (it "evaluates the expression in the starting buffer"
      (nbt/with-file "test-data/multibyte-text.txt"
        (goto-char 2)
        (nbt/with-raw-data (buffer-substring-no-properties (point) (+ 10 (point)))
          (expect (point-max) :to-be 11))))))
