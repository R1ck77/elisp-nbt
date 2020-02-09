(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-data)

(describe "nbt-data"
  (describe "nbt/read-byte"
    (it "can read a byte and advace"
      (with-small-suite
        (expect (nbt/read-byte) :to-be 10)
        (expect (nbt/read-byte) :to-be 0)
        (expect (nbt/read-byte) :to-be 11))))
  (describe "nbt/read-short"
    (it "can read a short value"
      (with-small-suite
        (goto-char (1+ (point)))
        (expect (nbt/read-short) :to-be 11)))))
