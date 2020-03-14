(require 'buttercup)
(require 'dash)
(require 'mcr)

(describe "mcr"
  (describe "mcr/parse-file-name"
    (it "returns the correct numbers for relative paths"
      (expect (mcr/parse-file-name "r.23.-34.mcr")
              :to-equal '(23 . -34)))
    (it "returns the correct numbers for absolute paths"
      (expect (mcr/parse-file-name "/a/b/c/d/r.-23.0.mcr")
              :to-equal '(-23 . 0))
      (expect (mcr/parse-file-name "c:\\a\\b\\c\\r.-23.0.mcr")
              :to-equal '(-23 . 0)))
    (it "throws error if the file is not compliant"
      (expect (mcr/parse-file-name "r.23.2.mcg") :to-throw 'error)
      (expect (mcr/parse-file-name "r.a23.2.mcr") :to-throw 'error)
      (expect (mcr/parse-file-name "r.23a.2.mcr") :to-throw 'error)
      (expect (mcr/parse-file-name "r.23.2.3.mcr") :to-throw 'error)
      (expect (mcr/parse-file-name "r.23.2.a.mcr") :to-throw 'error)
      (expect (mcr/parse-file-name "ar.23.2.mcr") :to-throw 'error)
      (expect (mcr/parse-file-name "rx23x2xmcr") :to-throw 'error)))
  (describe "mcr/region-bound-chunks"
    (it "returns the expected values"
      (expect (mcr/region-bound-chunks 3)
              :to-equal '(96 . 127))
      (expect (mcr/region-bound-chunks -1)
              :to-equal '(-32 . -1))))
  (describe "mcr/region-bound-chunks"
    (it "returns the expected values"
      (expect (mcr/compute-chunk-range 3 -1)
              :to-equal '((96 . 127) . (-32 . -1)))))
  (describe "mcr/region-for-chunk"
    (it "returns the correct result for positive chunks"
      (expect (mcr/region-for-chunk 23) :to-be 0)
      (expect (mcr/region-for-chunk 32) :to-be 1)
      (expect (mcr/region-for-chunk 33) :to-be 1)
      (expect (mcr/region-for-chunk 64) :to-be 2))
    (it "returns the correct result for positive chunks"
      (expect (mcr/region-for-chunk -1) :to-be -1)
      (expect (mcr/region-for-chunk -32) :to-be -1)
      (expect (mcr/region-for-chunk -33) :to-be -2)
      (expect (mcr/region-for-chunk -64) :to-be -2)))
  (describe "mcr/read-file"
    (it "can read an mcr file without crashing"
      (expect (mcr/demo-something "test-data/r.0.0.mca")
              :not :to-throw 'error))))
