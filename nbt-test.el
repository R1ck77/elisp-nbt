(require 'buttercup)
(require 'dash)
(require 'nbt-test-utils)
(require 'nbt-objects)
(require 'nbt)

(defun debug-term-f (n)
  (% (+ (* n n 255)
        (* n 7))
     100))

(defconst long-byte-values (-map #'debug-term-f (number-sequence 0 999)))

(defconst nested-compound-test
  (nbt-compound :name "nested compound test"
                :value (list
                        (nbt-compound :name "egg"
                                      :value (list
                                              (nbt-string :name "name"
                                                          :value "Eggbert")
                                              (nbt-float :name "value"
                                                         :value 0.5)))
                        (nbt-compound :name "ham"
                                      :value (list
                                              (nbt-string :name "name"
                                                          :value "Eggbert")
                                              (nbt-float :name "value"
                                                         :value 0.5))))))

;;; TODO/FIXME long values not supported
;;; TODO/FIXME double and float values not supported
(defconst big-result
  (nbt-compound :value (list
                        (nbt-long :value 4294967295 :name "longTest")
                        (nbt-short :value 32767 :name "shortTest")
                        (nbt-string :value "HELLO WORLD THIS IS A TEST STRING \303\205\303\204\303\226!" :name "stringTest")
                        (nbt-float :value 0.49823147 :name "floatTest")
                        (nbt-int :value 2147483647 :name "intTest")
                        (nbt-compound :value (list (nbt-compound :value (list (nbt-string :value "Hampus" :name "name")
                                                                              (nbt-float :value 0.75 :name "value"))
                                                                 :name "ham")
                                                   (nbt-compound :value (list (nbt-string :value "Eggbert" :name "name")
                                                                              (nbt-float :value 0.5 :name "value"))
                                                                 :name "egg"))
                                      :name "nested compound test")
                        (nbt-list :value (list (nbt-long :value 11 :name "")
                                               (nbt-long :value 12 :name "")
                                               (nbt-long :value 13 :name "")
                                               (nbt-long :value 14 :name "")
                                               (nbt-long :value 15 :name ""))
                                  :name "listTest (long)")
                        (nbt-list :value (list (nbt-compound :value (list (nbt-string :value "Compound tag #0" :name "name")
                                                                          (nbt-long :value 1379390861 :name "created-on"))
                                                             :name "")
                                               (nbt-compound :value (list (nbt-string :value "Compound tag #1" :name "name")
                                                                          (nbt-long :value 1379390861 :name "created-on"))
                                                             :name ""))
                                  :name "listTest (compound)")
                        (nbt-byte :value 127 :name "byteTest")
                        (nbt-byte-array :value (list 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48 0 62 34 16 8 10 22 44 76 18 70 32 4 86 78 80 92 14 46 88 40 2 74 56 48 50 62 84 16 58 10 72 44 26 18 20 32 54 86 28 80 42 14 96 88 90 2 24 56 98 50 12 84 66 58 60 72 94 26 68 20 82 54 36 28 30 42 64 96 38 90 52 24 6 98 0 12 34 66 8 60 22 94 76 68 70 82 4 36 78 30 92 64 46 38 40 52 74 6 48)
                                        :name "byteArrayTest (the first 1000 values of (n*n*255+n*7)%100, starting with n=0 (0, 62, 34, 16, 8, ...))")
                        (nbt-double :value 0.4931287132182315 :name "doubleTest"))
                :name "Level"))

(describe "nbt"
  (describe "nbt/exit-if-zlib-absent"
    (it "throws an error if zlib is missing"
      (spy-on 'zlib-available-p :and-return-value nil)
      (expect (nbt/exit-if-zlib-absent) :to-throw 'error))
    (it "does nothing if zlib is present"
      (spy-on 'zlib-available-p :and-return-value t)
      (expect (nbt/exit-if-zlib-absent) :not :to-throw 'error)))
  (describe "nbt/read-uncompressed-file"
    (it "can read the basic test"
      (let ((expected-result (nbt-compound :name "hello world"
                                           :value (list (nbt-string :name "name"
                                                                    :value "Bananrama")))))
        (expect (nbt/read-uncompressed-file "test-data/hello_world.nbt")
                :to-equal expected-result)))
    (it "can read the large test"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest_uncompressed.nbt")))
        (expect structure :not :to-be nil)))
    (it "can read the large test CORRECTLY"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest_uncompressed.nbt")))
        (expect structure :to-equal big-result))))
  (describe "nbt/read-compressed-file"
    (it "can read the basic test"
      (let ((expected-result (nbt-compound :name "hello world"
                                           :value (list (nbt-string :name "name"
                                                                    :value "Bananrama")))))
        (expect (nbt/read-compressed-file "test-data/hello_world_compressed.nbt")
                :to-equal expected-result)))
    (it "can read the large test"
      (let ((structure (nbt/read-compressed-file "test-data/bigtest_compressed.nbt")))
        (expect structure :not :to-be nil)))
    (xit "can read the large test CORRECTLY"
      (let ((structure (nbt/read-compressed-file "test-data/bigtest_compressed.nbt")))
        (expect structure :to-equal big-result))))
  (describe "nbt/file-compressed-p"
    (it "returns t for compressed files"
      (expect (nbt/file-compressed-p "test-data/bigtest_compressed.nbt") :to-be t))
    (it "returns nil for uncompressed files"
      (expect (nbt/file-compressed-p "test-data/bigtest_uncompressed.nbt") :to-be nil)))
  (describe "nbt/read-file"
    (it "uses nbt/read-uncompressed-file if the file is not compressed"
      (spy-on 'nbt/read-uncompressed-file :and-call-through)
      (spy-on 'nbt/read-compressed-file :and-call-through)
      (expect (nbt/read-file "test-data/bigtest_uncompressed.nbt") :not :to-throw 'error)
      (expect 'nbt/read-uncompressed-file :to-have-been-called-times 1)
      (expect 'nbt/read-compressed-file :not :to-have-been-called))
    (it "uses nbt/read-compressed-file if the file is compressed"
      (spy-on 'nbt/read-uncompressed-file :and-call-through)
      (spy-on 'nbt/read-compressed-file :and-call-through)
      (expect (nbt/read-file "test-data/bigtest_compressed.nbt") :not :to-throw 'error)
      (expect 'nbt/read-compressed-file :to-have-been-called-times 1)
      (expect 'nbt/read-uncompressed-file :not :to-have-been-called))
    (it "can read a Minecraft™ level file"
      (let ((content (nbt/read-file "test-data/level.dat")))
        (expect content :not :to-be nil)))))
