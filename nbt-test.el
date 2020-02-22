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

(defconst big-result
  (nbt-compound :name "Level"
                :value (list
                        nested-compound-test
                        (nbt-int :name "intTest"
                                 :value 2147483647)
                        (nbt-byte :name "byteTest"
                                  :value 127)
                        (nbt-string :name "stringTest"
                                    :value "HELLO WORLD THIS IS A TEST STRING \xc3\x85\xc3\x84\xc3\x96!")
                        (nbt-list :name "listTest (long)"
                                  :value (list ;; TODO/FIXME in the example the name is unbound! This may create problems during the write phase
                                          (nbt-long :name "" :value 11)
                                          (nbt-long :name "" :value 12)
                                          (nbt-long :name "" :value 13)
                                          (nbt-long :name "" :value 14)
                                          (nbt-long :name "" :value 15)))
                        (nbt-double :name "doubleTest"
                                    :value 0.49312871321823148)
                        (nbt-float :name "floatTest"
                                   :value 0.49823147058486938)
                        (nbt-long :name "longTest"
                                  :value 9223372036854775807)
                        (nbt-list :name "listTest (compound)"
                                  :value (list
                                          (nbt-compound :name ""
                                                        :value (list
                                                                (nbt-long :name "created-on"
                                                                          :value 1264099775885)
                                                                (nbt-string :name "name"
                                                                            :value "Compound tag #0")))
                                          (nbt-compound :name ""
                                                        :value (list
                                                                (nbt-long :name "created-on"
                                                                          :value 1264099775885)
                                                                (nbt-string :name "name"
                                                                            :value "Compound tag #1")))))
                        (nbt-byte-array :name "byteArrayTest"
                                        :value long-byte-values)
                        (nbt-short :name "shortTest"
                                   :value 32767))))

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
    (xit "can read the large test CORRECTLY"
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
