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
  (describe "nbt/read-uncompressed-file"
    (it "can read the basic test"
      (let ((expected-result (nbt-compound :name "hello world"
                                           :value (list (nbt-string :name "name"
                                                                    :value "Bananrama")))))
        (expect (nbt/read-uncompressed-file "test-data/hello_world.nbt")
                :to-equal expected-result)))
    (it "can read the large test"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest-uncompressed.nbt")))
        (message "Thre result is: %s" structure)
        (expect structure :not :to-be nil)))
    (xit "can read the large test CORRECTLY"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest-uncompressed.nbt")))
        (message "Thre result is: %s" structure)
        (expect structure :to-equal big-result)))))
