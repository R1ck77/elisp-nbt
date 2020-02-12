(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-tags)
(require 'nbt)

(describe "nbt"
  (describe "nbt/read-uncompressed-file"
    (it "can read the basic test"
      (let ((expected-result (make-nbt-compound :name "hello world"
                                                :items (list (make-nbt-string :name "name"
                                                                              :value "Bananrama")))))
        (expect (nbt/read-uncompressed-file "test-data/hello_world.nbt")
                :to-equal expected-result)))
    (it "can read the large test"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest-uncompressed.nbt")))
        (message "Thre result is: %s" structure)
        (expect structure :not :to-be nil)))))
