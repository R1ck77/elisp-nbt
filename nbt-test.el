(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-objects)
(require 'nbt)

(describe "nbt"
  (describe "nbt/read-uncompressed-file"
    (it "can read the basic test"
      (let ((expected-result (nbt-compound :id start-compound-tag-id
                                           :name "hello world"
                                           :value (list (nbt-string :id string-tag-id
                                                                    :name "name"
                                                                    :value "Bananrama")))))
        (expect (nbt/read-uncompressed-file "test-data/hello_world.nbt")
                :to-equal expected-result)))
    (it "can read the large test"
      (let ((structure (nbt/read-uncompressed-file "test-data/bigtest-uncompressed.nbt")))
        (message "Thre result is: %s" structure)
        (expect structure :not :to-be nil)))))
