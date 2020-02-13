(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-objects)

;; https://www.emacswiki.org/emacs/read-float.el

(describe "nbt-objects"
  (describe "nbt/create-tag"
    (it "can read the first tag-compound"
      (with-small-suite
        (expect (nbt/create-tag)
                :to-equal (nbt-raw-compound :id start-compound-tag-id
                                            :name "hello world")))))
  (describe "nbt/read-all-raw-tags"
    (it "can read the simple example"
      (with-small-suite
        (expect (nbt/read-all-raw-tags)
                :to-equal (list
                           (nbt-raw-compound :id start-compound-tag-id
                                             :name "hello world")
                           (nbt-string :id string-tag-id
                                       :name "name"
                                       :value "Bananrama")
                           (nbt-end :id end-tag-id)))))))
