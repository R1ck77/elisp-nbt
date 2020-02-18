(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-objects)

;; https://www.emacswiki.org/emacs/read-float.el

(describe "nbt-objects"
  (describe "nbt/create-named-tag"
    (it "can read the first tag-compound"
      (with-small-suite
        (expect (nbt/create-named-tag)
                :to-equal (nbt-raw-compound :name "hello world")))))
  (describe "nbt/read-all-raw-tags"
    (it "can read the simple example"
      (with-small-suite
        (expect (nbt/read-all-raw-tags)
                :to-equal (list
                           (nbt-raw-compound :name "hello world")
                           (nbt-string :name "name" :value "Bananrama")
                           (nbt-end)))))))
