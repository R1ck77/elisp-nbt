(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-objects)

;; https://www.emacswiki.org/emacs/read-float.el

(describe "nbt-objects"
  (describe "nbt/create-named-tag"
    (it "can read the first tag-compound"
      (with-small-suite
        (expect (oref (nbt/create-named-tag) name)
                :to-equal "hello world"))))
  (describe "nbt/read-all-raw-tags"
    (it "can read the simple example"
      (with-small-suite
        (expect (nbt/read-all-raw-tags)
                :to-equal (nbt-compound :name "hello world"
                                        :value (list (nbt-string :name "name" :value "Bananrama"))))))))
