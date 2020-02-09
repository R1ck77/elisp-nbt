(require 'buttercup)
(require 'nbt-tags)

;; https://www.emacswiki.org/emacs/read-float.el

(describe "nbt-tags"
  (describe "nbt/read-raw-tag"
    (it "can read the first tag-compound"
      (with-small-suite
        (expect (nbt/read-raw-tag)
                :to-equal (make-nbt-start-compound :name "hello world")))))
  (describe "nbt/read-all-raw-tags"
    (it "can read the simple example"
      (with-small-suite
        (expect (nbt/read-all-raw-tags)
                :to-equal (list
                           (make-nbt-start-compound :name "hello world")
                           (make-nbt-string :name "name"
                                            :value "Bananrama")
                           (make-nbt-end-compound)))))))
