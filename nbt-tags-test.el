(require 'buttercup)
(require 'nbt-tags)

;; https://www.emacswiki.org/emacs/read-float.el

(describe "nbt-tags"
  (describe "nbt/read-raw-tag"
    (it "can read the first tag-compound"
      (with-small-suite
        (expect (nbt/read-raw-tag)
                :to-equal (make-nbt-raw-compound :name "hello world"))))))
