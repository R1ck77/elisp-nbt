(require 'buttercup)
(require 'nbt-test-utils)
(require 'nbt-objects)

(defvar compound-sample (nbt-compound :name "compound"
                                      :value (list (nbt-string :name "string" :value "string value")
                                                   (nbt-int :name "int" :value 23))))
(defvar similar-compound-sample (nbt-compound :name "compound"
                                              :value (list (nbt-string :name "string" :value "string value")
                                                           (nbt-int :name "int" :value 23))))
(defvar compound-sample-different-items (nbt-compound :name "compound"
                                                      :value (list (nbt-string :name "string" :value "string value")
                                                                   (nbt-long :name "long" :value "1984"))))
(defvar compound-sample-different-count (nbt-compound :name "compound"
                                                      :value (list (nbt-string :name "string" :value "string value")
                                                                   (nbt-int :name "int" :value 23)
                                                                   (nbt-string :name "other string" :value "other value"))))

(defmacro test-equality (class value copy-value other-value)
  `(describe ,(symbol-name class)
     (it "different types"
       (expect (nbt-equal (,class :name "a name" :value ,value)
                          (nbt-end))
               :to-be nil)        )
     (it "positive case"
       (expect (nbt-equal (,class :name "a name" :value ,value)
                          (,class :name "a name" :value ,copy-value))
               :to-be t))
     (it "different name"
       (expect (nbt-equal (,class :name "a name" :value ,value)
                          (,class :name "another name" :value ,copy-value))
               :to-be nil))
     (it "different value"
       (expect (nbt-equal (,class :name "a name" :value ,value)
                          (,class :name "a name" :value ,other-value))
               :to-be nil))))

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
                                       :value (list (nbt-string :name "name" :value "Bananrama")))))))
  (describe "nbt-equal"
    (it "compares correctly nbt-end types"
      (expect (nbt-equal (nbt-end) (nbt-end)))
      (expect (nbt-equal (nbt-end) (nbt-string)) :to-be nil))
    (test-equality nbt-string "a string" "a string" "another string")
    (test-equality nbt-byte 12 12 14)
    (test-equality nbt-short 12 12 14)
    (test-equality nbt-int 12 12 14)
    (test-equality nbt-long "12" "12" "14")
    (test-equality nbt-float 12.0 12.0 14.0)
    (test-equality nbt-double 12.0 12.0 14.0)
    (test-equality nbt-byte-array (list 1 2 3) (list 1 2 3) (list 1 2 3 4))
    (test-equality nbt-int-array (list 1 2 3) (list 1 2 3) (list 1 2 3 4))
    (test-equality nbt-long-array (list "1" "2" "3") (list "1" "2" "3") (list "1" "2" "3" "4"))
    (describe "nbt-compound"
      (it "is equal to a compound with similar name and items"
        (expect (nbt-equal compound-sample similar-compound-sample) :to-be t)
        (expect (nbt-equal (nbt-compound :name "a"
                                         :value '())
                           (nbt-compound :name "a"
                                         :value '()))
                :to-be t))
      (it "is different from a tag of different id"
        (expect (nbt-equal (nbt-compound)
                           (nbt-end)) :to-be nil))
      (it "is different when the name is different"
        (expect (nbt-equal (nbt-compound :name "a name"
                                         :value nil)
                           (nbt-compound :name "another name"
                                         :value nil)) :to-be nil))
      (it "is different when the number of sub-tags is different"
        (expect (nbt-equal compound-sample
                           compound-sample-different-count)
                :to-be nil))
      (it "is different when the subtags are different"
        (expect (nbt-equal compound-sample
                           compound-sample-different-items)
                :to-be nil)))))


;;; TODO/FIXME equality tests/implementation for lists, compounds etc
