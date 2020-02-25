(require 'buttercup)
(require 'nbt-ieee754)

(describe "nbt-ieee754"
  (describe "nbt/hello-world"
    (it "returns the expected value"
      (nbt/check-environment)
      (expect (nbt-hello-world)
              :to-equal "Hello, World (from C)!")))
  (describe "nbt/convert-bytes-to-float"
    (it "does something"
      (nbt/check-environment)
      (expect (nbt/convert-bytes-to-float (list 1 2 3 4))
              :to-equal 12.0))))
