(require 'buttercup)
(require 'nbt-native)

(describe "nbt-native"
  (describe "nbt/hello-world"
    (it "returns the expected value"
      (nbt/check-environment)
      (expect (nbt-hello-world)
              :to-equal "Hello, World (from C)!")))
  (describe "nbt/convert-bytes-to-float"
    (it "converts float data correctly"
      (nbt/check-environment)
      (expect (nbt/convert-bytes-to-float (list 65 64 0 0))
              :to-equal 12.0)))
  (describe "nbt/convert-bytes-to-double"
    (it "converts double data correctly"
      (nbt/check-environment)
      (expect (nbt/convert-bytes-to-double (list 65 -99 111 52 84 0 0 0))
              :to-equal 123456789.0))))
