(require 'buttercup)
(require 'nbt-ieee754)

(describe "nbt-ieee754"
  (describe "nbt/hello-world"
    (it "returns the expected value"
      (nbt/check-environment)
      (expect (nbt-hello-world)
              :to-equal "Hello, World (from C)!"))))
