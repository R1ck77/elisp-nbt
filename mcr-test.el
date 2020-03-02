(require 'buttercup)
(require 'dash)
(require 'nbt-test-utils)

(describe "mcr"
  (describe "mcr/read-file"
    (it "can read an mcr file without crashing"
      (expect (mcr/read-file "test-data/hello_world.nbt")
              :not :to-be nil))))
