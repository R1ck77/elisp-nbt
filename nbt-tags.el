(require 'cl)

(cl-defstruct nbt-compound
  name
  items)

(cl-defstruct nbt-string
  name
  value)

(provide 'nbt-tags)
