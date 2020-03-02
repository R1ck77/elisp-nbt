## Emacs LISP NBT library

Basic Emacs Lisp library to read/write Minecraft **Named Binary Tag** format files.

This library  uses a C module for long/double/float parsing which could, in principle, be replaced with a pure Emacs LISP IEEE-754 implementation, but it's a lot work and not a funny thing to do, so you are stuck with the C module :)

It can currentl read the examples found here

    https://wiki.vg/NBT
    
as well as one of my Minecraft level.dat files, so it's bug free of course! :)

**NBT writing/updating is not yet supported**

## Requirements

You will need:

- Emacs with dynamic module loading compiled in (you will probably need to compile your own Emacs from source)
- GNU make, GCC and linux if you want to compile the library automatically

## Installation

Run `make` to compile the library and run the automatic tests.

To read a NBT file open `nbt.el` and, assuming you are in the library folder:

    (setq load-path (cons "." load-path))
    (require 'nbt)
    (nbt/read-file "path to the nbt file")

should return a hierarchy of simple EIEIO objects with `name` and `value` fields.

Note that, due to the limitations of Emacs primitives, precision may be lost on float values, and long tags are read as string representations.

## Disclaimer

Minecraft and possibly other names/trademarks are properties of their own respective trademark owners. I am in no way related or associated to Mojang, Microsoft or any of the brands indirectly involved by this software.

## License

Copyright © 2019 Riccardo Di Meo

This work is distributed under the terms of the Creative Commons Attribution 4.0 International Public License.

The full text of the license can be found at the URL https://creativecommons.org/licenses/by/4.0/legalcode


