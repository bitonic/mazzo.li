---
title: "Building inline-c projects just got a lot easier"
date: 2017-07-23
tags: [post, short]
---

TL;DR: the latest version of `inline-c`,
[`0.6.0.0`](http://hackage.haskell.org/package/inline-c), does not
require manual specification of generated C files. Moreover, `ghci` now
works, provided you use `-fobject-code`. The build process as a whole is
much more reliable.

GHC 8.2.1 just came out, and it contains a new template Haskell function
that I added specifically to make `inline-c` more of a first-class
citizen:
[`addForeignFile`](http://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Syntax.html#v:addForeignFile).

This function lets you emit bits of foreign code that will be linked
with the object code for the current module.

The new `inline-c` now uses this function, which means that compiling
Haskell code with inline C expressions will "just work", without
compiling the generated C files explicitly or without specifying the
generated C files in the cabal file.

You can also use `ghci` as you would normally, provided that you pass
`-fobject-code` to it. We reccomend using `ghci -fobject-code -O0` for
speed. `-fobject-code` is needed because `addDependentFile` does not
work when loading a module in interpreted mode. I plan to fix this in
the near future, and after that happens I'll probably consider
`inline-c` "done", since it's working out very well for the projects
that use it with no API change since the first release.
