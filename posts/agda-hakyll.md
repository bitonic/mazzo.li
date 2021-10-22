---
title: Literate Agda and Hakyll
date: 2013-04-19
---

To render nicely my [previous post](/posts/AgdaSort.html) I have written [some
glue Haskell code](http://hackage.haskell.org/package/hakyll-agda) that
implements an Hakyll `Compiler` that supports Agda files.  The interesting
function is
[`pandocAgdaCompilerWith`](https://github.com/bitonic/hakyll-agda/blob/master/Hakyll/Web/Agda.hs#L169).
It takes configurations for the Pandoc reader and writer and for the Agda
compiler and produces HTML files.  The HTML is produced by interpreting the
non-code in the literate file as Markdown.

The code works but has some rough edges:

* The Agda files must be top level modules.
* An Agda interface file will be generated along the original literate file, so
  you must tell Hakyll to ignore said interface files (which end in `.agdai`).
* Hakyll metadata is not "filtered out", so it's better to include it as a
  separate `.metadata` file.
* Only links internal to the file will be generated.  Cross links between blog
  posts of the same site could easily be achieved.
* The generated code is in a `pre.Agda` block.  A css file (`css/agda.css`)
  adapted from the one distributed with Agda, is provided in the package.

An example of the package in action can be found in the
[Haskell code](https://github.com/bitonic/mazzo.li) that generates this website,
where [`site.hs`](https://github.com/bitonic/mazzo.li/blob/master/site.hs) is the
main generator and
[`posts/AgdaSort.lagda`](https://github.com/bitonic/mazzo.li/blob/master/posts/AgdaSort.lagda)
(and the respective metadata file) is a literate Agda file that generates the
post mentioned before.
