---
title: Flame graphs for GHC time profiles
date: 2015-04-27
---

*This article was originally published on the
 [FPComplete blog](https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph).*

GHC comes with a number of nice
[profiling facilities](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html).
Among other things, GHC can generate time profiles, a useful facility for answering the following question: "where in the source code is my program spending all its CPU time?". With the right flags turned on, GHC's RTS dumps a time profile in a `.prof` file when your program exits, providing textual summary and detailed views of the program's runtime, broken down by cost centre.

However, in large programs these `.prof` files can become quite hard to
make sense of.  Visualizing profiling data is a common problem, and one
neat solution is to use
[flame graphs](http://www.brendangregg.com/flamegraphs.html) to get a
high-level view of where time is spent, and why it is spent there.
That's why we wrote
[`ghc-prof-flamegraph`](https://github.com/fpco/ghc-prof-flamegraph), a
new utility useful for turning textual `.prof` reports into a pretty
picture (click on the image to get to the interactive SVG):

<a href="https://s3.amazonaws.com/download.fpcomplete.com/francesco/binary-trees.svg">
  <img src="https://s3.amazonaws.com/download.fpcomplete.com/francesco/binary-trees.svg" alt="Flame graph for binary-trees.hs">
</a>

In the figure above we have the flame graph for a run of a small Haskell
program, which we will describe later.  Paraphrasing the description of flame graphs
from the website:

> The x-axis shows the stack profile, sorted alphabetically
> (it is not the passage of time), and the y-axis shows stack
> depth. Each rectangle represents a cost center. The wider the rectangle is
> is, the more time is spent in that cost centre or its descendants.
> Cost centers often represent function calls, in which case each rectangle
> can be thought of as a stack frame in the call stack. The top edge shows
> what is on-CPU, and beneath it is its ancestry. The colors are usually
> not significant, picked randomly to differentiate frames.

Notice how the generated SVG image is interactive.  Hovering over a stack
frame gives us more information about it, and double clicking on it we
can drill down that particular code path.

## Installation

Installation is easy:

    $ cabal install ghc-prof-flamegraph

You'll also need the
[FlameGraph](https://github.com/brendangregg/FlameGraph) scripts to
produce SVG files.  I will assume that the `flamegraph.pl` script is in
the `$PATH`, but it can also be called from some other location.

## Usage

(Example taken from from
<http://jaspervdj.be/posts/2014-02-25-profiteur-ghc-prof-visualiser.html>)

Using
[an example](https://s3.amazonaws.com/download.fpcomplete.com/francesco/binary-trees.hs)
from the [Haskell wiki](https://wiki.haskell.org/Shootout/Binary_trees),
we first compile it using profiling options:

    $ ghc --make -auto-all -prof -rtsopts binary-trees.hs
    [1 of 1] Compiling Main             ( binary-trees.hs, binary-trees.o )
    Linking binary-trees ...

Then we run it enabling time profiling:

    $ ./binary-trees 15 +RTS -p -RTS
    stretch tree of depth 16	 check: -1
    65536	 trees of depth 4	 check: -65536
    16384	 trees of depth 6	 check: -16384
    4096	 trees of depth 8	 check: -4096
    1024	 trees of depth 10	 check: -1024
    256	 trees of depth 12	 check: -256
    64	 trees of depth 14	 check: -64
    long lived tree of depth 15	 check: -1

Which will generate `binary-trees.prof`.  Now we can use
`ghc-prof-flamegraph` to convert it into a format understandable by
`flamegraph.pl`:

    $ cat binary-trees.prof | ghc-prof-flamegraph > binary-trees.folded

and finaly use `flamegraph.pl` to convert it to an interactive SVG
image:

    $ cat binary-trees.folded | flamegraph.pl > binary-trees.svg

The result is shown at the beginning of the post.  Note that
`flamegraph.pl` assumes the data is derived from sampling the execution
of the program, and thus `ghc-prof-flamegraph` uses a fictitious numbers
for the number of entries of each stack frame, derived from the
individual time as reported in the `.prof` file.

## A larger example

Let's scale this up to a larger application: consider
[this `.prof` file](https://s3.amazonaws.com/download.fpcomplete.com/francesco/hoogle.prof),
resulting from running `hoogle generate`, and the resulting flame graph:

<a href="https://s3.amazonaws.com/download.fpcomplete.com/francesco/hoogle.svg">
  <img src="https://s3.amazonaws.com/download.fpcomplete.com/francesco/hoogle.svg" alt="Flame graph for hoogle">
</a>

Looking at the flame graph we are immediately able to understand the two
code paths that take the vast majority of the time:
`Input.Hoogle.parseHoogle` and `General.Store.storeWriteFile`.  We are
then able to drill down on each path by double clicking on it to explore
where time is spent in detail.  On the other hand, if we want to examine
the `.prof` file directly, we can quickly identify the hotspots:

    myParseDecl       Input.Type       29.3   21.8
    writeItems.\.\.bs Output.Items     22.9   21.9
    pretty            General.Util     13.5   15.4

but we need to manually chase down their occurrences in the `.prof` file
to understand where these functions are being call: `myParseDecl` occurs
twice, `writeItems.\.\.bs` only once, and `pretty` 7 times.  It is often
the case that the hotspots are even more fragmented, making them even
harder to interpret.
