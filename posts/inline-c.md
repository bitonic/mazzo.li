---
title: "`inline-c`: Call C functions from Haskell without bindings"
date: 2015-05-20
tags:
- post
- talk
---

**Update**: I gave <a href="https://skillsmatter.com/skillscasts/6546-call-c-functions-from-haskell-without-bindings">a talk</a> at the Haskell eXchange 2015 on this topic.

***

*This article was originally published on the
 [FPComplete blog](https://www.fpcomplete.com/blog/2015/05/inline-c).*

Because Haskell is a language of choice for many problem domains, and
for scales ranging from one-off scripts to full scale web services, we
are fortunate to by now have over 8,000 open source packages (and
a few commercial ones besides) available to build from. But in
practice, Haskell programming in the real world involves interacting
with myriad legacy systems and libraries. Partially because the
industry is far older than the comparatively recent strength of our
community. But further still, because quality new high-performance
libraries are created every day in languages other than Haskell, be it
intensive numerical codes or frameworks for shuffling bits across
machines. Today we are releasing `inline-c`, a package for writing
mixed C/Haskell source code that seamlessly invokes native and foreign
functions in the same module. No FFI required.

## The joys of programming with foreign code

Imagine that you just found a C library that you wish to use for your
project. The standard workflow is to,

1. check Hackage if a package with a set of bindings for that library
   exists,
1. if one does, program against that, or
1. if it doesn't, write your own bindings package, using Haskell's
   FFI.

Writing *and maintaining* bindings for large C libraries is hard work.
The libraries are constantly updated upstream, so that the bindings
you find are invariably out-of-date, providing only partial coverage
of library's API, sometimes don't compile cleanly against the latest
upstream version of the C library or need convoluted and error-prone
conditional compilation directives to support multiple versions of the
API in the package. Which is a shame, because typically you only need
to perform a very specific task using some C library, using only
a minute proportion of its API. It can be frustrating for a bindings
package to fail to install, only because the binding for some function
that you'll never use doesn't match up with the header files of the
library version you happen to have installed on your system.

This is especially true for large libraries that expose sets of
related but orthogonal and indepedently useful functions, such as
GTK+, OpenCV or numerical libraries such as the GNU Scientific Library
(GSL), NAG and IMSL. `inline-c` lets you call functions from these
libraries using the full power of C's syntax, directly from client
code, without the need for monolithic bindings packages. High-level
bindings (or "wrappers") may still be useful to wrap low-level details
into an idiomatic Haskell interface, but `inline-c` enables rapid
prototyping and iterative development of code that uses directly some
of the C library today, keeping for later the task of abstracting
calls into a high-level, type safe wrapper as needed. In short,
`inline-c` let's you "pay as you go" when programming foreign code.

We first developed `inline-c` for use with numerical libraries, in
particular the popular and very high quality commercial
[NAG library](http://www.nag.com/numeric/CL/CLdescription.asp), for
tasks including ODE solving, function optimization, and interpolation.
If getting seamless access to the gold standard of fast and reliable
numerical routines is what you need, then you will be interested in
our companion package to work specifically with NAG,
[`inline-c-nag`](https://github.com/fpco/inline-c-nag).

## A taste of `inline-c`

What follows is just a teaser of what can be done with `inline-c`.
Please refer to the
[Haddock documentation](http://hackage.haskell.org/package/inline-c) and
the [README](https://github.com/fpco/inline-c/blob/master/README.md) for
more details on how to use the showcased features.

Let's say we want to use C's variadic `printf` function and it's
convenient string formats. `inline-c` let's you write this function
call inline, without any need for a binding to the foreign function:

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "<math.h>"

main :: IO ()
main = do
   x <- [C.exp| int{ printf("Some number: %.2f\n", cos(0.5)) } |]
   putStrLn $ show x ++ " characters printed."
```

Importing `Language.C.Inline` brings into scope the Template Haskell
function `include` to include C headers (`<stdio.h>` and `<math.h`),
and the `exp` quasiquoter for embedding expressions in C syntax in
Haskell code. Notice how `inline-c` has no trouble even with
C functions that have advanced calling conventions, such as variadic
functions. This is a crucial point: we have the full power of
C available at our fingertips, not just whatever can shoe-horned
through the FFI.

We can capture Haskell variables to be used in the C expression, such
as when computing `x` below:

```haskell
mycos :: CDouble -> IO CDouble
mycos x = [C.exp| double{ cos($(double x)) } |]
```

The anti-quotation `$(double x)` indicates that we want to capture the
variable `x` from the Haskell environment, and that we want it to have
type `double` in C (`inline-c` will check at compile time that this is
a sensible type ascription).

We can also splice in a block of C statements, and explicitly `return`
the result:

```haskell
C.include "<stdio.h>"

-- | @readAndSum n@ reads @n@ numbers from standard input and returns
-- their sum.
readAndSum :: CInt -> IO CInt
readAndSum n = do
  x <- [C.block| int {
      int i, sum = 0, tmp;
      for (i = 0; i < $(int n); i++) {
        scanf("%d ", &tmp);
        sum += tmp;
      }
      return sum;
    } |]
  print x
```

Finally, the library provides facilities to easily use Haskell data in
C. For example, we can easily use Haskell `ByteString`s in C:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Foreign.C.Types
import qualified Language.C.Inline as C
import           Text.RawString.QQ (r)

C.context (C.baseCtx <> C.bsCtx)

-- | Count the number of set bits in a 'BS.ByteString'.
countSetBits :: BS.ByteString -> IO CInt
countSetBits bs = [C.block|
    int {
      int i, bits = 0;
      for (i = 0; i < $bs-len:bs; i++) {
        char ch = $bs-ptr:bs[i];
        bits += (ch * 01001001001ULL & 042104210421ULL) % 017;
      }
      return bits;
    }
  |]
```

In this example, we use the `bs-len` and `bs-ptr` anti-quoters to get
the length and pointer for a Haskell `ByteString`. `inline-c` has
a modular design: these anti-quoters are completely optional and can
be included on-demand. The `C.context` invocation adds the extra
`ByteString`s anti-quoters to the base set. Similar facilities are
present to easily use Haskell `Vector`s as well as for invoking
Haskell closures from C code.

### Larger examples

We have included various examples in the
[`inline-c`](https://github.com/fpco/inline-c/) and
[`inline-c-nag`](https://github.com/fpco/inline-c-nag) repositories.
Currently they're geared toward scientific and numerical computing,
but we would welcome contributions using `inline-c` in other fields.

For instance,
[`gsl-ode.hs`](https://github.com/fpco/inline-c/blob/master/inline-c/examples/gsl-ode.hs)
is a great example of combining the strengths of C and the strengths
of Haskell to good effect: we use a function from C's
[GNU Scientific Library](http://www.gnu.org/software/gsl/gsl.html) for
solving ordinary differential equations (ODE) to solve
a [Lorenz system](http://en.wikipedia.org/wiki/Lorenz_system), and
then take advantage of the very nice `Chart-diagrams` Haskell library
to display its x and z coordinates:

![Lorenz system](/assets/images/lorenz.png)

In this example, the `vec-ptr` anti-quoter is used to get a pointer out
of mutable vector:

```
$vec-ptr:(double *fMut)
```

Where `fMut` is a variable of type `Data.Storable.Vector.Mutable.Vector
CDouble`.  Moreover, the `fun` anti-quoter is used to get a function
pointer from a Haskell function:

```
$fun:(int (* funIO) (double t, const double y[], double dydt[], void * params))
```

Where, `funIO` is a Haskell function of type

```haskell
CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr () -> IO CInt
```

Note that all these anti-quoters (apart from the ones where only one
type is allowed, like `vec-len` or `bs-ptr`) force the user to specify
the target C type. The alternative would have been to write the
Haskell type. Either way *some* type ascription is unfortunately
required, due to a limitation of Template Haskell. We choose C type
annotations because in this way, the user can understand precisely and
state explicitly the target type of any marshalling.

Note that at this stage, type annotations are needed, because it is
[not possible to get the type of locally defined variables in Template Haskell](https://mail.haskell.org/pipermail/ghc-devs/2015-February/008327.html).

## How it works under the hood

`inline-c` generates a piece of C code for most of the Template Haskell
functions and quasi-quoters function that it exports.  So when you write

```
[C.exp| double{ cos($(double x)) } |]
```

a C function gets generated:

```c
double some_name(double x) {
    return cos(x);
}
```

This function is then bound to in Haskell through an automatically
generated FFI import declaration and invoked passing the right argument
-- the `x` variable from the Haskell environment. The types specified in
C are automatically translated to the corresponding Haskell types, to
generate the correct type signatures.

Custom anti quoters, such as `vec-ptr` and `vec-len`, handle the C and
Haskell types independently.  For example, when writing

```
[C.block| double {
    int i;
    double res;
    for (i = 0; i < $vec-len:xs; i++) {
      res += $vec-ptr:(double *xs)[i];
    }
    return res;
  } |]
```

we'll get a function of type

```c
double some_name(int xs_len, double *xs_ptr)
```

and on the Haskell side the variable `xs` will be used in conjuction
with some code getting its length and the underlying pointer, both to be
passed as arguments.

### Building programs that use `inline-c`

The C code that `inline-c` generates is stored in a file named like the
Haskell source file, but with a `.c` extension.

When using cabal, it is enough to specify generated C source, and
eventual options for the C code:

    executable foo
      main-is:             Main.hs, Foo.hs, Bar.hs
      hs-source-dirs:      src
      -- Here the corresponding C sources must be listed for every module
      -- that uses C code.  In this example, Main.hs and Bar.hs do, but
      -- Foo.hs does not.
      c-sources:           src/Main.c, src/Bar.c
      -- These flags will be passed to the C compiler
      cc-options:          -Wall -O2
      -- Libraries to link the code with.
      extra-libraries:     -lm
      ...

Note that currently `cabal repl` is not supported, because the C code is
not compiled and linked appropriately.  However, `cabal repl` will fail
at the end, when trying to load the compiled C code, which means that we
can still use it to type check our package when developing.

If we were to compile the above manaully we could do
    
    $ ghc -c Main.hs
    $ cc -c Main.c -o Main_c.o
    $ ghc Foo.hs
    $ ghc Bar.hs
    $ cc -c Bar.c -o Bar_c.o
    $ ghc Main.o Foo.o Bar.o Main_c.o Bar_c.o -lm -o Main

### Extending `inline-c`

As mentioned previously, `inline-c` can be extended by defining custom
anti-quoters. Moreover, we can also tell `inline-c` about more C types
beyond the primitive ones.

Both operations are done via the `Context` data type.  Specifically, the
`Context` contains a `TypesTable`, mapping C type specifiers to Haskell
types; and a `Map` of `AntiQuoter`s.  A `baseCtx` is provided specifying
mappings from all the base C types to Haskell types (`int` to `CInt`,
`double` to `CDouble`, and so on).  `Context`s can be composed using
their `Monoid` instance.

For example, the `vecCtx` contains two anti-quoters, `vec-len` and
`vec-ptr`.  When using `inline-c` with external libraries we often
define a context dedicated to said library, defining a `TypesTable`
converting common types find in the library to their Haskell
counterparts.  For example
[`inline-c-nag`](https://github.com/fpco/inline-c-nag) defines a context
containing information regarding the types commonly using in the NAG
scientific library.

See the `Language.C.Inline.Context` module documentation for more.

### C++ support

Our original use case for `inline-c` was always C oriented. However,
thanks to extensible contexts, it should be possible to build C++
support on top of `inline-c`, as we dabbled with in
[`inline-c-cpp`](https://github.com/fpco/inline-c-cpp). In this way,
one can mix C++ code into Haskell source files, while reusing the
infrastructure that `inline-c` provides for invoking foreign
functions. Since `inline-c` generates C wrapper functions for all
inline expressions, one gets a function with *bona fide* C linkage to
wrap a C++ call, for free. Dealing with C++ templates, passing C++
objects in and out and conveniently manipulating them from Haskell are
the next challenges. If C++ support is what you need, feel free to
contribute to this ongoing effort!

## Wrapping up

We meant `inline-c` as a simple, modular alternative to monolithic
binding libraries, borrowing the core concept of FFI-less programming of
foreign code from the
[H project](https://ifl2014.github.io/submissions/ifl2014_submission_16.pdf)
and
[language-c-inline](https://github.com/mchakravarty/language-c-inline). But
this is just the first cut! We are releasing the library to the
community early in hopes that it will accelerate the Haskell community's
embrace of quality foreign libraries where they exist, as an alternative
to expending considerable resources reinventing such libraries for
little benefit.  Numerical programming, machine learning, computer
vision, GUI programming and data analysis come to mind as obvious areas
where we want to leverage existing quality code. In fact, FP Complete is
using `inline-c` today to enable quick access to all of NAG, a roughly
1.6K function strong library, for a large compute-intensive codebase. We
hope to see many more use cases in the future.


