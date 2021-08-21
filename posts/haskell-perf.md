---
title: perf for low-level Haskell profiling
date: 2015-07-05
---

*This article was originally published on the
[School of Haskell](https://www.fpcomplete.com/user/bitonic/perf-for-low-level-profiling).*

**TL;DR:** `perf` can be particularly helpful when profiling performance
critical sections of your programs.  We show how through an example.

The `perf_events` framework in the linux kernel, together with the
`perf` userland tools, can be used to profile applications without any
instrumentation -- beyond compiling them with debug symbols.  The debug
symbols are specified in the DWARF format.  Recently, support was added
to GHC to support DWARF debug symbols, thanks to work by Peter Wortmann.

In this article we go through the process of understanding the
performance of a low-level Haskell program -- a function computing the
CRC32 digest of a `ByteString`.  We compare a pure Haskell version,
provided by the `digest-pure` package; to a version using a C function
provided by `zlib`, provided by the `digest` package.  After we
understand why the Haskell version is slower we modify it to make it
almost at fast as the C one.

Note that this document is meant to be a showcase of what can be done
with `perf`, rather than a tutorial on how to make an Haskell program as
fast as a C one.  In this case, the shortest route would have been to
directly replicate the behaviour of the C program in Haskell: we use
`perf` to show how it's possible to understand precisely where and why
time is being spent.

## The setup

### Building Haskell libraries with DWARF

Getting up to speed with `perf` and Haskell is easy but tedious and
undocumented.  First, GHC HEAD (7.11) is needed, for DWARF support.  GHC
7.10.1 has partial DWARF support, but some features are broken, so GHC
HEAD is recommended.  The issues with GHC 7.10.1 are resolved in GHC 7.10.2, which will
be released shortly.

Moreover, the RTS and the libraries built as part of the GHC build
process should be built with debug symbols.  This can be done by adding

    GhcRtsHcOpts += -g
    GhcLibHcOpts += -g

to your `mk/build.mk` file in the `ghc` source tree.

Once you have GHC 7.11, special care is needed to install every library
with the right options when using `cabal`.  Specifically, we want to
pass the `-g` options to GHC to generate debug symbols.  Moreover, we
must stop `cabal` from stripping the object files from debug symbols.
For example, to install the library `digest` we'll use

    $ cabal install --disable-library-stripping --ghc-options="-g -rtsopts" digest

### Debug symbols for non-Haskell code

Moreover, when debugging Haskell code involving foreign C code, we need
to make sure that we have the debug symbols for the C libraries too.
This can be accomplished in various ways -- on Debian systems debug
symbols are packaged separately in `-dbg` packages.  So if we wanted to
properly debug a program using the `digest` library, which uses `zlib`,
we would need to issue

    $ sudo apt get install zlib1g-dbg

### Keeping the source code for built library

Moreover, if we want to see source code in the `perf` tools, as we'll
see in the following sections, we need to keep the Haskell source files
where they were picked up to be built.  The easiest way to do this is
using `cabal get`, for example:

    $ cabal get zlib
    Unpacking to zlib-0.6.1.0/
    $ cd zlib-0.6.1.0
    $ cabal install --disable-library-stripping --ghc-options="-g -rtsopts"

The same applies for C libraries that we want to debug with source code
support.

### Building the example program

As an example, we will be testing the `digest-pure` library,
implementing the CRC32 algorithm in Haskell; against the `digest`
library, which also offers the CRC32 algorithm by binding to the `zlib`
library.

First we'll build the `zlib` library manually to keep the source code in
place, taking care of adding `-g` to the `CFLAGS` in the `Makefile`:

    $ wget http://zlib.net/zlib-1.2.8.tar.gz
    $ tar xzvf zlib-1.2.8.tar.gz
    $ cd zlib-1.2.8
    $ nano Makefile.in # Add -g to CFLAGS
    $ ./configure
    $ make
    $ export LD_LIBRARY_PATH=`pwd`:$LD_LIBRARY_PATH

Then, we'll install the `digest` and `digest-pure` libraries, which we
need for the benchmark:

    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.11.20150411
    $ cabal get digest
    Unpacking to digest-0.0.1.2/
    $ cd digest-0.0.1.2/
    $ cabal install --disable-library-stripping --ghc-options="-g -rtsopts"
    $ cd ..
    $ cabal get digest-pure
    $ cd digest-pure-0.0.3/
    $ cabal install --disable-library-stripping --ghc-options="-g -rtsopts"

Note how we're getting the packages using `cabal get`, so that the
Haskell sources will be preserved in the locations where the objects
were built from.

We'll also install `mwc-random` without bothering to keep the sources,
since we won't need to inspect its functions (we use it to generate
random data to run our benchmark on):

    $ cabal install --disable-library-stripping --ghc-options="-g -rtsopts" mwc-random

## Making the Haskell version fast

### Comparing the C and Haskell version

We're going to use a
[simple benchmark](https://gist.github.com/bitonic/781ff80e6eb65eebf14a)
to compare the Haskell and C function.

    $ wget https://gist.github.com/bitonic/781ff80e6eb65eebf14a/raw/7cee087dbe12c6cf813667eccb0a00d721ecb9a6/crc32.hs
    $ ghc -g -O2 crc32.hs

`crc32.hs` let us compare two functions with an identical interface,
computing the CRC32 for a given `ByteString`:

    crc32 :: ByteString -> Word32

`digest` implements this function by calling a foreign `crc32` routine
provided by `zlib`, while `digest-pure` implements it natively in
Haskell.

Now we're ready to compare the two versions, one using the pure CRC32,
and one using zlib.  First we generate some random data to work on

    $ ./crc32 generate

This will write a `data` file to disk, which we can use in all
subsequent tests.  Then we time each version:

    $ time ./crc32 hask # Using digest-pure
    ./crc32 hask  34,55s user 0,01s system 99% cpu 34,599 total
    $ time ./crc32 c # Using digest
    ./crc32 c  10,42s user 0,00s system 99% cpu 10,444 total

The Haskell version is more than 3 times slower than the C one.  Let's
try to find out why.

### Finding out where time is spent

The first thing we'll do is understand where time is spent.  We expect
some CRC32 function to make up for most of the running time, but we'd
like to verify this.  Moreover, we'd like to know what exactly is taking
time in the CRC32 function itself.

Let's run the two versions again, but this time using `perf record`:

    $ perf record -o perf-hask.data ./crc32 hask
    [ perf record: Woken up 2 times to write data ]
    [ perf record: Captured and wrote 0.346 MB perf-hask.data (~15126 samples) ]
    $ perf record -o perf-c.data ./crc32 c
    [ perf record: Woken up 1 times to write data ]
    [ perf record: Captured and wrote 0.110 MB perf-c.data (~4789 samples) ]

`perf record` will sample the execution with the aid of the
`perf_event_open` linux syscall.  Using `perf report -i perf-hask.data`
we can inspect the result of the Haskell version.  The main screen shows
a breakdown of where time is spent in the executable:

    Samples: 8K of event 'cpu-clock', Event count (approx.): 2233500000
      99,75%  crc32  crc32              [.] s4Tw_info
       0,10%  crc32  crc32              [.] c5gn_info
       0,03%  crc32  crc32              [.] c5g3_info
       0,02%  crc32  crc32              [.] c2k_info
       0,02%  crc32  crc32              [.] c7F_info
       0,01%  crc32  [kernel.kallsyms]  [k] __do_softirq
       0,01%  crc32  crc32              [.] allocatePinned
       0,01%  crc32  crc32              [.] c6le_info
       0,01%  crc32  crc32              [.] chg3_info
       0,01%  crc32  crc32              [.] 0x000000000041ca88
       0,01%  crc32  libc-2.19.so       [.] _dl_addr

Symbols like `c5gn_info` are generated by the GHC code generator.  As we
can see, almost all the time is spent in a single symbol, `s4Tw_info`.
We can drill down into `s4Tw_info`, and "annotate" it, to get a precise
breakdown of where time is spent in `s4Tw_alone`:

    s4Tw_info  /home/francesco/crc32/crc32
           │    Disassembly of section .text:
           │
           │    000000000063a738 <s4Tw_info>:
           │        crc = flipAll prevCRC
           │        flipAll = xor 0xffffffff
           │
           │    -- | Unsafe array access.
           │    (!!!) :: (IArray a e, Ix i, Integral i) => a i e -> i -> e
           │    arr !!! i = unsafeAt arr $ fromIntegral i
      0,79 │ 0:┌─→add    $0x10,%r12
     10,33 │   │  cmp    0x358(%r13),%r12
           │   │↓ ja     27
      0,04 │   │  cmp    %rdi,%rsi
      0,01 │   │↓ jne    36
           │   │  movq   $0xa737a0,-0x8(%r12)
           │   │  mov    %r14,(%r12)
           │   │  lea    -0x7(%r12),%rbx
           │   │↓ jmpq   ffffffffff9c58c8
           │27:│  movq   $0x10,0x388(%r13)
           │   │↓ jmpq   ffffffffff9c58c8
           │   │  flipAll (tblEntry `xor` (crc `shiftR` 8))
           │   │  where
           │   │    -- Note: unsafe access is ok here, we guarantee that the value is within
           │   │    -- (0..255), crc32Table covers that range.
           │   │    tblEntry = crc32Table !!! ((crc `xor` (fromIntegral c)) .&. 0xff)
           │   │    crc = flipAll prevCRC
      0,76 │36:│  add    $0xfffffffffffffff0,%r12
      0,02 │   │  mov    $0xffffffff,%eax
     10,01 │   │  xor    %rax,%r14
      0,03 │   │  mov    $0xffffffff,%eax
      1,07 │   │  mov    %r14,%rcx
           │   │  shr    $0x8,%rcx
     10,55 │   │  xor    %rax,%rcx
      0,10 │   │  mov    0x4(%rbx),%rax
      1,03 │   │  movzbl (%rsi),%edx
      0,16 │   │  xor    %rdx,%r14
      9,69 │   │  and    $0xff,%r14d
      0,75 │   │  mov    0x10(%rax,%r14,4),%eax
     44,32 │   │  xor    %rcx,%rax
     10,31 │   │  inc    %rsi
      0,01 │   │  mov    %rax,%r14
           │   └──jmp    0

What this breakdown tells us is that a whopping 75% of the time is spent
in `xor` and `and` instructions alone.  Another 20% of the time is spent
in looping code: a `cmp` at the beginning, and an `inc` at the end --
which keep track of an index in the `ByteString` we're digesting to find
out when we're done traversing it.  Moreover, we get the Haskell source
code interleaved with the assembly deriving from it, which lets us
understand what Haskell code parts of `s4Tw_info` correspond to.  In
this case, the part that eats almost all of the time is
[the section of the code corresponding to the main section of the CRC32 algorithm](https://github.com/danieldk/digest-pure/blob/master/src/Data/Digest/Pure/CRC32.hs#L50),
which is what we expected.  What the code does is get a value out of pre
computed table, and then use it together with the current CRC to get the
new value.

To get more information about the symbol, we can also use `objdump` on
the binary to get more information about the "compile unit" `s4Tw_info`
comes from:

    $ objdump -Wi crc32
    [...]
      Compilation Unit @ offset 0xdbd68:
       Length:        0x629 (32-bit)
       Version:       3
       Abbrev Offset: 0x6cb
       Pointer Size:  8
     <0><dbd73>: Abbrev Number: 1 (DW_TAG_compile_unit)
        <dbd74>   DW_AT_name        : src/Data/Digest/Pure/CRC32.hs 
        <dbd92>   DW_AT_producer    : The Glorious Glasgow Haskell Compilation System 7.11.20150411 
        <dbdd0>   DW_AT_language    : 0x18  (Unknown: 18)
        <dbdd4>   DW_AT_comp_dir    : /tmp/digest-pure-0.0.3/
        <dbdec>   DW_AT_use_UTF8    : 255
        <dbded>   DW_AT_stmt_list   : 0x32790
    [...]
     <1><dbf83>: Abbrev Number: 2 (DW_TAG_subprogram)
        <dbf84>   DW_AT_name        : !!!
        <dbf88>   DW_AT_MIPS_linkage_name: s4Tw_info
        <dbf92>   DW_AT_external    : 0
        <dbf93>   DW_AT_low_pc      : 0x63afc8
        <dbf9b>   DW_AT_high_pc     : 0x63b03a
        <dbfa3>   DW_AT_frame_base  : 1 byte block: 9c      (DW_OP_call_frame_cfa)
    [...]

Which tells us the exact file the symbol comes from.  The DWARF
information also takes a stab at pairing the symbol with a name,
although it is often difficult to do so: in this case it picked the
inlined `!!!` function, which is misleading.

Check out [this screencast](https://asciinema.org/a/19532) for a taste
of what interacting with `perf` on this data set looks like.

Now let's do the same with the C version, using `perf report -i
perf-c.data`:

<div class="center-image">[![asciicast](https://asciinema.org/a/19533.png)](https://asciinema.org/a/19533)</div>

In this case almost all the time is spent in `libz.so` `crc32` function,
which is again what we expect.  However when drilling into the `crc32`
function, we get a much bigger disassemble output.  While confusing at
first, it is quite clear that all the time is spent in the mysterious
`DOLIT32`, a rather long section of the code:

           │             DOLIT32;
      0,04 │ 70:   xor    (%r8),%edi
      1,29 │       mov    %rdi,%rax
           │       mov    %edi,%r11d
           │       movzbl %ah,%ebx
      2,31 │       movzbl %dil,%eax
      0,04 │       shr    $0x18,%edi
           │       mov    0xc00(%rcx,%rax,4),%eax
      3,33 │       xor    (%rcx,%rdi,4),%eax
      2,76 │       shr    $0x10,%r11d
      [...omitted code...]
           │       shr    $0x18,%edi
      1,17 │       shr    $0x10,%r11d
      0,08 │       movzbl %ah,%eax
      1,06 │       mov    (%rcx,%rdi,4),%edi
      4,20 │       xor    0xc00(%rcx,%rbx,4),%edi
      2,38 │       movzbl %r11b,%r11d
           │       xor    0x800(%rcx,%rax,4),%edi
      1,14 │       xor    0x400(%rcx,%r11,4),%edi

In fact, summing up the small percentages in `DOLIT32` we find out that
around 85% of the time is spent there.  To find out what `DOLIT32` does
we need to consult the
[`zlib` source code](https://github.com/madler/zlib/blob/master/crc32.c#L241).
`DOLIT32` is a macro repeating `DOLIT4` 8 times.  `DOLIT4` is a macro
which computes the CRC32 4 bytes at a time, assuming little endianness.
Equivalent macros for big endian architecture are present.

### Why is the Haskell version is slower

The fact that `zlib` computes the CRC32 4 bytes at a time, and that it
unrolls the main loop makes the code per byte needed to compute the
CRC32 much smaller.  The loop (from `cmp` to `jmp`) with `DOLIT32` as a
body uses around 110 instructions to compute the CRC32 for 32 bytes
(≈3.5 instructions per byte), while the loop generated from the pure
Haskell version uses 25 instructions for just one byte.  This should
amount to a seven fold increase in the instructions executed by the
Haskell program.

We can easily verify this fact using `perf`.  In this case we'll use
`perf stat`, which gives us global statistics, since we know that one
symbol is taking up all the time.  Otherwise we could use `perf record
-e` to record the events we're interested and get figures for the
specific symbols.

    $ perf stat ./crc32 hask
    
     Performance counter stats for './crc32 hask':
    
          35623,015084 task-clock (msec)         #    0,995 CPUs utilized
                 4.982 context-switches          #    0,140 K/sec
                   207 cpu-migrations            #    0,006 K/sec
                 3.318 page-faults               #    0,093 K/sec
        94.594.036.631 cycles                    #    2,655 GHz
        44.565.899.687 stalled-cycles-frontend   #   47,11% frontend cycles idle
       <not supported> stalled-cycles-backend
       210.111.081.169 instructions              #    2,22  insns per cycle
                                                 #    0,21  stalled cycles per insn
        30.020.583.035 branches                  #  842,730 M/sec
             1.138.765 branch-misses             #    0,00% of all branches
    
          35,788883623 seconds time elapsed

    $ perf stat ./crc32 c
    
     Performance counter stats for './crc32 c':
    
          10690,308263 task-clock (msec)         #    0,995 CPUs utilized
                 1.754 context-switches          #    0,164 K/sec
                    63 cpu-migrations            #    0,006 K/sec
                 3.314 page-faults               #    0,310 K/sec
        28.453.816.903 cycles                    #    2,662 GHz
        18.427.339.226 stalled-cycles-frontend   #   64,76% frontend cycles idle
       <not supported> stalled-cycles-backend
        31.027.193.409 instructions              #    1,09  insns per cycle
                                                 #    0,59  stalled cycles per insn
           330.594.059 branches                  #   30,925 M/sec
               670.612 branch-misses             #    0,20% of all branches
    
          10,743639214 seconds time elapsed

The first thing to notice is the difference in the number of
instructions.  The Haskell program uses 201 billions, versus the 31
billions of the C program.  A 6.5 difference, which is a factor close to
our rough estimate of 7.

Note that the 6.5 difference in instructions does not translate to a 6.5
performance increase in the C program.  This can be due to a variety
of reasons, but the `perf stat` output already shows a factor: the C
version spends more time stalled, as evident from the
`stalled-cycles-frontend` hardware counter.  This is most likely since
the C code spends less time in the loop code, and more time fetching
bytes from the input buffer and from the static tables used to compute
the CRC32.  In fact, the overall productivity is lower: 1 instruction
per cycle in C versus more than 2 in Haskell.  Thus, we only get a 3x
speedup in C , as we have seen.

### Making the Haskell version faster

Now that we know the main reason for the Haskell version being slower,
we can make the Haskell version more similar to the C one.  The first
measure we adopted is to implement the CRC32 algorithm so that it
consumes 4 bytes at a time.  We choose to do this before loop unrolling
because it has more possibilities to impact performance.  For instance
it makes the algorithm less sequential and increases the chances for pipelining, since we don't have to
wait for each byte to continue.

To do so, we will split the input `ByteString` in three sections: an
aligned vector of `Word32`s, and the remaining unaligned leading and
trailing bytes.  For example, for a `ByteString` of length 26 starting
at address 9, we will have 3 leading bytes (up to address 12), 6 aligned
words (up to address 38), and 2 trailing bytes.  This will give us a
memory-aligned section to perform the CRC32 word-by-word instead of
byte-by-byte.  The type signature for the function splitting the
`ByteString` will be

    getWord32Vector
      :: BS.ByteString
      -> (BS.ByteString -> V.Vector Word32 -> BS.ByteString -> IO a)
      -> IO a

We use a continuation-passing style since we need to manipulate the
pointer underlying the `ByteString` to know where the aligned section of
the memory starts, and the functions that let us do this use CPS
themselves.

Once we have this function, the ugly part is over: we can very easily
port the algorithm present in `zlib` to Haskell, but in a more
functional style:

    {-# NOINLINE crc32UpdateLittle #-}
    crc32UpdateLittle
      :: Word32
      -- ^ The previous CRC
      -> BS.ByteString
      -- ^ The input to digest
      -> Word32
      -- ^ Resulting CRC
    crc32UpdateLittle prevC bs =
      unsafePerformIO $ getWord32Vector bs $ \leading aligned trailing -> do
        let c0 = complement prevC
        let c1 = BS.foldl' word8Step c0 leading  -- Perform CRC32 on the leading bytes...
        let c2 = V.foldl' word32Step c1 aligned  -- ...on the aligned words...
        let c3 = BS.foldl' word8Step c2 trailing -- ...and on the trailing bytes.
        return $ complement c3
      where
        {-# INLINE word32Step #-}
        word32Step :: Word32 -> Word32 -> Word32
        word32Step c0 word =
          (crc32Table_3 !!! (c .&. 0xff)) `xor`
          (crc32Table_2 !!! ((c `unsafeShiftR` 8) .&. 0xff)) `xor`
          (crc32Table_1 !!! ((c `unsafeShiftR` 16) .&. 0xff)) `xor`
          (crc32Table_0 !!! (c `unsafeShiftR` 24))
          where
            c = c0 `xor` word
    
        {-# INLINE word8Step #-}
        word8Step :: Word32 -> Word8 -> Word32
        word8Step c byte =
          (crc32Table_0 !!! ((c `xor` fromIntegral byte) .&. 0xff)) `xor` (c `unsafeShiftR` 8)

The various `crc32Table_n` are various pre-computed tables needed to
perform the algorithm, and `!!!` is an unsafe indexing operator (we know
we're going to be within the bounds). The `Little` is to indicate that
this is the algorithm for little-endian architectures.  A matching
`crc32UpdateBig` has to be defined for big-endian architectures.

You can find the full source in [my fork of `digest-pure`](https://github.com/bitonic/digest-pure/blob/fast-crc32/src/Data/Digest/Pure/CRC32.hs).

After we recompile `digest-pure` again with the changes above, and
recompile our `crc32.hs` benchmark, we can run it again using the
Haskell version:

    $ time ./crc32 hask
    ./crc32 hask  12,87s user 0,02s system 99% cpu 12,903 total

With the new algorithm we get a three fold speedup, and we're only 20%
slower than the C version.

    $ perf stat ./crc32 hask
    
     Performance counter stats for './crc32 hask':
    
          13025,715282 task-clock (msec)         #    0,993 CPUs utilized
                 1.668 context-switches          #    0,128 K/sec
                    61 cpu-migrations            #    0,005 K/sec
                 3.340 page-faults               #    0,256 K/sec
        35.174.753.255 cycles                    #    2,700 GHz
        15.093.419.773 stalled-cycles-frontend   #   42,91% frontend cycles idle
       <not supported> stalled-cycles-backend  
        77.735.375.354 instructions              #    2,21  insns per cycle
                                                 #    0,19  stalled cycles per insn
         2.541.638.382 branches                  #  195,125 M/sec
               956.494 branch-misses             #    0,04% of all branches
    
          13,121155788 seconds time elapsed

The number of instructions is reduced by almost three times, while the
productivity stays the same -- which explains the threefold increase in
performance.  Note that we're still using more than twice the
instructions as the C program, due to loop unrolling.  However, loop
unrolling seems to push the program towards being memory-bound and thus
does not result in a great performance increase.  We have not pursued
this road further.

However, when analyzing the program through `perf report`, we get
somewhat surprising results:

           │    crc32UpdateLittle :: Word32 -> BS.ByteString -> Word32
           │    crc32UpdateLittle prevC bs =
           │      unsafePerformIO $ getWord32Vector bs $ \leading aligned trailing -> do
           │        let c0 = complement prevC
           │        let c1 = BS.foldl' word8Step c0 leading
           │        let c2 = V.foldl' word32Step c1 aligned
      0,05 │ 9:   mov    0xe(%rbx),%rax
      0,03 │      mov    0x16(%rbx),%rax
      7,21 │      mov    0x1e(%rbx),%rax
      0,01 │      mov    0x26(%rbx),%rax
      0,06 │      mov    0x3e(%rbx),%rax
      0,02 │      mov    0x46(%rbx),%rcx
      7,27 │      mov    0x4e(%rbx),%rdx
      0,02 │      mov    0x56(%rbx),%rdi
      0,08 │      mov    0x36(%rbx),%r8
      0,03 │      mov    (%r8,%rsi,4),%r8d
      7,34 │      xor    %r8,%r14
      0,07 │      mov    %r14,%r8
      0,06 │      and    $0xff,%r8d
      0,09 │      mov    (%rax,%r8,4),%eax
     29,32 │      mov    %r14,%r8
      0,01 │      shr    $0x8,%r8
      0,02 │      and    $0xff,%r8d
           │      mov    (%rcx,%r8,4),%ecx
     10,83 │      mov    %r14,%r8
      0,00 │      shr    $0x10,%r8
      0,02 │      and    $0xff,%r8d
      0,01 │      mov    (%rdx,%r8,4),%edx
      8,89 │      shr    $0x18,%r14
      0,00 │      mov    (%rdi,%r14,4),%edi
      1,18 │      inc    %rsi
      0,00 │      xor    %rdi,%rdx
     13,49 │      xor    %rdx,%rcx
      6,75 │      xor    %rcx,%rax
      7,15 │      mov    %rax,%r14

`mov` instructions from register to register make up for more than 60%
of the time spent in the critical section of the code, while we would
expect most of the time to be spent `xor`ing and `and`ing.  I have not
investigated why this is the case, ideas welcome!

### Acknowledgements

Thanks to Peter Wortmann and to Arash Rouhani for the very informative
discussions on the status of DWARF in GHC.  Also thanks to Niklas
Hambüchen for reviewing a draft of this blogpost.
