---
title: "`inline-verilog`: Execute Verilog circuits from Haskell"
date: 2025-06-09
tags: [post, short]
image: https://mazzo.li/assets/images/inline-verilog.png
---

Like for most of the past 13 years, early June meant attending [ZuriHac](https://zfoh.ch/zurihac2025/). The event was as pleasant and well organized as ever.

While talking to Ed Kmett he half-jokingly mentioned that it would be useful to have a Verilog analogue of [`inline-c`](/posts/inline-c.html), especially to test Verilog circuits against Haskell functions.

A few hours later I had [`inline-verilog`](https://github.com/fpco/inline-c/tree/master/inline-verilog), which lets to do just that.

`inline-verilog` allows you to embed "nameless" Verilog modules right in your Haskell:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Language.Verilog.Inline as V
import           Data.Word
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable (peek)

V.verilog

adder :: Word16 -> Word16 -> IO (Bool, Word16)
adder a b = alloca $ \sum -> alloca $ \carry_out -> do
  [V.block|
    module (
        input  [15:0] a,
        input  [15:0] b,
        output [15:0] sum,
        output        carry_out
    );
        assign {carry_out, sum} = a + b;
    endmodule
  |]
  (,) <$> peek carry_out <*> peek sum

main :: IO ()
main = print =<< adder 1 2
```

The example above will print `(False,3)`.

Input and output arguments are expected to be present in the Haskell context in which the `V.block` is called. The Haskell types for the input and outputs are automatically inferred from the Verilog code. The output arguments are expected to be pointers in which the outputs will be stored.

Verilog code can also be factored out within a Haskell file using `V.verbatim`. See [`tests.hs`](https://github.com/fpco/inline-c/blob/master/inline-verilog/test/tests.hs) for a more complex example.

The way `inline-verilog` works is by invoking `verilator` to compile each Verilog block to C++, generating C stubs which execute the verilated circuit, and linking everything up with the Haskell executable.

There are some limitations, namely I haven't implemented or tested:

* Inputs/outputs wider than 64 bits.
* `struct` ports.
* Multidimensional input/output ports, e.g. `reg [15:0] foo [3:0][3:0]` .
* Importing.

All of the above should be easy, but it did not fit in the couple of hours it took to cook `inline-verilog` to its current state.

Also, `inline-verilog` does not make much sense for non-combinatorial circuits (i.e. stuff that does IO). But to test functional units, it should do the job.
