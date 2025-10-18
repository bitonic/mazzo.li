---
title: Hidden benefits of undefined behavior
date: 2025-10-17
tags: [post, short]
---

I was reviewing some code at work, and something jumped at me when reading some arithmetic:

```c
static inline int64_t int32ToDecimal9(int32_t i) { return i * 1000000000; }
```

If you've had the good fortune of dealing with C or C++ for extended periods you would have recognized the problem. `i * 1000000000` will be performed with 32-bit precision, which is almost certainly not what the programmer intended since it will lead to an overflow for any `i` outside $[-2, +2]$.

So far so good, a classic C footgun -- we've all been there.

However code using this function was running, and working, which was more surprising. Take a moment to consider why before revealing the solution.

<details>
<summary>Reveal</summary>

A direct compilation of `int32ToDecimal9` will look something like this:

```
imul    eax, edi, 1000000000
cdqe
```

First, perform 32-bit signed multiplication, then sign-extend the result into 64-bit.

However, signed overflow is undefined behavior, so an optimizing compiler might prefer to do the sign extension _before_ as part of a `mov` that it needed to do anyway, and then do the multiplication in 64 bits, thereby saving one instruction and "fixing" our bug in the process.

So we have a rare case of two C footguns cancelling each other out, rather than combining into an even bigger footgun as usual.

</details>
