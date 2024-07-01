---
title: CRC-32C tips and tricks
date: 2024-07-01
tags: [post]
sidenotes: true
description: In a companion blog post I talked about how to effectively combine Reed-Solomon coding and CRCs. To do so I presented a few functions to manipulate CRCs. In this post I'll explain why and how they can be implemented.
---

<script>
  window.fleqn = true;
</script>

In [a companion blog post](/posts/rs-crc.html) I talked about how to effectively combine Reed-Solomon coding and CRCs. To do so I presented a few functions to manipulate CRCs, namely:

```c
// Given CRCs for `a` and `b`, computes the CRC of the pointwise XOR
// of `a` and `b`.
//
//   crc32c_xor(size, crc32c(0, size, a), crc32c(0, size, b)) =
//   crc32c(0, size, [x^y for (x, y) in zip(a, b)])
uint32_t crc32c_xor(size_t size, uint32_t crc_a, uint32_t crc_b);

// Given CRCs for `a` and `b`, compute the the CRC of `a` concatenated
// with `b`. Only the size for `b` is needed, and the sizes may differ.
//
//   crc32c_append(crc32c(0, size_a, a), size_b, crc32c(0, size_b, b)) =
//   crc32c(crc32c(0, size_a, a), size_b, b)
uint32_t crc32c_append(uint32_t crc_a, size_t size_b, uint32_t crc_b);

// Given CRC for `a`, gives the CRC we'd get by adding `n` zero
// bytes at the end of `a`.
//
//   crc32c_add_zeros(size_a, crc32c(0, size_a, a), n) =
//   crc32c(crc32c(0, size_a, a), n, [0]*n)
uint32_t crc32c_add_zeros(uint32_t crc, size_t n);

// Given CRC for `a + [0]*n`, that is the CRC for a
// bytestring with `n` trailing zeros, gives the CRC
// of `a` without the trailing zeros.
//
//   crc32c_remove_zeros(crc32c(0, size_a+n, a + [0]*n), n) =
//   crc32c(0, size_a, a)
uint32_t crc32c_remove_zeros(uint32_t crc, size_t n);
```

The code for these functions [is available on GitHub](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e). In the rest of the post I'll explain why and how they can be implemented.

## Bytestring as polynomials

<div>

When computing for the purpose of CRCs we treat bytestrings as polynomials with [one-bit numbers](https://en.wikipedia.org/wiki/GF(2)) as coefficients. From now on we'll call one-bit numbers $\mathbb{F}_2$, and polynomials over them $\mathbb{F}_2[x]$.

In a byte, the least significant bit will be the coefficient of $x^7$, and the most significant bit will be the coefficent to $x^0 = 1$. For instance:

</div>

$$
\texttt{10011010} = 1 + x^3 + x^4 + x^6
$$

When processing data we'll consider each byte to be of higher degree compared to the one that follows:

$$
\texttt{10011010 01110100} = (1 + x^3 + x^4 + x^6) \times x^8 + (x + x^2 + x^3 + x^5)
$$

<div>

Seeing our data as polynomials with weird coefficients is convenient because these polynomials behave a lot like the integers -- they can be added, multipied, and divided with remainder. In algebra speak they're _rings_.[^why-not-integers]

[^why-not-integers]: Moreover, unlike with the integers, the operations we're interested in can be efficiently implemented in hardware.

</div>

## Some properties of $\mathbb{F}_2[x]$

Before proceeding, it's useful to get a few useful properties out of the way, since we'll be implicitly using them often.

Firstly, addition over $\mathbb{F}_2$ is equivalent to one-bit XOR, and therefore addition over $\mathbb{F}_2[x]$ is equivalent to bitwise XOR:

$$
\begin{equation*}
\begin{split}
  \; & \texttt{10011010} + \texttt{01011001} \\
= \; & (1 + x^3 + x^4 + x^6) + (x + x^3 + x^4 + x^7) \\
= \; & 1 + x + x^6 + x^7 \\
= \; & \texttt{11000011} \\
\end{split}
\end{equation*}
$$

Unlike integer addition, $\mathbb{F}_2[x]$ addition does not change the "size" of the polynomial. That is, the number of digits of the result is the same as the number of digits of the operands. Conveniently, this means that that $\bmod$ is linear:

$$
(a + b) \bmod p = (a \bmod p) + (b \bmod p)
$$

XOR AKA $+$ also lets us invert parts of the data. We'll often invert blocks of 32 bits like so:

$$
\texttt{\textasciitilde a} = \texttt{a\,\textasciicircum\,0xFFFFFFFF} = a + \sum_{i = 0}^{\deg(p)-1} x^i = a + \mathtt{1}_{\deg(p)}
$$

Where $\mathtt{1}_n$ is a shorthand for $\sum_{i = 0}^{n-1} x^i$.

Multiplication does not have an easy bitwise analogy, but it works as you'd expect:

$$
\begin{equation*}
\begin{split}
  & \; \texttt{10011010} \times \texttt{01011001} \\
= & \; (1 + x^3 + x^4 + x^6) \times (x + x^3 + x^4 + x^7) \\
= & \; x + x^3 + x^5 + x^6 + x^8 + x^9 + x^{11} + x^{13} \\
= & \; (1 + x + x^3 + x^5)x^8 + x + x^3 + x^5 + x^6 \\
= & \; \texttt{11010100 01010110} \\
\end{split}
\end{equation*}
$$

As you can see multiplication _does_ increase the number of digits, which also means that $\bmod$ distributes over products in the same way we're used to with integers:

$$
(a \times b) \bmod p = ((a \bmod p) \times (b \bmod p)) \bmod p
$$

<div>

Finally, we'll frequently multiply by $x^n$ to add $n$ zeros to a bytestring:

$$
\begin{equation*}
\begin{split}
  & \; \texttt{10011010} \times x^{16} \\
= & \; (1 + x^3 + x^4 + x^6) \times x^{16} \\
= & \; \texttt{11010100 00000000 00000000} \\
\end{split}
\end{equation*}
$$

</div>

## CRC = $\bmod$

It's the remainder operation, $\bmod$, which we're interested in. In fact CRCs in their most basic forms _are_ the $\bmod$ operation:

$$
\mathrm{CRC}_{\mathrm{raw}}(a) = a x^{\deg(p)} \bmod p
$$

$a$ is our polynomial/bytestring, $p$ is the _generator polynomial_ we've picked to be our divisor, and $\deg(p)$ is the degree of $p$. The degree of $p$ will determine how wide our CRC will be, much like an integer divisor determines the maximum amount of digits of the remainder.

In the case of CRC-32C, our CRC of choice, $p$ will predictably be of degree 32, for a 32-bit output. The C stands for Guy Castagnoli, one of the authors of a 1993 paper which first used this specific polynomial. We use it since it satisfies several properties we're interested in, but more importantly because it is implemented in hardware in all modern CPUs.

Implementing $\bmod$ is a matter of performing a [simple version of long division](https://en.wikipedia.org/wiki/Polynomial_long_division):

```c
// Castagnoli polynomial, most significant bit = degree zero,
// least significant bit = degree 31:
//
//   10000010111101100011101101111000 1 =     (last 1 = x^32 is implicit)
//   1 + x^6 + x^8 + x^10 + x^11 + x^13 + x^14 + x^18 + x^19 + x^20 + x^22 + x^23 + x^26 + x^27 + x^28 + x^32
static const uint32_t CASTAGNOLI_POLY = 0x82F63B78u;

// Computes CRC_raw for an 8-term polynomial.
uint32_t crc32c_u8(uint8_t a) {
  // Scale the input polynomial so that it's of degree 31
  // (remember, lowest significance bit = highest degree).
  uint32_t crc = a;
  for (int i = 0; i < 8; i++) {            // For each input coefficient...
    crc = (crc >> 1) ^                     // ...pop highest degree coefficient...
          ((crc&1) ? CASTAGNOLI_POLY : 0); // ...and XOR with polynomial if it was non-zero.
  }
  return crc;
}
```

In the code above, and in all code that follows, we'll always compute with the same 32-bit polynomial. The code can be trivially changed to use another 32-bit polynomial, and easily changed to work with other widths.

It's also easy to chain CRC computations together, which lets us implement `crc32c` for bytestrings:

```c
uint32_t crc32c_u8(uint32_t crc, uint8_t a) {
  // Include new data in previous computation.
  crc ^= a;
  // Resume.
  for (int i = 0; i < 8; i++) {
    crc = (crc >> 1) ^ ((crc&1) ? CASTAGNOLI_POLY : 0);
  }
  return crc;
}

// Computes CRC_raw for a polynomial/bytestring of arbitrary
// length. Here `data` is `a`, and `crc` lets us compute
// CRC_raw in a streaming fashion by chaining calls to
// crc32c_raw. The first (and possibly only) call initializes
// the crc to 0.
uint32_t crc32c_raw(uint32_t crc, size_t size, uint8_t* data) {
  for (size_t i = 0; i < size; i++) {
    crc = crc32c_u8(crc, data[i]);
  }
  return crc;
}
```

`crc32c_raw` is not intended to be a fast implementation, but is functionally equivalent to fast ones implemented using the [CRC-32C instructions](https://www.felixcloutier.com/x86/crc32) present in recent CPUs, or to [even faster ones](https://github.com/corsix/fast-crc32/) implemented combining those instructions with instructions specific to $\mathbb{F}_2[x]$.

## Safety measures

As the name indicates, we typically don't use `crc32c_raw` as-is in practice. Instead, we augment it with an important safety measure:

$$
\mathrm{CRC}(a) = \mathtt{1}_{\deg(p)} + ((a x^n + x^{\mathrm{len}(a)}\mathtt{1}_{\deg(p)})  \bmod p)
$$

$\mathrm{len}(a)$ is the length of $a$ in bits. The key detail here is that $\mathrm{len}(a)$ includes leading zeros, and therefore is _not_ the same as the degree of $a$. For instance the bytestring $\texttt{00000000 10110001}$, equivalent to polynomial $1 + x^2 + x^3 + x^7$, will have length 16 but degree 7.

<div>

The objective of the inner XOR is precisely to turn turn leading zeros into non-zeros, so that they can be factored into the computation. In other words, $\mathrm{CRC}_{\mathrm{raw}}(\texttt{10110001}) = \mathrm{CRC}_{\mathrm{raw}}(\texttt{00000000 10110001})$, which we almost certainly do not want. Inverting the high bits of the input fixes this.

The outer XOR makes $\mathrm{CRC}$ well behaved in the presence of the inner XOR -- we'll have for instance  that `crc32c(x, 0, ...) = x`.

<!--
[^concatenate]: The outer XOR is also relevant in the case where CRCc are trailing protects against the much rarer occurence which will cause us to ignore trailing zeros if our input bytestring is a multiple of $p$. That is, the case where $42 \bmod 7 = (42 \times 100) \bmod 7 = 0$, but with polynomials. It also 
-->

In code, this is what it looks like:

</div>

```c
// Implements CRC for the Castagnoli polynomial.
uint32_t crc32c(uint32_t crc, size_t size, uint8_t* data) {
  // Note that at the beginning, when crc=0, this is equivalent to
  // crc = 0xFFFFFFFF, which will invert the high bits
  // of `data` in `crc32c_u8`.
  crc = ~crc;
  for (size_t i = 0; i < size; i++) {
    crc = crc32c_u8(crc, data[i]);
  }
  // Invert, again equivalent to XORing with 0xFFFFFFFF at
  // the end.
  return ~crc;
}
```

This section specifies what the CRC32-C of a bytestring is, both in maths ($\mathrm{CRC}$ with $p$ being the Castagnoli polynomial) and code (`crc32c`). Now that we're done with that, we can start to implement our functions.

## `crc32c_add_zeros`

Let's go back to the type signature and documentation for `crc32c_add_zeros`:

```c
// Given CRC for `a`, gives the CRC we'd get by adding `n` zero
// bytes at the end of `a`.
//
//   crc32c_add_zeros(size_a, crc32c(0, size_a, a), n) =
//   crc32c(crc32c(0, size_a, a), n, [0]*n)
uint32_t crc32c_add_zeros(uint32_t crc, size_t n);
```

As we know, if $a$ is our polynomial/bytestring we can pad it with $n$ zeros simply by multiplying by $x^n$.

Therefore computing `crc32c_add_zeros(crc_a, n)` consists of computing $\mathrm{CRC}(ax^{8n})$ given $\mathrm{CRC}(a)$. The $8n$ is just due to the fact that `n` refers to bytes, but each polynomial coefficient refers to a single bit. We'll omit this factor from now on, since it's not relevant.

Through [some algebra](#appendix_add_zeros), we can rearrange $\mathrm{CRC}(ax^n)$ into something we can work with:

$$
\mathrm{CRC}(ax^n) = \mathtt{1}_{\deg(p)} + ((\mathtt{1}_{\deg(p)} + \mathrm{CRC}(a)) \times (x^n \bmod p)) \bmod p
$$

<div>

In the formula above, the only non-trivial operation is $x^n \bmod p$. $\mathrm{CRC}(a)$ is what we're starting with. Addition of degree 32 polynomials is just XOR over `uint32_t`. [Multiplication of degree 32 polynomials](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L215-L234) is also a constant time operation, and [available in all recent CPUs](https://en.wikipedia.org/wiki/CLMUL_instruction_set).

We can divide and conquer the problem of computing $x^n \bmod p$ by partitioning $n$ into powers of two:

$$
x^{152} = x^{2^3 + 2^4 + 2^7} = x^{2^3}x^{2^4}x^{2^7}
$$

Which lets us simplify $x^n \bmod p$:

$$
((((x^{2^3} \bmod p) \times (x^{2^4} \bmod p)) \bmod p) \times (x^{2^7} \bmod p)) \bmod p
$$

So all we need is to precompute $x^{2^k} \bmod p$ so that we can cover all the bits of our input size. Once we've done that, [we can implement `crc32c_add_zeros`](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L278-L292):[^table-loop]

[^table-loop]: Keen eyed readers might have realized that `CRC_POWER_TABLE` loops over every 31 elements. This is due to the degree of the largest irreducible factor of the Castagnoli polynomial.

    We could have used a 31 element table instead, indexing it modulo 31, but it's cheaper and easier to just generate a table large enough for `size_t`.

</div>

```c
// x^2^3, x^2^4, ..., x^2^63
static uint32_t CRC_POWER_TABLE[64];

uint32_t crc32c_x_pow_n(size_t n) {
  uint32_t x_pow_n = 1u << 31; // x_pow_n = x
  for (int k = 0; n; k++, n >>= 1) {
    if (n&1) {
      // (x_pow_n * x^2^k) mod p
      x_pow_n = crc32c_mul_mod_p(x_pow_n, CRC_POWER_TABLE[k]); 
    }
  }
  return x_pow_n;
}

uint32_t crc32c_add_zeros(uint32_t crc, size_t size) {
  return ~crc32c_mul_mod_p(~crc, crc32c_x_pow_n(size));
}
```

## `crc32c_remove_zeros` {#remove-zeros}

`crc32c_remove_zeros` works very similarly to `crc32c_add_zeros`, but instead of multiplying by $x^n$, we want to multiply by $x^{-n}$:

$$
\mathrm{CRC}(ax^nx^{-n}) = \mathtt{1}_{\deg(p)} + ((\mathtt{1}_{\deg(p)} + \mathrm{CRC}(ax^n)) \times (x^{-n} \bmod p)) \bmod p
$$

Note that we're starting out with $\mathrm{CRC}(ax^n)$, since we assume that the function is fed the CRC of a string with $n$ trailing zeros:

```c
// Given CRC for `a + [0]*n`, that is the CRC for a
// bytestring with `n` trailing zeros, gives the CRC
// of `a` without the trailing zeros.
//
//   crc32c_remove_zeros(crc32c(0, size_a+n, a + [0]*n), n) =
//   crc32c(0, size_a, a)
uint32_t crc32c_remove_zeros(uint32_t crc, size_t n);
```

The only problem is finding $x^{-1}$ -- or in fact, whether $x^{-1}$ exists at all. We're looking for a polynomial $x^{-1}$ such that $(x \times x^{-1}) \bmod p = 1$.

One easy way to guarantee that such a polynomial exists is by including $1$ in $p$, which will ensure that $p$ is not divisible by $x$. This is the case with the Castagnoli polynomial and more generally all polynomials used for CRC purposes.[^one-term]

[^one-term]: This is due to the fact that not having $1$ as a term [effectively wastes one bit of the polynomial](https://en.wikipedia.org/wiki/Mathematics_of_cyclic_redundancy_checks#Error_detection_strength).

Given that we know that $x^{-1}$ must exist, the easiest way to find $x^{-1}$ is just [to loop through all 32-bit numbers](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L295-L302). Once we've done so, [`crc32c_remove_zeros` is identical to `crc32c_add_zeros`](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L329-L343), but with another table:

```c
static uint32_t CRC_INVERSE_POWER_TABLE[64];

uint32_t crc32c_x_pow_neg_n(size_t n) {
  uint32_t x_pow_n = 1u << 31;
  for (int k = 0; n; k++, n >>= 1) {
    if (n&1) {
      x_pow_n = crc32c_mul_mod_p(x_pow_n, CRC_INVERSE_POWER_TABLE[k]);
    }
  }
  return x_pow_n;
}

uint32_t crc32c_remove_zeros(uint32_t crc, size_t size) {
  return ~crc32c_mul_mod_p(~crc, crc32c_x_pow_neg_n(size));
}
```

## `crc32c_xor` and `crc32c_append`

As mentioned earlier, $\bmod$ over $\mathbb{F}_2[x]$ is a linear operation. This means that computing the CRC of the XOR of two bytestring is trivial for $\mathrm{CRC}_{\mathrm{raw}}$. $\mathrm{CRC}$ requires [a bit more work](#appendix_xor), but can be reduced to something more manageable:

$$
\mathrm{CRC}(a + b) = \mathrm{CRC}(a) + \mathrm{CRC}(b) + \mathrm{CRC}(\mathtt{0}_{\mathrm{len}(b)})
$$

Where $\mathtt{0}_{n}$ is a bytestring made up of $n$ zeros. [In code](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L345-L348):

```c
// Given CRCs for `a` and `b`, computes the CRC of the pointwise XOR
// of `a` and `b`.
//
//   crc32c_xor(size, crc32c(0, size, a), crc32c(0, size, b)) =
//   crc32c(0, size, [x^y for (x, y) in zip(a, b)])
uint32_t crc32c_xor(size_t size, uint32_t crc_a, uint32_t crc_b) {
  return crc_a ^ crc_b ^ crc32c_add_zeros(0, size);
}
```

For what concerns `crc32c_append`, note that appending $b$ to $a$ is equivalent to $a^{\mathrm{len}(b)} + b$. Again, [we can rephrase](#appendix_append) $\mathrm{CRC}(ax^{\mathrm{len}(b)} + b)$ into something easy to compute given the CRCs of $a$ and $b$:

$$
\mathrm{CRC}(ax^{\mathrm{len}(b)} + b) = (\mathrm{CRC}(a) \times (x^{\mathrm{len}(b)} \bmod p)) \bmod p + \mathrm{CRC}(b)
$$

Or [in code](https://gist.github.com/bitonic/f60880888b20a8b5d67d53213e11f25e#file-crc32c-tips-cpp-L350-L353):

```c
// Given CRCs for `a` and `b`, compute the the CRC of `a` concatenated
// with `b`. Only the size for `b` is needed, and the sizes may differ.
//
//   crc32c_append(crc32c(0, size_a, a), size_b, crc32c(0, size_b, b)) =
//   crc32c(crc32c(0, size_a, a), size_b, b)
uint32_t crc32c_append(uint32_t crc_a, size_t size_b, uint32_t crc_b) {
  return crc32c_mul_mod_p(crc_a, crc32c_x_pow_n(size_b)) ^ crc_b;
}
```

## Wrapping up

The functions I presented are precisely the functions we needed for a distributed filesystem. However knowing that CRCs can be manipulated in this sort of way is useful in many circumstances.

Fast software implementation of CRCs themselves rely on this kind of math. More generally, these functions come in handy whenever we need to distribute work on a block of checksummed data (maybe while implementing a [parallel decompressor](https://zlib.net/pigz/)), while still being able to check integrity.

## Acknowledgements

Thanks to [Shachaf Ben-Kiki](https://shachaf.net/), [Peter Cawley](https://corsix.org), and [Alexandru Scvorţov](https://scvalex.net) for reading drafts of this blog post.

## Appendix

Proofs for the formulas used to implement the functions.

### $\mathrm{CRC}(ax^n)$ {#appendix_add_zeros}

$d = \deg(p)$, $l = \mathrm{len}(a)$

$$
\begin{equation*}
\begin{split}
  & \mathrm{CRC}(ax^n) \\
= \; & \mathtt{1}_{d} + (a x^n x^{d} + x^{l+n} \mathtt{1}_{d}) \bmod p \\
= \; & \mathtt{1}_{d} + ((a x^{d} + x^{l} \mathtt{1}_{d})x^n) \bmod p \\
= \; & \mathtt{1}_{d} + (((a x^{d} + x^{l} \mathtt{1}_{d}) \bmod p)(x^n \bmod p)) \bmod p \\
= \; & \mathtt{1}_{d} + ((\mathtt{1}_{d} + \mathtt{1}_{d} + (a x^{d} + x^{l} \mathtt{1}_{d}) \bmod p)(x^n \bmod p)) \bmod p \\
= \; & \mathtt{1}_{d} + ((\mathtt{1}_{d} + \mathrm{CRC}(a))(x^n \bmod p)) \bmod p \\
\end{split}
\end{equation*}
$$

### $\mathrm{CRC}(a + b)$ {#appendix_xor}

$d = \deg(p)$, $l = \mathrm{len}(a) = \mathrm{len}(b)$

$$
\begin{equation*}
\begin{split}
  & \mathrm{CRC}(a + b) \\
= \; & \mathtt{1}_{d} + ((a + b) x^d + x^l \mathtt{1}_{d}) \bmod p \\
= \; & \mathtt{1}_{d} + (ax^d + x^l \mathtt{1}_{d} + bx^d + x^l \mathtt{1}_{d} + \mathtt{0}_l x^d + x^l \mathtt{1}_{d}) \bmod p \\
= \; & \mathtt{1}_{d} + (ax^d + x^l \mathtt{1}_{d}) \bmod p + (bx^d + x^l \mathtt{1}_{d}) \bmod p + (\mathtt{0}_l x^d + x^l \mathtt{1}_{d}) \bmod p \\
= \; & \mathtt{1}_{d} + (ax^d + x^l \mathtt{1}_{d}) \bmod p + \mathtt{1}_{d} + (bx^d + x^l \mathtt{1}_{d}) \bmod p + \mathtt{1}_{d} + (\mathtt{0}_l x^d + x^l \mathtt{1}_{d}) \bmod p \\
= \; & \mathrm{CRC}(a) + \mathrm{CRC}(b) + \mathrm{CRC}(\mathtt{0}_l) \\
\end{split}
\end{equation*}
$$

### $\mathrm{CRC}(ax^{\mathrm{len}(b)} + b)$ {#appendix_append}

$d = \deg(p)$, $l_a = \mathrm{len}(a)$, $l_b = \mathrm{len}(b)$

$$
\begin{equation*}
\begin{split}
     & \mathrm{CRC}(ax^{l_b} + b) \\
= \; & \mathtt{1}_d + ((ax^{l_b} + b)x^d + \mathtt{1}_d x^{l_a} x^{l_b}) \bmod p \\
= \; & (\mathtt{1}_d x^{l_b} + a x^{l_b}x^d + \mathtt{1}_d x^{l_a} x^{l_b}) \bmod p +
       \mathtt{1}_d + (b x^d + \mathtt{1}_d x^{l_b}) \bmod p \\
= \; & ((\mathtt{1}_d + (a x_d + \mathtt{1}_d x^{l_a}) \bmod p)(x^{l_b} \bmod p)) \bmod p +
       \mathrm{CRC}(b) \\
= \; & (\mathrm{CRC}(a)(x^{l_b} \bmod p)) \bmod p + \mathrm{CRC}(b)
\end{split}
\end{equation*}
$$
