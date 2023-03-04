---
title: The essence of Reed-Solomon coding
date: 2022-11-06
tags: [post]
sidenotes: true
tags: [post, short]
image: https://mazzo.li/assets/images/lagrange.webp
---

Let's say we want to store some data on multiple drives, so that we can recover from drive failures.

<div>

One obvious (and valid!) first attempt is to just store everything multiple times -- usually called "mirroring". The most common form of mirroring is to store things twice: if one drive fails, the data is still in the other.[^raid1]

[^raid1]: This is often known as [RAID1](https://en.wikipedia.org/wiki/Standard_RAID_levels#RAID_1).

[Reed-Solomon coding](https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction) gives us much more flexibility, allowing us to store our data over $n = k + t$ drives, so that _any $t$_ drives can fail while still not losing data.[^which-rs]

[^which-rs]: This post presents a specific way to perform Reed-Solomon coding, using Lagrange interpolation. See the [Wikipedia article](https://en.wikipedia.org/wiki/Reed–Solomon_error_correction) for more information.

For instance, with $\mathrm{RS}(10, 4)$ we can store data over 14 drives, and any 4 drives can fail at once, without incurring data loss. The data is split over 10 equal blocks, and then 4 additional blocks ("parity" blocks) are generated in a such a way that any 4 blocks can be recovered from the other 10.[^raid6][^parity]

[^raid6]: RAID4 and RAID6 fit in this framework as $\mathrm{RS}(3,1)$ and $\mathrm{RS}(4,2)$ respectively.

[^parity]: The "parity" terminology comes from other error [detecting](https://en.wikipedia.org/wiki/Parity_bit) or [correcting](https://en.wikipedia.org/wiki/Hamming_code) codes which use parity bits.

The main idea behind Reed-Solomon is easy to visualize. We'll focus on the task of durably storing $k$ numbers $y_1, ..., y_k$. We'll plot them as $(1, y_1), ..., (k, y_k)$. For instance, with $k = 4$, we might have:

</div>

<div class="center-image">
![](/assets/images/rs-plot-1.svg)
</div>

<div>

For any distinct $k$ points, there's a [unique polynomial](https://en.wikipedia.org/wiki/Lagrange_polynomial) of degree $d < k$ passing through them.[^degree] This fact should not be too surprising, and is apparent when $k = 2$: given two distinct points, there's only one line passing through them.

[^degree]: A polynomial of degree $d$ is a something of the form

    $$a_0 + a_1 x + a_2 x^2 + ... + a_d x^d$$

Here's the unique third-degree polynomial passing through our four points:

<div class="center-image">
![](/assets/images/rs-plot-2.svg)
</div>

</div>

Armed with this fact, we can pick an additional $t$ points on the same polynomial. Again, with $k = 4$ and $t = 2$, we would have:

<div class="center-image">
![](/assets/images/rs-plot-3.svg)
</div>

Sampled points are in gold. Since the interpolating polynomial is unique given any $k$ points it passes through, we can drop any $t$ points, and we'll still be able to recover the polynomial. Once we've done that, we can just resample it to recover the numbers we're storing.

To recap, our procedure to durably store $k$ numbers is as follows:

* Compute the unique polynomial of degree $d < k$ by placing the numbers at some predefined interval on the XY plane;
* Sample the polynomial $t$ times beyond the original points;
* Store the $k$ original numbers alongside the $t$ "parity" numbers;
* If some numbers are lost we can recompute them by resampling the polynomial.

At this point, you already understand a key idea powering Reed-Solomon coding.

<div>

The other key idea allows us to store bits, rather than numbers. I won't properly explain it here, but the gist of it is to use _[finite fields](https://en.wikipedia.org/wiki/Finite_field)_ of order $2^{\mathrm{bits}}$ as our number type.[^peter]

Such finite fields are numeric types working differently from the usual integers modulo $2^{\mathrm{bits}}$ that we're used to program with, but still easy to implement in hardware, and importantly numbers for which the considerations in this blog post about polynomials [hold](https://en.wikipedia.org/wiki/Lagrange_polynomial#Finite_fields).[^fields]

[^fields]: As the name suggests, finite fields are [fields](https://en.wikipedia.org/wiki/Field_(mathematics)), which is handy since generating polynomials passing through a set of points involves divisions.

    Integers modulo $2^\mathrm{bits}$, which is what we're used to program with, are _not_ closed under division, which jams the maths required to perform the operations we described.

[^peter]: Head over to [Peter Cawley's blog](http://www.corsix.org/content/galois-field-instructions-2021-cpus) for details on how the finite field machinery works on modern CPUs.

Once we have such a numeric type, all we need to do to durably store some blob of data is to split it in a series of $2^{\mathrm{bits}}$ numbers (each $\mathrm{bits}$ wide), group them in sets of $k$, and then store them durably as described in this post, tuning $t$ based on how redundant we want to be.

The final trick worth mentioning is that this kind of Reed-Solomon encoding can be implemented efficiently given that we have fixed the $x$ coordinates, no matter what numbers we want to store. Or in other words, [the definition for $\ell_j$ which we use to generate the unique polynomial](https://en.wikipedia.org/wiki/Lagrange_polynomial#Definition) only depends on the $x$ coordinates, which allows us to do the heavy lifting once for any $k$ numbers we want to store.

</div>