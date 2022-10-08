---
title: Lánczos interpolation explained
date: 2022-10-07
tags: [post]
sidenotes: true
image: https://mazzo.li/assets/images/lanczos-applied-analysis-on-laptop.jpg
description: When you resize, rotate, or in any way transform an image; or more generally when you resample some discrete signal, the software you are using must _interpolate_ between the discrete points to produce a result. Linear and cubic interpolation are easy to explain, but Lánczos interpolation, one of the most popular methods, is more interesting. In this post we will build up to how Lánczos interpolation works, pulling in various interesting topics on the way.
---

<div>

Lánczos interpolation is one of the most popular methods to resize images, together with linear and cubic interpolation. I've spent a lot of time staring at images resampled with Lánczos, and a few years ago, I wondered where it came from. While many sources evaluate interpolation filters visually, I couldn't find a good explanation of how Lánczos interpolation is derived. So here it is!

In this post I do not attempt to explain what a [Fourier transform](https://en.wikipedia.org/wiki/Fourier_transform) does, so if you do not know that already you might find the mathematical details unclear. However, I do try to visualize and explain the intuition behind all the ideas.[^mathematica]

[^mathematica]: A lot of the maths is also worked out in a [Mathematica notebook](/assets/other/lanczos.html).

Before we begin, the images below showcase a variety of images resized with no interpolation, linear interpolation, and Lánczos interpolation. Lánczos produces non-blocky, sharp images, both when upscaling and downscaling.[^no-cubic][^lobes]

[^no-cubic]: Cubic interpolation is not included in the showcase since it is a family of filters rather than a single filter --- most cubic filters used in practice end up looking similar to Lánczos, although probably a bit less sharp but with less ringing.

[^lobes]: The images use a 3-lobed Lánczos interpolation.

</div>

<div class="filters-showcase-container">
<table class="filters-showcase">
  <thead><tr>
  <td></td>
  <th colspan="3" class="name">[Gutenberg](https://digital.bodleian.ox.ac.uk/objects/bea56620-793f-4dee-875d-ff051563292a/surfaces/1c554cf7-de45-4dc9-84e2-ee4d1902e9ef/)</th>
  <th colspan="3" class="name">[Bath](https://commons.wikimedia.org/wiki/File%3ABath_from_alexandra_park.jpg)</th>
  <th colspan="3" class="name">[Quixote](https://commons.wikimedia.org/wiki/File%3AAdventure_with_the_Windmills.jpg)</th>
  <th colspan="3" class="name">[Place](https://www.reddit.com/r/place/comments/txvk2d/rplace_datasets_april_fools_2022/)</th>
  <th colspan="3" class="name">[Renoir](https://commons.wikimedia.org/wiki/File:Auguste_Renoir_-_Dance_at_Le_Moulin_de_la_Galette_-_Google_Art_ProjectFXD.jpg)</th>
  <th colspan="3" class="name">[Peacock](https://commons.wikimedia.org/wiki/File%3AZoo_de_la_Barben_20100605_048.jpg)</th>
  </tr>
 <tr>
   <td></td>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
   <th class="algorithm">Nearest</th>
   <th class="algorithm">Linear</th>
   <th class="algorithm">Lánczos</th>
 </tr></thead>
<tbody>
 <tr>
   <th>4</th>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-bath-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-bath-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-bath-4x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-quixote-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-quixote-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-quixote-4x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-place-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-place-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-place-4x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-renoir-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-renoir-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-renoir-4x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-4x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-4x-nearest.png" type="image/png"><img src="/assets/images/lanczos-peacock-4x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-peacock-4x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-peacock-4x-Lanczos.png"></picture></div></td>
 </tr>
 <tr>
   <th>2</th>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-2x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-bath-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-bath-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-bath-2x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-quixote-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-quixote-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-quixote-2x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-place-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-place-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-place-2x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-renoir-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-renoir-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-renoir-2x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-2x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-2x-nearest.png" type="image/png"><img src="/assets/images/lanczos-peacock-2x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-2x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-2x-linear.png" type="image/png"><img src="/assets/images/lanczos-peacock-2x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-2x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-2x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-peacock-2x-Lanczos.png"></picture></div></td>
 </tr>
 <tr>
   <th class="original">1</th>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-1x.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-1x.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-1x.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-1x.png" type="image/png"><img src="/assets/images/lanczos-bath-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-1x.png" type="image/png"><img src="/assets/images/lanczos-bath-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-1x.png" type="image/png"><img src="/assets/images/lanczos-bath-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-1x.png" type="image/png"><img src="/assets/images/lanczos-quixote-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-1x.png" type="image/png"><img src="/assets/images/lanczos-quixote-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-1x.png" type="image/png"><img src="/assets/images/lanczos-quixote-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-1x.png" type="image/png"><img src="/assets/images/lanczos-place-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-1x.png" type="image/png"><img src="/assets/images/lanczos-place-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-1x.png" type="image/png"><img src="/assets/images/lanczos-place-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-1x.png" type="image/png"><img src="/assets/images/lanczos-renoir-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-1x.png" type="image/png"><img src="/assets/images/lanczos-renoir-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-1x.png" type="image/png"><img src="/assets/images/lanczos-renoir-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-1x.png" type="image/png"><img src="/assets/images/lanczos-peacock-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-1x.png" type="image/png"><img src="/assets/images/lanczos-peacock-1x.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-1x.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-1x.png" type="image/png"><img src="/assets/images/lanczos-peacock-1x.png"></picture></div></td>
 </tr>
 <tr>
   <th>½</th>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.5x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-bath-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-bath-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-bath-0.5x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.5x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-place-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-place-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-place-0.5x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.5x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.5x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.5x-nearest.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.5x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.5x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.5x-linear.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.5x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.5x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.5x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.5x-Lanczos.png"></picture></div></td>
 </tr>
 <tr>
   <th>¼</th>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-gutenberg-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-0.25x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-bath-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-bath-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-bath-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-bath-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-bath-0.25x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-quixote-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-quixote-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-quixote-0.25x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-place-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-place-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-place-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-place-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-place-0.25x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-renoir-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-renoir-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-renoir-0.25x-Lanczos.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.25x-nearest.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.25x-nearest.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.25x-nearest.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.25x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.25x-linear.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.25x-linear.png"></picture></div></td>
   <td><div class="sample"><picture><source srcset="/assets/images/lanczos-peacock-0.25x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-peacock-0.25x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-peacock-0.25x-Lanczos.png"></picture></div></td>
 </tr>
</tbody>
</table>
</div>

We will proceed as follows:

1. [A description of the problem we're trying to solve;](#problem)
2. [How interpolation can be phrased in terms of convolutions;](#convolutions)
3. [An introduction to $\mathrm{sinc}$, the "best" interpolation function, together with a quick recap on Fourier transforms;](#sinc)
4. [A concrete example of interpolating with $\mathrm{sinc}$;](#sinc-in-practice)
5. [Why we can't and shouldn't use $\mathrm{sinc}$ directly;](#sinc-problems)
6. [How $\mathrm{sinc}$ can be modified to make it work well in practice;](#lanczos)
7. [Some closing thoughts.](#closing-thoughts)

If you're already familiar with Fourier analysis, section 6 might still be of interest, given that it explains the main ideas behind Lánczos interpolation.

Let's get started.

## The problem {#problem}

When resizing an image, we generally have a specific target size in mind. In this post, we focus on the general problem of "filling in" the gaps between regularly spaced samples --- that is, interpolating. Once we know how to interpolate between samples, we can resample at will, including downsampling and upsampling.

Let's consider the 1-dimensional case --- we'll see shortly how to extend 1D interpolation techniques to two dimensions. Here's a 1-dimensional signal sampled at a fixed interval:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-example-fun.svg)
</div>

We'll generally call the "original" signal from which the samples are taken $f(t)$, and refer to the input as "time" to go along with digital signal processing terminology.

_Linear interpolation_ is just connecting the dots:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-example-fun-linear.svg)
</div>

We'll call the interpolated signal $\bar{f}(t)$, shown above in gold. More samples mean better interpolation:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-example-fun-linear-more-samples.svg)
</div>

<div>

Cubic interpolation fits a third-degree polynomial between each point and generally produces a smoother, more pleasant interpolation:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-example-fun-cubic.svg)
</div>

If we can interpolate in one dimension, we can easily extend it to two by first interpolating along one axis, then interpolating the interpolated 1-dimensional signals:

</div>

<div>

<div class="center-image" style="margin-bottom: 1rem;">
<figure>
<img src="../assets/images/Comparison_of_1D_and_2D_interpolation.svg" alt="Courtesy of Cmglee on Wikipedia" style="max-height: 25rem"><figcaption aria-hidden="true"><small>Courtesy of <a href="https://commons.wikimedia.org/wiki/File:Comparison_of_1D_and_2D_interpolation.svg">Cmglee on Wikipedia</a></small></figcaption>
</figure>
</div>

Here, the discrete samples are colored, and the interpolated points are shown in black. Note how linear interpolation needs two samples, while cubic needs four -- the number of points we need for a unique line and cubic respectively.[^cubic-methods]

[^cubic-methods]: In practice, when imaging software performs "cubic interpolation", it is not actually fitting polynomials to points.

    Instead, it usually performs something quite similar to Lánczos interpolation, with the goal of having the output resemble what we'd get if we actually fit polynomials.

    ImageMagick [has a long list](https://legacy.imagemagick.org/Usage/filter/) of such cubic filters --- see Catmull-Rom or Mitchell filters for examples of cubic filters which resemble Lánczos.

<!--
Here's another image explaining in the context of images in particular:

<div class="center-image" style="margin-bottom: 1rem;">
![<small>Courtesy of [ImageMagick](https://legacy.imagemagick.org/Usage/misc/)</small>](/assets/images/imagemagick-interpolation-diagram.png)
</div>
-->

We'll only be concerned with 1-dimensional interpolation from this point forward.

</div>

## Interpolation and convolution {#convolutions}

It is mathematically and practically helpful to frame interpolations as convolutions.

Let's imagine that our signal $f(t)$ is sampled at regular intervals so that they are $1/\xi$ apart from each other. We'll say that $\xi$ is the _frequency_ at which the samples are taken -- the higher the frequency, the denser the samples. We'll also use $\xi$ to talk about frequencies in the spectrum of $f(t)$ later on, and relate the two.

For instance, using our previous function and $\xi = 2$:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-evenly-spaced.svg)
</div>

We'll refer to a single sample with $f_k$, where $f_k$ is taken at time $k/\xi$. Given some interpolation function $g$, we can convolve it with our discrete samples to get an interpolated signal $\bar{f}(t)$:

$$
\bar{f}(t) = \sum_{k = -\infty}^{+\infty} g(t - k/\xi) \, f_k 
$$

If the sum above is a bit opaque, don't worry, visual aid is coming shortly.

Within this framework, linear interpolation can be seen as interpolating with an appropriately scaled triangle function:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-triangle.svg)
</div>

The first thing to note is that when $g(t) = \mathrm{triangle}(\xi t)$, convolving at the sample points will not change them. Or in other words, the convolution at $k/\xi$ will multiply $f_k$ by $1$, and all the other $f_k$s by $0$, with the result of having $\bar{f}(k/\xi) = f(k/\xi)$:

Let's see this in action when $\xi = 2$, and $t = -3/\xi$:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-triangle-1.svg)
</div>

We're showing the interpolating function $\mathrm{triangle}(t)$ in green, the green points are each term of the convolution sum, and the gold point is the interpolated point (that is, the sum of the green points, which is the convolution).

As you can see, the only point that contributes is $f_{-3} = f(-3/2)$. Note that the only non-zero green point is hidden behind the gold point -- since they have the same value.

Leaving the samples intact is a desirable and guiding property for interpolating functions.

Between sample points is where things get interesting:

<div class="center-image full-width-image">
![](/assets/images/lanczos-triangle-2.svg)
</div>
<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-triangle-3.svg)
</div>

The triangle will mix the two points to its left and right, producing the linear interpolation in the middle. Running the convolution at every point, we get back our linear interpolation:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-triangle-linear.svg)
</div>

The triangle function is a very basic option, but we'll soon discuss a fancier interpolation function.

## $\mathrm{sinc}$, master interpolator {#sinc}

We now get talking about $\mathrm{sinc}$, which is the basis for Lánczos interpolation and many other interpolation filters.

$\mathrm{sinc}$ is defined as

$$
\mathrm{sinc}(t) = \frac{\sin t}{t}
$$

The hole at $t = 0$ is plugged by setting $\mathrm{sinc}(0) = 1$. It looks like this:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-sinc.svg)
</div>

The first good property of $\mathrm{sinc}$ is that it interpolates well in the sense that it keeps samples intact: we have $\mathrm{sinc}(0) = 1$, and then we have evenly spaced zeros. By default the zeros are spaced by $\pi$, but we can scale it to work with any frequency $\xi$ using $\mathrm{sinc}(\pi \xi t)$:

<div class="center-image full-width-image" style="margin-bottom: 1rem;">
![](/assets/images/lanczos-sinc-1.svg)
</div>

Just like with $\mathrm{triangle}$, the samples are preserved, given the very conveniently placed zeros. But just like before, the interesting case is when we're interpolating _between_ samples:

<div class="center-image full-width-image">
![](/assets/images/lanczos-sinc-2.svg)
</div>
<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-3.svg)
</div>

I've centered the interpolation point to better show what happens to the sides. While $\mathrm{triangle}$ considers only the two closest points, $\mathrm{sinc}$ considers an infinite number of points to its left and right.

This is what the full interpolation looks like for our function:

<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-convolution.svg)
</div>

Nice and smooth!

Beyond being a well-behaved interpolating function, $\mathrm{sinc}$ has a second property which makes it special in this realm.

Before going forward, let's recap on a few Fourier ingredients:

* A [very large class of functions](https://en.wikipedia.org/wiki/Dirichlet_conditions#Dirichlet_conditions_for_Fourier_transform) can be represented as the sum of $\cos \pi \xi$ and $\sin \pi \xi$ oscillations (or frequencies).

* The _Fourier transform_ $\mathcal{F}\{f(t)\}(\xi)$ of $f(t)$ tells us "how much" of a frequency $\xi$ is present in $f(t)$.

* $\mathcal{F}\{f(t)\}$ uniquely determines $f$, and vice-versa.

<div>

$\mathrm{sinc}$'s Fourier transform[^spectrum] looks a bit like a brick wall. Here it is compared to the spectrum of $\mathrm{triangle}$:[^which-fourier]

[^spectrum]: We'll use Fourier transform and _spectrum_ and _frequency response_ interchangeably.

[^which-fourier]: We're doing two common things to simplify the Fourier transform plots:

    * We plot the positive part only since the functions we're analyzing are real-valued, and therefore the negative side will be identical.
    * We plot the absolute value of Fourier transform, which is complex-valued. This is usually what we want, but in this case it is also irrelevant since the interpolation functions we're analyzing are all [even](https://en.wikipedia.org/wiki/Even_and_odd_functions), and therefore only contain $\cos$ frequencies.
    
    We're using Mathematica to both compute and plot the frequency response -- see the function `FT` in [the notebook.](/assets/other/lanczos.html)

<div class="center-image full-width-image">
![](/assets/images/lanczos-sinc-vs-triangle.svg)
</div>
<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-vs-triangle-spectrum.svg)
</div>

The spectrum of $\mathrm{sinc}(\pi t)$ is constant within $1/2$, and $0$ elsewhere. Or in other words, $\mathrm{sinc}(\pi t)$ contains a constant amount of frequencies below $1/2$, and no frequencies beyond that.

The spectrum of $\mathrm{triangle}$, on the other hand, tapers smoothly. This tapering reflects the blurring we witness when resizing images with linear interpolation.

If we increase the $\xi$ in $\mathrm{sinc}(\pi \xi t)$, the spectrum will widen accordingly:

<div class="center-image full-width-image">
![](/assets/images/lanczos-sinc-vs-sinc2.svg)
</div>
<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-vs-sinc2-spectrum.svg)
</div>

</div>

With $\xi = 2$ the spectrum is now of width $1$, although with halved magnitude. The halving is compensated by the fact that we have double the zeros (that is, double the samples when interpolating).

When we're interpolating by convolving we're adding up scaled versions of $\mathrm{sinc}$:

$$
\bar{f}(t) = \sum_{k = -\infty}^{+\infty} \mathrm{sinc}(\xi \pi (t - k/\xi)) \, f_k 
$$

<div>

This means that the spectrum of $\bar{f}(t)$ will also be limited to $\xi/2$ just like $\mathrm{sinc}(\pi \xi t)$ is.[^linear-fourier]

[^linear-fourier]: I'm being pretty handwavy here, but you can convince yourself of this mathematically too [since the Fourier transform is a linear operator, and given that translation does not change the width of the spectrum.](https://en.wikipedia.org/wiki/Fourier_transform#Basic_properties)

Moreover, _samples taken at a rate of $\xi$ uniquely identify functions with a spectrum of width less than $\xi/2$_. Or in other words, there's only one function with frequencies limited to less than $\xi/2$ passing through a given set of samples taken at a rate of $\xi$.

Intuitively, what this means is that if we take enough samples, the frequencies have nowhere to hide and are fully determined by the samples.

</div>
<div>

This key fact is known as the Shannon-Nyquist sampling theorem,[^shannon-paper] and implies that if the spectrum for $f(t)$ is narrower than $\xi/2$, and if we generate $\bar{f}(t)$ using $\mathrm{sinc}(\pi \xi t)$, we'll have that $f(t) = \bar{f}(t)$.

[^shannon-paper]: The [original landmark paper](https://fab.cba.mit.edu/classes/S62.12/docs/Shannon_noise.pdf) is still one of the clearest sources for an explanation of the theorem and its mathematical details.

So, if we sample densely enough, $\mathrm{sinc}$ can reconstruct continuous signals from discrete samples _perfectly!_ This is the sense in which $\mathrm{sinc}$ is the "best" interpolating function, given that it is the _only_ function with these characteristics.

We've gone through a lot of mathy details quickly, but what's important to remember is:

* $\mathrm{sinc}$ interpolates leaving samples intact;
* The spectrum for $\mathrm{sinc}(\pi \xi t)$ is constant within $\xi/2$, and zero otherwise;
* Convolving adds scaled and translated versions of $\mathrm{sinc}(\pi \xi t)$, so the spectrum of the convolution will still be constrained to $\xi/2$;
* The facts above, together with Shannon-Nyquist, allow $\mathrm{sinc}$ to reconstruct signals perfectly, assuming we sample densely enough.

In the next section we'll gain a more intuitive understanding for how this process works out.

</div>

## $\mathrm{sinc}$ in practice {#sinc-in-practice}

Let's put this knowledge in action with our example function, which just so happens to be formed of three individual frequencies:

$$
f(t) = \sin \pi \frac{t}{5} + \frac{1}{2}\cos \pi t + \frac{1}{3}\sin \pi 2 t
$$

<div class="center-image lanczos-table" style="margin-bottom: 1rem;">
<table>
  <tr>
  <td>$\sin \pi t/5$</td>
  <td>$\frac{1}{2} \cos \pi t$</td>
  <td>$\frac{1}{3} \sin \pi 2 t$</td>
  </tr>
  <tr>
  <td>![](/assets/images/lanczos-example-fun-harmonics-1.svg)</td>
  <td>![](/assets/images/lanczos-example-fun-harmonics-2.svg)</td>
  <td>![](/assets/images/lanczos-example-fun-harmonics-3.svg)</td>
  </tr>
</table>
</div>

Usually, the Fourier transform of a function will be continuous. However, since we're dealing with three isolated frequencies, for this signal it will not even be a proper function, but rather one impulse for each of the three frequencies. We might plot as follows:

<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-example-fun-freqs.svg)
</div>

Shannon-Nyquist tells us that that we need to sample at a frequency greater than twice the maximum frequency in the spectrum. In this case the maximum frequency is $1$, so we need to sample using some $\xi > 2$. If we do that and convolve with a scaled $\mathrm{sinc}$, we should reconstruct the signal exactly:

<div class="large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-reconstruct.svg)
</div>

<div>

The dramatic switch from $\xi = 2$ to $\xi = 2.01$ is no accident: our highest frequency sits right at the boundary of the spectrum, so we need to go past it to capture it.[^dirac-deltas]

[^dirac-deltas]: This is why this post and most sources (including Shannon's original paper) are careful to state we need to sample at a rate _greater_ than $\xi$.

While I wanted to give a good intuition on why $\mathrm{sinc}$ is such a mathematically sound interpolator, when interpolating we usually don't really know nor care what the frequency of the original image is. However, $\mathrm{sinc}$ is still the starting point for many interpolation functions given its ideal properties.

</div>

## The problem with $\mathrm{sinc}$ {#sinc-problems}

Now that we know about how convolving with $\mathrm{sinc}$ interpolates points, we still face problems. The first problem is evident: to convolve with $\mathrm{sinc}$, we need to consider all the samples we have every time. This is clearly impractical: we'd need to look at every pixel in the input image to generate each pixel in the output!

The second problem is what happens when our spectrum is _not_ of limited width. Let's consider a simple stepping function, and its spectrum:

<div class="large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sign.svg)
</div>

As you can see, the spectrum of $\mathrm{sgn}(t)$ tapers but never reaches zero. This makes sense: to identify the precise moment where the function goes from $-1$ to $+1$, we'd need samples with no distance between them. So we can never reconstruct this function exactly with a finite number of samples.

That said, one might still be surprised at how poorly $\mathrm{sinc}$ performs in situations like this:

<div class="large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-reconstruct-sign.svg)
</div>

<div>

The reconstructed signal repeatedly overshoots and undershoot our original function. These undesirable oscillations, known as _Gibbs phenomenon_, show up all the time in Fourier analysis when dealing with jump discontinuities and finite approximations. They are intimately related to $\mathrm{sinc}$ -- in a sense the Gibbs oscillations are all ghosts of $\mathrm{sinc}$ in one form or another.[^si]

[^si]: In this case, the reconstructed signals are very close to (although not exactly the same as) the "sine integral" function:

    $$
    \mathrm{Si}(z) = \int_0^z \mathrm{sinc}(t) \, dt
    $$

Lánczos interpolation will address both problems presented in this section.

</div>

## $\mathrm{sinc}$, chopped and screwed {#lanczos}

Let's first consider the problem of $\mathrm{sinc}$ extending into infinity, and therefore requiring to examine all samples. Our first attempt to solve this issue might be just to just set $\mathrm{sinc}$ to zero outside a certain window:

<div class="center-image full-width-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-3-lobed.svg)
</div>

This allows our convolution to be made up of "only" 7 terms, rather than an infinite number of terms. For images, that would be $7 \times 7 = 49$ terms, given we need to go in both dimensions. Not a tiny number, but manageable.

We'll refer to function set to zero when $|t| \ge a$ as $\langle f(t) \rangle_a$:

$$
\langle f(t) \rangle_a = \begin{cases}
 f(t) & \text{if}\ -a < t < a, \\
 0 & \text{otherwise}.
\end{cases}
$$

How will $\langle \mathrm{sinc} \rangle_3$ fare as an interpolation function? The first thing to notice is that we're still stuck with the Gibbs oscillations in the reconstructed signal:

<div class="center-image full-width-image" style="margin-bottom: 1rem; margin-top: 1rem">
![](/assets/images/lanczos-truncated-sinc-ringing.svg)
</div>

In images, this shows up in so-called "ringing" artifacts:

<div class="center-image sample-big" style="margin-top: 1rem; margin-bottom: 1rem">
<picture><source srcset="/assets/images/lanczos-gutenberg-4x-600px-sinc.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-600px-sinc.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-600px-sinc.png" style="border-radius: 0.25rem;"></picture>
</div>

Those "echoes" around the letters are the same phenomenon as the wobbles in our reconstructed step function. They are particularly evident in images when text is present -- the glyphs' edges being our jump discontinuities.

However, to characterize how $\langle \mathrm{sinc} \rangle_a$ fares mathematically we can again look at its spectrum compared to $\mathrm{sinc}$'s. Here's the spectrum of $\langle \mathrm{sinc} \rangle_a$ with $a$ set to $3$, $5$, and $10$ respectively:

<div class="extra-large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-sinc-vs-truncated-spectrum.svg)
</div>

<div>

The Gibbs oscillations spoil the fun again. Earlier, we didn't have enough samples, this time, we have too little $\mathrm{sinc}$, but it's the same phenomenon. Lánczos' key idea was to exploit its specific nature to modify our truncated function and counteract them.[^series]

[^series]: Lánczos actually developed these techniques to deal with truncated Fourier series rather than truncated interpolation functions, but exactly the same line of reasoning applies.

The oscillations in the spectrum for our truncated function follow a frequency that depends directly on how wide our window is -- that is, how big $a$ is. We can immediately see this from the last plot: as we increase $a$, we're getting more rapid oscillations.

We're going to make this precise now, so we need to get a bit technical. The maths is not required to understand the intuition behind Lánczos interpolation -- but it is useful to grasp the ideas that underpin it.

We've been throwing the Fourier transform around, but let's define it exactly, alongside its inverse:

</div>

$$
\begin{equation*}
\begin{split}
\mathcal{F}\{f(t)\}(\xi) = & \int_{-\infty}^{+\infty} f(t) \, e^{-2 \pi i \xi t} \, dt \\
f(t) = &\int_{-\infty}^{+\infty} \mathcal{F}\{f(t)\}(\xi) \, e^{+2 \pi i \xi t} \, d\xi
\end{split}
\end{equation*}
$$

As usual we're packing both $\sin$ and $\cos$ oscillations together using Euler's formula:

$$
e^{i x} = \cos x + i \sin x
$$

The Fourier transform picks out how much a given frequency contributes to the function by looking how much they "line up".

What we want to do is modify $\langle f(t) \rangle_a$ so that its spectrum is as close as possible to that of $f(t)$. Our starting point will be characterizing the nature of the error in the spectrum more precisely. First, let's split the spectrum of $f(t)$ in three parts:

$$
\mathcal{F}\{f(t)\}(\xi) = \int_{-\infty}^{-a} f(t) e^{-2 \pi i \xi t} \, dt + \int_{-a}^{+a} f(t) e^{-2 \pi i \xi t} \, dt + \int_{+a}^{+\infty} f(t) e^{-2 \pi i \xi t} \, dt
$$

The error for the spectrum of $\langle f(t) \rangle_a$ will be the first and third term:

$$
\mathcal{F}\{f(t)\}(\xi) - \mathcal{F}\{\langle f(t) \rangle_a\}(\xi) = \int_{-\infty}^{-a} f(t) \, e^{-2 \pi i \xi t} \, dt + \int_{+a}^{+\infty} f(t) \, e^{-2 \pi i \xi t} \, dt
$$

These are the annoying oscillations we see for our truncated $\mathrm{sinc}$s, and that we do _not_ want. Considering only the right hand side of the sum above, and reworking it a bit, we get:

$$
\begin{equation*}
\begin{split}
\int_{+a}^{+\infty} f(t) \, e^{-2 \pi i \xi t} \, dt & = \int_{0}^{+\infty} f(t+a) \, e^{-2 \pi i \xi (t + a)} \, dt\\
                                                     & = e^{-2 \pi i \xi a} \int_{0}^{+\infty} f(t+a) \, e^{-2 \pi i \xi t} \, dt
\end{split}
\end{equation*}
$$

<div>

We've now phrased the error as a modulation of the frequency $e^{-2 \pi i \xi a}$. Again, pause to appreciate that this "carrier wave" gets faster as $a$ grows, which is consistent with how we witnessed our oscillations get more rapid as we truncated $\mathrm{sinc}$ with a wider window.

Here's the error for the spectrum of $\langle \mathrm{sinc} \rangle_3$, together with its carrier wave $\cos 2 \pi 3 \xi$:[^cos]

[^cos]: As remarked in a previous footnote, here we can get away with only considering $\cos$, since the functions we're analyzing are [even](https://en.wikipedia.org/wiki/Even_and_odd_functions).

    Also, the absolute, 20x downscaled $\cos 2 \pi \xi 3$ is plotted, so that it can be better shown together with the error.

<div class="full-width-image center-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-carrier-wave.svg)
</div>

</div>

Lánczos' next step was to "smooth out" the undesired oscillations by averaging out the result by the period of that frequency:

$$
\text{smoothed} \: \mathcal{F}\{\langle f(t) \rangle_a\}(\xi) = a \int_{\xi-1/2 a}^{\xi+1/2a} \mathcal{F}\{\langle f(t) \rangle_a\}(\xi_0) \, d \xi_0
$$

The period of $e^{-2 \pi i \xi a}$ is $1/a$, so we modify the spectrum by taking the average with a window of that width at each point.

This is what the "smoothed" spectrum looks like for $\langle \mathrm{sinc} \rangle_3$, $\langle \mathrm{sinc} \rangle_5$, and $\langle \mathrm{sinc} \rangle_{10}$:

<div class="extra-large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-smoothed-spectrum.svg)
</div>

It tracks the original spectrum much more closely -- in fact it's useful to focus on the area around the cutoff to see what's going on:

<div class="extra-large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-smoothed-spectrum-zoom.svg)
</div>

<div>

The smoothed spectrum _does_ taper a bit earlier. But this is a compromise well worth taking. Moreover, the convergence to the spectrum of $\mathrm{sinc}$ as $a$ grows to infinity is not compromised.

This is all well and good, but how do we apply this in practice? Well, Lánczos' final trick was to prove that we can smooth out the spectrum of any truncated function in this way by multiplying the function pointwise with $\mathrm{sinc}(\pi t / a)$. That's right, it's $\mathrm{sinc}$ again!

So, instead of $\langle f(t) \rangle_a$, we'd use

$$
\langle f(t) \rangle_a \, \mathrm{sinc}(\pi t / a)
$$

This $\mathrm{sinc}$ stretched to match the Gibbs oscillations is called the _Lánczos window_.[^proving] Wanting to smoothen the spectrum of $\langle \mathrm{sinc}(\pi t) \rangle_a$ itself, we'll use

[^proving]: Deriving the Lánczos window would make this blog post too long and technical for my tastes. Lánczos himself derives it not by directly taking the inverse Fourier transform of the smoothed spectrum, but by adjusting the derivative of the error term.

    You can head to section IV.6 of Lánczos' Applied Analysis for details. It is available for free at [the Internet Archive.](https://archive.org/details/appliedanalysis00lanc_0/page/219/mode/1up)

    I also want to stress that Lánczos' method for reducing the Gibbs phenomenon in truncated series is very general, and using it for interpolation together with $\mathrm{sinc}$ is just one (very useful) application.

$$
\langle \mathrm{sinc}(\pi t) \rangle_a \, \mathrm{sinc}(\pi t / a)
$$

Lánczos interpolation consists of convolving our samples with this chopped and modified $\mathrm{sinc}$. This is what this new interpolation function looks like:

</div>

<div class="large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-window.svg)
</div>

The plot above shows $\mathrm{sinc}_3$, the Lánczos window, and their pointwise multiplication. The spectrum is smoothed in exactly the way we desire. $\mathrm{sinc}$ and its spectrum are also shown for reference.

While we set out to mimick $\mathrm{sinc}$'s spectrum, Lánczos interpolation actually works _better_ than interpolating with $\mathrm{sinc}$ in the presence of jump discontinuities, due to its dampened curve:

<div class="extra-large-image" style="margin-bottom: 1rem">
![](/assets/images/lanczos-ringing-compare.svg)
</div>

<!--
TODO maybe be more specific of when exactly ringing arises?
-->

That said, a bit of ringing remains. This is the main drawback with Lánczos or similar filters, compared to linear interpolation or some cubic filters:

<table class="filters-showcase" style="margin-bottom: 1rem; margin-left: auto">
  <thead>
 <tr>
   <th style="font-weight: normal; font-size: 0.9rem;">Nearest</th>
   <th style="font-weight: normal; font-size: 0.9rem;">$\langle \mathrm{sinc} \rangle_3$</th>
   <th style="font-weight: normal; font-size: 0.9rem;">$\mathrm{Lánczos}_3$</th>
</thead>
<tbody>
 <tr>
   <td><div class="sample" style="max-width: none; max-height: none; border-top-left-radius: 0.5rem; border-bottom-left-radius: 0.5rem;"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-linear.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-linear.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-linear.png"></picture></div></td>
   <td><div class="sample" style="max-width: none; max-height: none;"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-sinc.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-sinc.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-sinc.png"></picture></div></td>
   <td><div class="sample" style="max-width: none; max-height: none;border-top-right-radius: 0.5rem; border-bottom-right-radius: 0.5rem;"><picture><source srcset="/assets/images/lanczos-gutenberg-4x-Lanczos.webp" type="image/webp"><source srcset="/assets/images/lanczos-gutenberg-4x-Lanczos.png" type="image/png"><img src="/assets/images/lanczos-gutenberg-4x-Lanczos.png"></picture></div></td>
 </tr>
</tbody>
</table>

But for many applications, Lánczos (or something close to it) will be the best compromise.

## Closing thoughts {#closing-thoughts}

When I set out to write this post, I wanted to work up to Lánczos interpolation using just high-school maths. I couldn't find a way to explain it without getting quite technical, so I downgraded to an explanation targetet to an audience who has at least heard of the Fourier transform.[^cs] That said, I still hope this post could illuminate these key areas in modern engineering through a concrete example.

[^cs]: Yes, I'm talking to _you_, computer science undergraduate!

Cornelius Lánczos himself brought forward a multitude of numerical methods. I highly recommend ["Applied Analysis"](https://archive.org/details/appliedanalysis00lanc_0), the book that introduced the technique explained in this post. Some of it is outdated, explaining how to manually compute roots of polynomials or solutions to linear systems. It's still very interesting, and written in an unusual conversational style that I find endearing.

Lánczos also strikes me as a delightful man, at least from two interviews I found on YouTube [about mathematics](https://www.youtube.com/watch?v=avSHHi9QCjA) and [about his life](https://www.youtube.com/watch?v=PO6xtSxB5Vg).

Another reason to write this blog post was to showcase how much one can get out of software like Mathematica when playing around with such topics. In the end I mostly used Mathematica for plots, but I encourage you to look at [the notebook](/assets/other/lanczos.html) to get a peek of what's possible.

## Acknowledgements

Many thanks to Alex Appetiti, [Alexandru Sçvortov](https://scvalex.net/), Alex Sayers, Stephen Lavelle, and Kurren Nischal for reviewing drafts of this blog post.

<script>
  window.lanczosImageScaling();
</script>
