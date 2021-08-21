---
title: "Haskell and OpenCV: theory and practice"
date: 2016-10-17
tags:
- talk
---

I was lucky enough to talk at the Haskell eXchange this year after having a lot
of fun there last year. To continue on the `inline-c` theme, I gave a demo of
the [excellent OpenCV bindings](https://github.com/LumiGuide/haskell-opencv)
that Bas and Roel van Dijk wrote.

I explained how the bindings work and then proceeded to write a few filters
that worked on the live feed from my webcam, including a nice Snapchat inspired
filter that replaces human eyes with nice manga eyes:

<div class="center-image">![Manga eyes filter](/assets/images/manga-eyes-snapshot.png)</div>

You can find a recording of the video at
<https://skillsmatter.com/skillscasts/8991-haskell-and-opencv-theory-and-practice>,
the repository with the showcased code at
<https://github.com/bitonic/hs-cam-filter>, and the slides at
<http://mazzo.li/assets/other/haskell-exchange-2016-slides.pdf>.
