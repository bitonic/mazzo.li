---
title: Parallelizing and distributing scientific software in Haskell
date: 2016-09-13
tags:
- talk
---

I recently gave a talk at ZuriHac on the pitfalls that one can incur on when
trying to parallelize Haskell software:

<div class="yt"><iframe width="600" height="337" src="https://www.youtube.com/embed/4py8BYIw1DI" frameborder="0" allowfullscreen></iframe></div>

Note that, as [one helpful YouTube commenter notes](https://www.youtube.com/watch?v=4py8BYIw1DI&lc=z13yg3nalvr3vp0aw04chrxwhmysubqh0sc0k), I wrongly
talk about "expected linear speedup", when really I mean "expected 1/n speedup".
The message of the talk stands, I was just confused when preparing the slides.

See also [experience report](https://www.fpcomplete.com/experience-scaling-computation) on FP Complete's website on this topic.
