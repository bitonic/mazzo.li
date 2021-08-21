---
title: Type Checking through Unification
date: 2016-10-03
tags:
- paper
---

TL; DR: You can read about a nifty algorithm to type-check dependently typed
languages [on arXiv](https://arxiv.org/abs/1609.09709).

During my 10 months stint as a PhD student I worked on the implementation of
dependently typed programming languages. I left mostly because I wanted to go
back to Italy, but I still love the subject -- I hope to find the time to work
on it again at some point!

In any case, I did produce something of interest while there, and recently I was
prompted to put it somewhere where people can refer to it, and now it's
[on arXiv](https://arxiv.org/abs/1609.09709).

In short, if your unification is powerful enough (and it is if your language is
Agda/Epigram/...), every type checking problem can be easily expressed as a set
of unification constraints. This simplifies the code in the type checker
considerably, trapping most of the complexity in the unifier.

I realized that you could do this after talking to Adam Gundry regarding how
Epigram treated uses of equality proofs that were not fully realized yet, but to
my knowledge my prototype was the first to take this to the extreme consequences
and do type checking entirely as unification constraints.
