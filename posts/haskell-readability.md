---
title: Haskell, Python, and Readability
date: 2012-02-27
---

This weekend on [/r/programming](http://www.reddit.com/r/programming) someone
posted a [nice
introduction](http://v1v3kn.tumblr.com/post/18238156967/roll-your-own-autocomplete-solution-using-tries)
to tries using Python. A while ago, I had implemented a [mini web
server](https://github.com/bitonic/suggest) to do completion and correction of
words using Ternary Search Tries in Haskell, and since the trie post generated a
lot of interest I decided to [post mine
too](http://www.reddit.com/r/programming/comments/q5dz2/roll_your_own_fast_completion_and_correction/).

Then, someone else posted a
[blog article](http://www.reddit.com/r/programming/comments/q5dz2/roll_your_own_fast_completion_and_correction/c3v6ruo)
commenting on the readability of Python and Haskell based on my web server code
and the trie example, concluding that the Python version was much more readable.

I personally prefer writing and reading Haskell, but I think that the comparison
is not fair since the data structures compared are very different. Thus, I
quickly coded an Haskell version of the code in the original blog post, so that
you can compare code that does, more or less, the same thing.

The only difference is that I don't store the current word in each node, since
it is not necessary---the current word can be kept track of when traversing the
path. Also, I'm using lists instead of sets when returning the completions, but
changing that would be trivial using `Data.Set`.

Moreover, the Haskell version has the advantage of working with any list, so for
example it would work with list of integers as well as Strings.  This could be
further abstracted to "unconsable" data types
([ListLike](http://hackage.haskell.org/package/ListLike) offers that and more)
but that's not that relevant here.

So, here's the original Python code:

```python
"""
A fast data structure for searching strings with autocomplete support.
"""

class Trie(object):
    def __init__(self, value=None):
        self.children = {}
        self.value = value
        self.flag = False # Flag to represent that a word ends at this node

    def add(self, char):
        val = self.value + char if self.value else char
        self.children[char] = Trie(val)

    def insert(self, word):
        node = self
        for char in word:
            if char not in node.children:
                node.add(char)
            node = node.children[char]
        node.flag = True

    def find(self, word):
        node = self
        for char in word:
            if char not in node.children:
                return None
            node = node.children[char]
        return node.value

    def all_prefixes(self, wlist):
        results = set()
        if self.flag:
            results.add(self.value)
        if not self.children: return results
        return reduce(lambda a, b: a | b,
                     [node.all_prefixes() for
                      node in self.children.values()]) | results

    def autocomplete(self, prefix):
        node = self
        for char in prefix:
            if char not in node.children:
                return set()
            node = node.children[char]
        return node.all_prefixes()
```

And this is the Haskell version:

```haskell
module Trie
    ( Trie
    , empty
    , insert
    , find
    , complete
    ) where

import           Data.Map (Map)
import qualified Data.Map as Map

-- | At each node, we store a 'Bool' indicating if we are at the end of
-- a word.
data Trie a = Trie (Map a (Trie a)) Bool

empty :: Ord a => Trie a
empty = Trie Map.empty False

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie tries _) =
    Trie tries True
insert word@(firstChar : rest) (Trie tries wordEnd) =
    case Map.lookup firstChar tries of
        Nothing ->
            insert word (Trie (Map.insert firstChar empty tries) wordEnd)
        Just trie ->
            Trie (Map.insert firstChar (insert rest trie) tries) wordEnd

find :: Ord a => [a] -> Trie a -> Bool
find [] (Trie _ wordEnd) =
    wordEnd
find (firstChar : rest) (Trie tries _) =
    maybe False (find rest) (Map.lookup firstChar tries)

complete :: Ord a => [a] -> Trie a -> [[a]]
complete [] (Trie tries wordEnd) =
    (if wordEnd then [[]] else []) ++
    concat [map (char :) (complete [] trie) | (char, trie) <- Map.toList tries]
complete (firstChar : rest) (Trie tries _) =
    maybe [] (map (firstChar :) . complete rest) (Map.lookup firstChar tries)
```

I didn't read the Python version too carefully so if I missed something let me
know!

You can discuss this post on
[reddit](http://www.reddit.com/r/programming/comments/q80nh/haskell_python_and_readability/).

* * *

**Update**: after the reddit discussion, I made the variable names a bit
clearer.
