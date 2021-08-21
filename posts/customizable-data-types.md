---
title: "Configurable data types"
date: 2017-06-19
---

Types are great, but sometimes organizing them and changing them can be
very painful. This problem is particularly evident when manipulating
expressions -- for example when writing a compiler, a type checker, an
interpreter, etc.

In that context, we often need to manipulate
many slightly different data types for our expressions, but we don't
want to redefine many generic functions (e.g. traversals, pretty printing)
for each version.

For example, consider some expression type `Expr`:

```
data Expr
  = Var Var
  | If Expr Expr Expr
  | ... -- many other cases
```

It could be the case that the definition of `Var`, the type of variables,
has to change across stages of our compiler: for example it could start
as a simple `Text` when parsing, but then change to a more sophisticated
`QualifiedName` after renaming. Similarly, we might want to desugar our
`If` expressions into case expressions
when elaborating the language.

However, it's likely the case that many functions on `Expr` can range over a large set of
`Var` types, and over expressions with or without `If`.[^pretty] It is
extremely annoying to define slight variations of the same function on
slightly different data types.

[^pretty]: Some examples: pretty printing, traversals that collect all mentioned
variables, serialization to some external format, etc.

One way out is to give up and fail at runtime if we encounter some data
constructor we don't expect. This approach is sometimes taken in GHC, and
as predictable it's easy to make mistakes.

Another option is parametrizing our data type, for example with

```
data Expr var if
  | Var var
  | If if
  | ...
```

but this approach gets unwieldly quite quickly and requires changing tons of
code when a type parameter is added or removed.

However there is a nicer trick that is very convenient in some situations,
that Ben Lippmeier suggested and that I have found very useful and expanded
upon. Instead of adding one parameter for each configurable part of our data type,
we have only one parameter (which I'll call "index") and a type family for
each configurable piece that we want. This is how it would look like
for our `Expr` type:

```
data Stage1 -- Right after parsing
data Stage2 -- After renaming
data Stage3 -- After desugaring

type family Var ix :: * where
  Var Stage1 = Text
  Var Stage2 = QualifiedName
  Var Stage3 = QualifiedName

type family If ix :: Bool where
  If Stage1 = 'True
  If Stage2 = 'True
  If Stage3 = 'False

data Expr ix
  = Var (Var ix)
  | (If ix ~ 'True) => If (Expr ix) (Expr ix) (Expr ix)
  | ...
```

Note that I'm using data kinds and closed type families, but it works equally
well with open type families and without data kinds.

This style of "configurable" data types is very amenable to refactorings, and
moreover functions can express precisely what kind of expression they manipulate.
For example we might have a function converting if statements in case expressions:

```
desugarIf :: (If ix ~ 'True, If ix' ~ 'False, Var ix ~ Var ix') => Expr ix -> Expr ix'
```

note how the function does not need to care what `Var ix` is, and moreover when
we add more configurability to the datatype the function type (and possibly the body)
won't need to be changed.

This style is not mutually exclusive with parametrization. For example if
we want our `Expr` to be a functor over the type of variables we can simply have
the parameter along with the index:

```
data Expr ix a
  = Var a
  | (If ix ~ 'True) => If (Expr ix a) (Expr ix a) (Expr ix a)
  | ...

instance Functor (Expr ix) where
  ...
```

Let me know what you think!
