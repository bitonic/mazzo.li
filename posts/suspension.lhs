---
title: "A well-typed suspension calculus"
date: 2017-05-19
---

When implementing interpreters for languages based on the lambda-calculus
one quickly moves away from naively substituting variables
when evaluating function application. This is mostly due
to the fact that terms can grow very big very fast, since
when substituting variables for some term said term will be
duplicated everywhere the variable is used.

In other words, if we have some term

```
(\x -> foo x x) SomeBigTerm
```

`SomeBigTerm` will be duplicated when substituting `x` for `SomeBigTerm`.

The solution is to not substitute eagerly. In most languages it
is quite easy to do so, for example using a [CEK machine](http://matt.might.net/articles/cek-machines/).

However, things get a bit tricky when one needs to compute
with free variables.[^free] This is the case in many dependently
typed languages, such as Agda or Coq. Agda perfomance in particular
has long suffered also because it substitutes terms too eagerly.

[^free]: Or "with open terms" or "under binders", depending to
who you are talking to.

This problem is also relevant in other languages that need to implement higher-order unification,
such as [lambda-Prolog](https://en.wikipedia.org/wiki/%CE%9BProlog).
The implementors of lambda-Prolog solved this problem by developing
a "suspension calculus", which allows substitutions to be delayed
until we really need them.[^susp]

[^susp]: For more about the suspension calculus as described by the
authors of lambda-Prolog, see [A Simplified Suspension Calculus and its Relationship to Other Explicit Substitution Calculi](https://arxiv.org/abs/cs/0702152),
by Andrew Gacek and Gopalan Nadathur.

The substitution calculus works with
[de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index). As
everybody who implemented algorithms involving de Bruijn indices knows,
it is _extremely_ easy to make mistakes and break the invariants
required for the indices to be well-formed. The suspension calculus
is no exception, and up to now I had trouble justifying its
rewrite rules.

However, it is possible to
encode what scope we're working on at the type level, thus making
operations involving de Bruijn indices much safer. This short
article is about implementing the suspension calculus using the
same techniques, so that we can implement its rewrite rules
with much more confidence.

The article is a literate Haskell file, you can save it and load it
using

```
$ stack --resolver lts-8.11 ghci
Prelude> :l suspension.lhs
```

You can also see the file [on GitHub](https://github.com/bitonic/website/blob/22b5cbf6fad4e63d9d77392f6fa6944bb55aab07/posts/suspension.lhs).

Let's get started.

Boring preamble
---

First of all, a few boring `LANGUAGE` extensions and import. Most of the
imports are needed for the pretty printing and parsing, which are
not really relevant to the article.

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE StrictData #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> import           Prelude hiding (head)
> import           Control.Applicative ((<|>), many)
> import           Control.Monad (void)
> import           Control.Monad.IO.Class (liftIO)
> import           Data.List (foldl', intersperse)
> import           Data.Text (Text)
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import           Data.HashMap.Strict (HashMap)
> import qualified Data.HashMap.Strict as HMS
> import qualified Data.HashSet as HS
> import           Data.Monoid ((<>))
> import           Data.Foldable (asum)
> import qualified System.Console.Haskeline as Haskeline
> import qualified Text.PrettyPrint.ANSI.Leijen as PP
> import qualified Text.Trifecta as Tri
> import qualified Text.Trifecta.Delta as Tri
> import qualified Text.Parser.Token as Parse
> import qualified Text.Parser.Combinators as Parse
> import qualified Text.Parser.Token.Highlight as Parse
> import qualified Text.Parser.Char as Parse

Variables and expressions
---


As I have mentioned, we will have our variables to be
well-typed, in the sense that the "depth" of the scope
of terms will be tracked at the type level. This
approach goes back at least to [de Bruijn notation as a nested datatype](http://staff.city.ac.uk/~ross/papers/debruijn.ps.gz)
by Bird and Paterson. You can refer to that paper for details,
but the core idea is quite simple.

We first define the type of variables for our terms:

> data Var a
>   = B -- Bound variable
>   | F a -- Free variable

This looks an awful lot like `Maybe`, but we define our own
type for nicer naming. The role of `B` and `F` will hopefully
become more clear as we define expressions, but intuitively
`B` refers to the most recently bound variable (like `0` if
we were using normal de Bruijn indices), and `F` refers to
a variable in the scope _without_ the most recently bound
variable.

Then we define `Syntax` and `Exp`, in tandem. `Syntax` specifies
the constructs of our lambda calculus: variables, applications,
lambda functions, and let bindings:

> data Syntax a
>   = Var a
>   | Lam Text (Exp (Var a)) -- Lambda function
>   | Let Text
>       (Exp a) -- Bound expression
>       (Exp (Var a)) -- Body of the let expression
>   | App
>       (Exp a) -- Function
>       (Exp a) -- Argument

We will define `Exp` shortly, but for now you can pretend it's
`Syntax`.

Note that the argument of `Syntax` indicates the scope of the
term. For example if we have a term of type

```
Syntax (Var (Var Text))
```

we know that it's a term with two bound variables (the two
`Var`) and some free variables represented by the
top-level `Text`. In such a term, we might have

```
F (F "someTopLevelDefinition")
F B -- The variable bound by the second most recent lambda or let
B -- The variable bound by the most recent lambda or let
```

Also note that we store a piece of `Text` in `Lam` and `Let`
to easily pretty print terms.

That said, `Exp` is either a piece of `Syntax`, or a _suspended_ piece
of `Syntax`:

> data Exp a where
>   Syntax :: Syntax a -> Exp a
>   Susp :: Env from to -> Syntax from -> Exp to
>   -- `Susp` stands for "suspension"

`Env b a` is some data structure containing information to turn a
term with scope `b` into a term in scope `a`. For example, something
of type

```
Env (Var (Var Text)) (Var Text)
```

contains information on how to remove one free variable out of
a term. We will define `Env` shortly, but first let's define a
couple of shortcuts to form `Exp`s quickly:

> var :: a -> Exp a
> var v = Syntax (Var v)
>
> lam :: Text -> Exp (Var a) -> Exp a
> lam n body = Syntax (Lam n body)
>
> let_ :: Text -> Exp a -> Exp (Var a) -> Exp a
> let_ n e1 e2 = Syntax (Let n e1 e2)
>

Environments
---

We can now get to the tricky part: defining environments that
let us delay substitution. We first define a GADT to specify
an _increase_ in scope depth:

> data Weaken from to where
>   WeakenZero :: Weaken a a
>   WeakenSucc :: Weaken from to -> Weaken from (Var to)

If we have `Weaken from to`, `to` it's going to be of the
form `Var (Var ... (Var from))`: it specifies an increase
of scope depth from `from`.

Then, a _canonical_ environment is either just a weakening,
or an existing environment added with an expression containing
the value for a bound varible:

> data CanonicalEnv from to where
>   EnvNil :: Weaken from to -> CanonicalEnv from to
>   EnvCons :: Text -> Exp to -> Env from to -> CanonicalEnv (Var from) to

Similarly to `Lam` and `Let`, we store the name of the variable
that `EnvCons` is referring to for easy pretty printing.

We call this form of environments canonical because we will
reduce all environments to this form. However, we can also
form environments by composition:

> data Env from to where
>   EnvCanonical :: CanonicalEnv from to -> Env from to
>   EnvComp :: Env a b -> Env b c -> Env a c

To recap:

`EnvNil wk` weakens terms by the amount specified by
`wk`. For example, applying

```
EnvCanonical (EnvNil (WeakenSucc WeakenZero)) :: Env a (Var a)
```

to a term of type `Exp a` will result in a term of type `Exp (Var a)`,
which will be implemented by applying all variables in the term to `F`.

`EnvCons n e env` will replace the first bound variable with
`e`, and then apply the environment `env`. For example, applying

```
EnvCanonical (EnvCons n e (EnvCanonical (EnvNil WeakeZero))) :: Env (Var a) a
```

to a term of type `Exp (Var a)` will result in a term of type
`Exp a`, where the first bound variable is replaced with `e`.

`EnvComp env1 env2` will apply first environment `env1` and then
environment `env2`.

Let's define some shortcuts to construct environments and suspensions:

> -- Takes an expression and forms the appropriate
> -- suspension applying the given environment.
> susp :: Env b a -> Exp b -> Exp a
> susp env = \case
>   Syntax e -> Susp env e
>   -- If the expression is already a suspension,
>   -- compose the environments.
>   Susp env' e -> Susp (EnvComp env' env) e
>
> envNil :: Env a a
> envNil = EnvCanonical (EnvNil WeakenZero)
>
> envCons :: Text -> Exp to -> Env from to -> Env (Var from) to
> envCons v e env = EnvCanonical (EnvCons v e env)
>
> envComp :: Env a b -> Env b c -> Env a c
> envComp = EnvComp
>
> envWeaken :: Env a b -> Weaken b c -> Env a c
> envWeaken env wk = envComp env (EnvCanonical (EnvNil wk))
>
> -- Useful to produce environments that work under
> -- abstractions (lambda and let) given an existing
> -- environment working outside the abstraction.
> envAbs :: Text -> Env from to -> Env (Var from) (Var to)
> envAbs v env = envCons v (var B) (envWeaken env (WeakenSucc WeakenZero))

Then, we can define a function to remove delayed
composition of environments. Although our environments
differ quite significantly from the ones in the original
lambda-Prolog paper, this is the trickiest part where
the well-typed scope are a really big help.

> evalEnv :: Env from to -> CanonicalEnv from to
> evalEnv = \case
>   -- If the environment is already canonical, stop
>   EnvCanonical env -> env
>   -- If we have a composition, evaluate the left
>   -- hand side and then compose...
>   EnvComp env1 env2 -> goComp (evalEnv env1) env2
>   where
>     -- Compose a canonical environment with normal environment
>     goComp :: CanonicalEnv a b -> Env b c -> CanonicalEnv a c
>     -- If the LHS is just a weakening, push the weakening
>     -- through.
>     goComp (EnvNil wk) env2 = goCompWeaken wk env2
>     -- If the LHS is a delayed substitution, return it
>     -- applying the RHS to the expression and composing
>     -- the rest of the environment.
>     goComp (EnvCons n e env1) env2 = EnvCons n (susp env2 e) (EnvComp env1 env2)
>
>     -- Composes a weakening and an environment.
>     goCompWeaken :: Weaken a b -> Env b c -> CanonicalEnv a c
>     goCompWeaken wk = \case
>       -- If the RHS is a weakening, just compose the two
>       -- weakenings
>       EnvCanonical (EnvNil wk') -> EnvNil (compWeaken wk wk')
>       -- If the RHS is a delayed substitution, drop it
>       -- if the weakening is non-zero, return the RHS
>       -- as-is otherwise.
>       EnvCanonical (EnvCons v e env) -> case wk of
>         WeakenZero -> EnvCons v e env
>         WeakenSucc wk' -> goCompWeaken wk' env
>       -- If the RHS is a composition, apply the weakening to the
>       -- LHS of the composition and then compose again.
>       EnvComp env1 env2 -> goComp (goCompWeaken wk env1) env2
>
>     -- Compose two weakenings -- same as addition
>     compWeaken :: Weaken a b -> Weaken b c -> Weaken a c
>     compWeaken wk1 WeakenZero = wk1
>     compWeaken wk1 (WeakenSucc wk2) = WeakenSucc (compWeaken wk1 wk2)

The only surprising rule is the one dropping the substitution
when a weakening is composed with an environment -- the intuition is that
if we've just weakened a term it surely can't refer to the first bound
variable.

Note that while these rules feel quite natural (and in fact are
pretty much forced by the types), they are natural because of
how we formulated environments, and that formulation was also
guided by the types. When pushing more property of the code in
the types, this often happens: data structures are largely driven
by making your programs type check more easily.
As Conor McBride [put it](https://skillsmatter.com/skillscasts/8893-is-a-type-a-lifebuoy-or-a-lamp),
types are lamps, not lifebuoys.

Once we have this function "evaluating" environments, we
can write a function taking a variable and looking up
into an environment:

> envLookup :: Env from to -> from -> Exp to
> envLookup env0 v = case evalEnv env0 of
>   -- If the environment is a weakening, just
>   -- weaken the variable by the required amount.
>   EnvNil wk -> var (weakenVar wk v)
>   -- If the environment is a cons, get the
>   -- bound expression if the variable is the
>   -- first bound variable, recurse otherwise.
>   EnvCons _b e env -> case v of
>     B -> e
>     F v' -> envLookup env v'
>
> weakenVar :: Weaken from to -> from -> to
> weakenVar wk0 v = case wk0 of
>   WeakenZero -> v
>   WeakenSucc wk -> F (weakenVar wk v)

Evaluating
---

Finally, we can evaluate expressions to their head normal
form (if they have one).

Normalized terms are either a lambda or a
free variable applied to some terms (a neutral
term):

> data Eval a
>   = EvalLam Text (Exp (Var a))
>   | EvalNeutral a [Exp a]

Then we define a function to push environments
substitution down the expression, giving us back
a `Syntax`:

> removeSusp :: Exp a -> Syntax a
> removeSusp = \case
>   Syntax e -> e
>   Susp env e0 -> case e0 of
>     -- Look up variables
>     Var v -> removeSusp (envLookup env v)
>     -- Push environments past lambdas...
>     Lam v body -> Lam v (susp (envAbs v env) body)
>     -- ..and lets.
>     Let n e1 e2 -> Let n (susp env e1) (susp (envAbs n env) e2)
>     -- Apply environments to both function and
>     -- argument.
>     App fun arg -> App (susp env fun) (susp env arg)

Then, evaluating a term is just a matter of creating
the right suspension and using `removeSusp`:

> eval :: Syntax a -> Eval a
> eval  = \case
>   -- If the term is a variable, return a neutral term
>   Var v -> EvalNeutral v []
>   -- If we have a lambda we're done
>   Lam v body -> EvalLam v body
>   -- If the term is an application, evaluate the function...
>   App fun0 arg -> case eval (removeSusp fun0) of
>     -- ...and add the argument if the function is neutral...
>     EvalNeutral v args -> EvalNeutral v (args ++ [arg])
>     -- ...or apply an environment containing the argument
>     -- if it's a lambda.
>     EvalLam v body -> eval (removeSusp (susp (envCons v arg envNil) body))
>   -- If we have a let, substitute the bound expression
>   -- in the body of the let.
>   Let n e1 e2 -> eval (removeSusp (susp (envCons n e1 envNil) e2))

And we're done! In the rest of the article I implement a parser
and pretty printer, which means that you can do

```
$ stack --resolver lts-8.11 ghci
Prelude> :l suspension.lhs
Main> repl
>>> \x -> x
Evaluated expression:
  \x -> x
Evaluated expression (no suspensions):
  \x -> x
>>> (\x -> x) foo
Evaluated expression:
  foo
Evaluated expression (no suspensions):
  foo
>>> let x = foo; x
Evaluated expression:
  foo
Evaluated expression (no suspensions):
  foo
```

Things get interesting when we evaluate expressions that
contain delayed substitutions:

```
>>> (\a b -> a) foo
Evaluated expression:
  \b ->
    $susp
      ($cons
        (b_1 := b)
        ($comp ($cons (a := foo) ($nil 0)) ($nil 1)))
      a
Evaluated expression (no suspensions):
  \b -> foo
>>> :{
  | let x = \y -> x y;
  | x foo
  | }:
Evaluated expression:
  x
    ($susp
      ($cons
        (y :=
          $susp ($cons (x := \y -> x y) ($nil 0)) foo)
        ($nil 0))
      y)
Evaluated expression (no suspensions):
  x foo
```

The first printed expression shows the full suspension
including the environment, where `$susp env a` represents
a `Susp`, `$cons (n := e) env` an `EnvCons n e env`, and
`$nil i` a `EnvNil wk` where `wk` weakens by `i`.

We also print the expression with all the suspensions removed.

Node that this approach can be paired with other performance
improvements, such as graph reduction (sharing common
subexpressions and evaluating them all at once), or more
eager evaluation of function arguments.

Thanks for listening, comments [on reddit](https://www.reddit.com/r/haskell/comments/6c578y/a_well_typed_suspension_calculus/).

Appendix
---

<h3>Pretty printing</h3>

> evalToSyntax :: Eval a -> Syntax a
> evalToSyntax = \case
>   EvalLam n body -> Lam n body
>   EvalNeutral v args -> foldl' (\e -> App (Syntax e)) (Var v) args
>
> removeAllSusps :: Exp a -> Exp a
> removeAllSusps e = Syntax $ case removeSusp e of
>   Var v -> Var v
>   Lam n body -> Lam n (removeAllSusps body)
>   Let n e1 e2 -> Let n (removeAllSusps e1) (removeAllSusps e2)
>   App fun arg -> App (removeAllSusps fun) (removeAllSusps arg)
>
> data PrettyEnv a = PrettyEnv
>   { peCounters :: HashMap Text Int
>   , pePrettyNames :: PrettyNames a
>   }
> data PrettyNames a where
>   PrettyNamesNil :: PrettyNames Text
>   PrettyNamesCons :: Text -> PrettyNames a -> PrettyNames (Var a)
>
> newPrettyEnv :: PrettyEnv Text
> newPrettyEnv = PrettyEnv mempty PrettyNamesNil
>
> weakenPrettyEnv :: PrettyEnv a -> Text -> (Text, PrettyEnv (Var a))
> weakenPrettyEnv vn n = let
>   counter = case HMS.lookup n (peCounters vn) of
>     Nothing -> 0
>     Just c -> c
>   counters = HMS.insert n (counter+1) (peCounters vn)
>   n' = if counter == 0 then n else n <> "_" <> T.pack (show counter)
>   in
>     ( n'
>     , vn
>         { peCounters = counters
>         , pePrettyNames = PrettyNamesCons n' (pePrettyNames vn)
>         }
>     )
>
> strengthenPrettyEnv :: PrettyEnv (Var a) -> PrettyEnv a
> strengthenPrettyEnv vn = vn
>   { pePrettyNames = case pePrettyNames vn of
>       PrettyNamesCons _ ns -> ns
>   }
>
> peLookup :: PrettyEnv a -> a -> Text
> peLookup vn = go (pePrettyNames vn)
>     where
>       go :: PrettyNames a -> a -> Text
>       go ns0 v = case ns0 of
>         PrettyNamesNil -> v
>         PrettyNamesCons txt ns -> case v of
>           B -> txt
>           F v' -> go ns v'
>
> data Position
>   = PosNormal
>   | PosArg
>
> posParens :: Position -> PP.Doc -> PP.Doc
> posParens = \case
>   PosNormal -> id
>   PosArg -> PP.parens
>
> hang :: PP.Doc -> PP.Doc
> hang = PP.group . PP.nest 2
>
> prettyExp :: Position -> PrettyEnv a -> Exp a -> PP.Doc
> prettyExp pos vn = \case
>   Syntax e -> prettySyntax pos vn e
>   Susp env e -> prettySusp pos vn env (Syntax e)
>
> prettySyntax :: Position -> PrettyEnv a -> Syntax a -> PP.Doc
> prettySyntax pos vn = \case
>   Var v -> PP.text (T.unpack (peLookup vn v))
>   e@App{} -> posParens pos (hang (prettyApp (Syntax e)))
>   e@Lam{} -> posParens pos (hang ("\\" PP.<> prettyLam vn (Syntax e)))
>   Let n e1 e2 -> posParens pos $ let
>     (n', vn') = weakenPrettyEnv vn n
>     in
>       PP.text (T.unpack n') PP.<+> "=" PP.<+>
>       prettyExp PosNormal vn e1 PP.<> ";" PP.<$>
>       prettyExp PosNormal vn' e2
>   where
>     prettyApp = \case
>       Syntax (App fun arg) -> prettyApp fun PP.<$> prettyExp PosArg vn arg
>       e -> prettyExp PosArg vn e
>
>     prettyLam :: PrettyEnv a -> Exp a -> PP.Doc
>     prettyLam vn' = \case
>       Syntax (Lam n body) -> let
>         (n', vn'') = weakenPrettyEnv vn' n
>         in PP.text (T.unpack n') PP.<+> prettyLam vn'' body
>       e -> "->" PP.<$> prettyExp PosNormal vn' e
>
> prettySusp :: Position -> PrettyEnv b -> Env a b -> Exp a -> PP.Doc
> prettySusp pos vn env e = let
>   (envDoc, vn') = prettyEnv PosArg vn env
>   in posParens pos (hang (PP.vsep ["$susp", envDoc, prettyExp PosArg vn' e]))
>
> prettyEnv :: Position -> PrettyEnv b -> Env a b -> (PP.Doc, PrettyEnv a)
> prettyEnv pos vn = \case
>   EnvComp env1 env2 -> let
>     (env2Doc, vn') = prettyEnv PosArg vn env2
>     (env1Doc, vn'') = prettyEnv PosArg vn' env1
>     in (posParens pos (hang (PP.vsep ["$comp", env1Doc, env2Doc])), vn'')
>   EnvCanonical (EnvNil wk) -> let
>     (wkDoc, vn') = prettyWeaken vn wk
>     in (posParens pos ("$nil" PP.<+> wkDoc), vn')
>   EnvCanonical (EnvCons n0 e env') -> let
>     (env'Doc, vn') = prettyEnv PosArg vn env'
>     (n, vn'') = weakenPrettyEnv vn' n0
>     in
>       ( posParens pos $ hang $ PP.vsep
>           [ "$cons"
>           , PP.parens (hang (PP.text (T.unpack n) PP.<+> ":=" PP.<$> prettyExp PosNormal vn e))
>           , env'Doc
>           ]
>       , vn''
>       )
>
> prettyWeaken :: PrettyEnv b -> Weaken a b -> (PP.Doc, PrettyEnv a)
> prettyWeaken vn0 wk0 = let
>   (wkNum, vn) = go vn0 wk0
>   in (PP.integer wkNum, vn)
>   where
>     go :: PrettyEnv b -> Weaken a b -> (Integer, PrettyEnv a)
>     go vn = \case
>       WeakenZero -> (0, vn)
>       WeakenSucc wk -> let
>         (c, vn') = go (strengthenPrettyEnv vn) wk
>         in (c+1, vn')

<h3>Parsing</h3>

> type ParseMonad = Tri.Parser
>
> data ParseEnv a where
>   ParseEnvNil :: ParseEnv Text
>   ParseEnvCons :: Text -> ParseEnv a -> ParseEnv (Var a)
>
> parseEnvLookup :: ParseEnv a -> Text -> a
> parseEnvLookup env0 txt = case env0 of
>   ParseEnvNil -> txt
>   ParseEnvCons txt' env -> if txt == txt'
>     then B
>     else F (parseEnvLookup env txt)
>
> parseExp :: Position -> ParseEnv a -> ParseMonad (Exp a)
> parseExp pos env = Syntax <$> parseSyntax pos env
>
> parseSyntax :: Position -> ParseEnv a -> ParseMonad (Syntax a)
> parseSyntax pos env = (case pos of
>   PosNormal -> asum
>     [ parseLam env
>     , parseLet env
>     , do
>         head <- parseSyntax PosArg env
>         args <- many (parseSyntax PosArg env)
>         return (foldl' (\fun arg -> App (Syntax fun) (Syntax arg)) head args)
>     ]
>   PosArg -> asum
>     [ Var <$> parseVar env
>     , Parse.parens (parseSyntax PosNormal env)
>     ]) Parse.<?> "expression"
>
> parseVar :: ParseEnv a -> ParseMonad a
> parseVar env = do
>   n <- Parse.ident parseIdentStyle
>   return (parseEnvLookup env n)
>
> parseIdentStyle :: Parse.IdentifierStyle ParseMonad
> parseIdentStyle = Parse.IdentifierStyle
>   { Parse._styleName = "identifier"
>   , Parse._styleStart = Parse.letter <|> Parse.char '_'
>   , Parse._styleLetter = Parse.alphaNum <|> Parse.char '_'
>   , Parse._styleReserved = HS.fromList ["let"]
>   , Parse._styleHighlight = Parse.Identifier
>   , Parse._styleReservedHighlight = Parse.ReservedIdentifier
>   }
>
> parseLam :: ParseEnv a -> ParseMonad (Syntax a)
> parseLam env0 = (do
>   void (Parse.symbolic '\\')
>   n <- Parse.ident parseIdentStyle
>   Lam n <$> go (ParseEnvCons n env0)) Parse.<?> "lambda"
>   where
>     go :: ParseEnv a -> ParseMonad (Exp a)
>     go env = asum
>       [ do
>           void (Parse.symbol "->")
>           parseExp PosNormal env
>       , do
>           n <- Parse.ident parseIdentStyle
>           lam n <$> go (ParseEnvCons n env)
>       ]
>
> parseLet :: ParseEnv a -> ParseMonad (Syntax a)
> parseLet env = (do
>   void (Parse.reserve parseIdentStyle "let")
>   n <- Parse.ident parseIdentStyle
>   void (Parse.symbolic '=')
>   e1 <- parseExp PosNormal env
>   void (Parse.symbolic ';')
>   e2 <- parseExp PosNormal (ParseEnvCons n env)
>   return (Let n e1 e2)) Parse.<?> "let"

<h3>REPL</h3>

> repl :: IO ()
> repl = Haskeline.runInputT
>   Haskeline.defaultSettings
>     { Haskeline.historyFile = Just ".lambda" }
>   loop
>   where
>     render x = PP.displayS (PP.renderSmart 0.6 80 x) ""
>     outputDoc = Haskeline.outputStrLn . render
>
>     parseInput = parseExp PosNormal ParseEnvNil <* Tri.eof
>
>     runParser fp (line :: Int) (col :: Int) s p = let
>       delta = Tri.Directed (T.encodeUtf8 (T.pack fp)) (fromIntegral line - 1) (fromIntegral col - 1) 0 0
>       in case Tri.parseByteString p delta s of
>         Tri.Success a -> Right a
>         Tri.Failure err -> Left (Tri._errDoc err)
>
>     loop :: Haskeline.InputT IO ()
>     loop = do
>       mbInput <- Haskeline.getInputLine ">>> "
>       let processAndLoop s = do
>             let mbX = runParser "repl" 0 0 (T.encodeUtf8 (T.pack s)) parseInput
>             case mbX of
>               Left err -> do
>                 outputDoc ("Error while parsing" PP.<$> err)
>                 loop
>               Right x -> do
>                 outputDoc ("Parsed expression:" PP.<$> PP.indent 2 (prettyExp PosNormal newPrettyEnv x))
>                 let whnf = Syntax (evalToSyntax (eval (removeSusp x)))
>                 outputDoc $
>                   "Evaluated expression:" PP.<$>
>                   PP.indent 2 (prettyExp PosNormal newPrettyEnv whnf)
>                 outputDoc $
>                   "Evaluated expression (no suspensions):" PP.<$>
>                   PP.indent 2 (prettyExp PosNormal newPrettyEnv (removeAllSusps whnf))
>                 loop
>       case mbInput of
>         Nothing -> return ()
>         Just (':' : cmd) -> case cmd of
>             "q" -> return ()
>             "{" -> do
>               let collect :: [String] -> Haskeline.InputT IO ()
>                   collect chunks = do
>                     mbInput' <- Haskeline.getInputLine "  | "
>                     case mbInput' of
>                       Nothing -> return ()
>                       Just "}:" -> processAndLoop (concat (intersperse "\n" (reverse chunks)))
>                       Just chunk -> collect (chunk : chunks)
>               collect []
>             'l' : ' ' : file -> processAndLoop =<< liftIO (readFile file)
>             _ -> do
>               outputDoc ("Unrecognized command" PP.<+> PP.text cmd)
>               loop
>         Just input -> processAndLoop input
