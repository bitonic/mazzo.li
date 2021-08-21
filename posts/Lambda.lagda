**Update**: I gave a talk at NY Haskell following this blogpost,
  although I didn't manage to talk about everything:

<div class="yt"><iframe src="//player.vimeo.com/video/77168227" width="600" height="337" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe></div>

***

In the [previous Agda example](/posts/AgdaSort.html) we saw how we can
approach the task of verifying a sorting function.  This time, we are
going to write a type checker for the simply typed λ-calculus, plus a
simple optimization on said terms that we will prove correct.  As in the
other post the bulk of the thinking has been done by other people.  The
type checking is a modified version of an example given in [*The View
from the Left*](http://strictlypositive.org/view.ps.gz) by Conor McBride
and James McKinna,[^ulf] while the optimisation is inspired from a talk
given by Adam Chlipala at POPL 2013---his Coq book [*Certified
Programming with Dependent Types*](http://adam.chlipala.net/cpdt/)
contains many similar examples.

[^ulf]: See also the Agda version by Ulf Norell in his [*Dependently
    Typed Programming in
    Agda*](http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf)
    tutorial, after which some of the data types for this tutorial are
    patterned after.

Let's get started.

## Useful imports

\begin{code}
module Lambda where

open import Data.Nat using (ℕ; zero; suc; _+_; _≤?_; _≥_)
open import Data.Fin using (Fin; zero; suc; toℕ)
open import Data.List using (List; []; _∷_; length)
open import Data.Vec using (Vec; []; _∷_; lookup)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong₂)
open import Relation.Nullary using (Dec; yes; no)
open import Function using (_∘_; _$_)
open import Data.Product
\end{code}

After the module declaration, we include some useful modules from the [Agda
standard
library](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Libraries.StandardLibrary):

* [`Data.Nat`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Data.Nat.html)
  defines natural numbers.

* [`Data.Fin`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Data.Fin.html)
  defines an inductive families to represent the type of all numbers
  less than a given number.  For instance `Fin 3` will be inhabited by
  `0`, `1`, and `2`.  Another interpretation is that `Fin n` is the type
  inhabited by `n` elements.

* [`Data.List`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Data.List.html),
  predictably, defines finite lists.

* [`Data.Vec`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Data.Vec.html)
  defines lists indexed by their length.  This allows, for example, for
  safe indexing of elements.

* [`Relation.Binary.PropositionalEquality`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Relation.Binary.PropositionalEquality.html)
  defines propositional equality as
  [presented](/posts/AgdaSort.html#propositional-equality) in the
  previous post.  `cong₂` is the two-argument version of `cong`.

* [`Relation.Nullary`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Relation.Nullary.html#1)
  defines a type for decidable relations, `Dec`:

        data Dec (P : Set) : Set where
          yes : ( p :   P) → Dec P
          no  : (¬p : ¬ P) → Dec P

* [`Function`](http://www.cse.chalmers.se/~nad/listings/lib-0.7/Function.html)
  exports some common utilities regarding functions that should be
  familiar to the Haskell programmer, such as function composition
  (`_∘_`) and explicit application (`_$_`).

## Simple types and raw terms

The language we are going to define is a simple [simply typed
λ-calculus](http://en.wikipedia.org/wiki/Simply_typed_lambda_calculus).
The types for our language will be of two kinds: natural numbers and
functions (arrow types):

\begin{code}
infixr 30 _⇒_
data Type : Set where
  nat : Type
  _⇒_ : Type → Type → Type
\end{code}

We will use [de Bruijn
indices](http://en.wikipedia.org/wiki/De_Bruijn_index) to represent
bound variables, where `n` represents the variable bound by the `n`-th
`λ` going towards the top of the term.  For example, using an Haskell
notation for `λ`s, we will have that

Named                              de Bruijn
-------------------------------    -------------------------------
`λx → x`                           `λ 1`
`λx → λy → x`                      `λ λ 2`
`λx → λy → λz → x z (y z)`         `λ λ λ 3 1 (2 1)`

and so on.  de Bruijn notation is a terrible device for humans to use
directly, but it is often more convenient when dealing with terms
mechanically.  When parsing a language it is easy to go from names to
nameless, so that we can have the best of both worlds.

Thus we will have:

\begin{code}
infixl 80 _·_
data Syntax : Set where
  -- A variable, the ℕ being a de Bruijn index.
  var : ℕ → Syntax
  -- A number literal.
  lit : ℕ → Syntax
  -- Addition.
  _⊕_ : Syntax → Syntax → Syntax
  -- Function application.
  _·_ : Syntax → Syntax → Syntax
  -- Lambda abstraction, the type indicates the type of the argument: no
  -- type inference here.
  lam : Type → Syntax → Syntax
\end{code}


## Typed terms, and type checking

Our λ-calculus has a very unsurprising type system.  Typing judgements
will be done in a context, holding the types of the abstracted
variables.  Calling the context `Γ`, we can give typing judgements for
our syntax:

<div class="center-image">![The type system for our language](/assets/images/lambda-typ.png)</div>

For those unfamiliar with typing rules like the ones above, they can be
read as "given what's above the bar, what's below holds".  In our case
we have judgements of the form `Γ ⊢ t : τ`, which can be read as "In
context `Γ`, term `t` has type `τ`".

<code class="sc">Lit</code> says that number literals have type `nat`.
<code class="sc">Add</code> says that given two terms of type `nat`, we
can add them together yielding another term of type `nat`.  <code
class="sc">Var</code> says that given a variable we can get its type by
looking it up in the context---we haven't defined what a context is but
we will soon. <code class="sc">App</code> says that given a function of
type `τ ⇒ σ` we can apply it to a term of type `τ` to get a term of type
`σ`.  Finally, if we have a term `t : τ` in a context with `σ` "at its
head", we can form a function of type `σ ⇒ τ`, typed in the context with
`σ` removed (<code class="sc">Lam</code>).

Now, taking advantage of Agda's facilities, we can define a `Term` data
type to represent terms together with their type---a `Term` will
essentially include a derivation that its type is valid.

We will represent the context as a `Vec` containing `Type`s.  The fact
that `Vec` carry their length at the type is useful, since it will
indicate how many variables are in scope, and thus what the largest de
Bruijn index is.

\begin{code}
Ctx : ℕ → Set
Ctx = Vec Type
\end{code}

A type checked `Term` will be parametrised over a context `Γ` of some
length `n`.  Moreover, it will be indexed by its type.  The type is an
index because different constructors will be typed differently, just
like in the typing derivations.

\begin{code}
data Term {n} (Γ : Ctx n) : Type → Set where
  var : ∀ {τ} (v : Fin n) → τ ≡ lookup v Γ → Term Γ τ
  lit : ℕ → Term Γ nat
  _⊕_ : Term Γ nat → Term Γ nat → Term Γ nat
  _·_ : ∀ {σ τ} → Term Γ (σ ⇒ τ) → Term Γ σ → Term Γ τ
  lam : ∀ σ {τ} → Term (σ ∷ Γ) τ → Term Γ (σ ⇒ τ)
\end{code}

Note that in the `var` case we could have written

    var : (v : Fin n) → Term Γ (lookup v Γ)

However this would have caused problems further down the line,^[See the
footnote ad the end of [the section about constant
folding](#constant-folding).] and thus we quantify over a type `τ` and
use an explicit equality.

Also in `var`, we use `Fin` to make sure that the index is indeed "in
scope".  If the context is of length `n`, an index of type `Fin n` makes
sure that we can look up the type of the variable, since `Fin n`
represents the type of all naturals less than `n`:

    lookup : ∀ {n} {A : Set} → Fin n → Vec A n → A

Closed terms will be those living in empty contexts:

\begin{code}
Closed : Type → Set
Closed = Term []
\end{code}

For example, we can write a function doubling numbers in `Term`.  The
type will predictably be `nat ⇒ nat`:

\begin{code}
double : Closed (nat ⇒ nat)
double = lam nat (var zero refl ⊕ var zero refl)
\end{code}

We can also write a function that forgets the type information and gets
us `Syntax` from `Term`:

\begin{code}
erase : ∀ {n} {Γ : Ctx n} {τ} → Term Γ τ → Syntax
erase (var v _) = var (toℕ v)
erase (lit n)   = lit n
erase (t ⊕ u)   = erase t ⊕ erase u
erase (t · u)   = erase t · erase u
erase (lam σ t) = lam σ (erase t)
\end{code}

### Type equality

Before writing our type checking function, we need some additional
tools.  We will begin by defining a decision procedure for equality
between types.  First two lemmas telling us that if two arrow types are
equal, both sides of the arrows are equal too:

\begin{code}
≡⇒₁ : ∀ {σ σ′ τ τ′} → σ ⇒ τ ≡ σ′ ⇒ τ′ → σ ≡ σ′
≡⇒₁ refl = refl
≡⇒₂ : ∀ {σ σ′ τ τ′} → σ ⇒ τ ≡ σ′ ⇒ τ′ → τ ≡ τ′
≡⇒₂ refl = refl
\end{code}

Then the actual function.  `Dec` is inhabited by either evidence that
the relation holds, using the constructor `yes`; or that it doesn't,
using `no`:

\begin{code}
_≟_ : (τ σ : Type) → Dec (τ ≡ σ)
nat   ≟ nat   = yes refl
nat   ≟ _ ⇒ _ = no λ()
_ ⇒ _ ≟ nat   = no λ()
σ ⇒ τ ≟ σ′ ⇒ τ′ with σ ≟ σ′ | τ ≟ τ′
σ ⇒ τ ≟ .σ ⇒ .τ | yes refl | yes refl = yes refl
σ ⇒ τ ≟ σ′ ⇒ τ′ | no  σ≢σ′ | _        = no (σ≢σ′ ∘ ≡⇒₁)
σ ⇒ τ ≟ σ′ ⇒ τ′ | _        | no τ≢τ′  = no (τ≢τ′ ∘ ≡⇒₂)
\end{code}

Note that in the cases where different constructors are present we use
`λ()`, which is a shorthand for a function with an empty
pattern---meaning that its input cannot be inhabited.  For example when
faced with `nat ≟ σ ⇒ τ` to prove the inequality we need to show that `¬
(nat ≡ σ ⇒ τ)`, which is a shorthand for `(nat ≡ σ ⇒ τ) → ⊥`---"We can
derive falsity given that `nat` is equal to an arrow type."  But given
how `≡` is defined Agda knows that its indices must be the same, and
thus that `nat ≡ σ ⇒ τ` cannot be inhabited, allowing us to use an empty
pattern.

Moreover, note how we use dependent pattern matching when recursing down
the structure of the types: when pattern matching on an equality proof
in a `yes` using `refl` the two relevant types are constrained to be the
same using dotted patterns, which allows us to use `refl` again to prove
equality of the type as a whole.

### From `Fin` to `ℕ`

The second tool that we will use is a "view" to tell us whether a given
`ℕ` "fits" in a `Fin n`.

\begin{code}
data Fromℕ (n : ℕ) : ℕ → Set where
  yes : (m : Fin n) → Fromℕ n (toℕ m)
  no  : (m : ℕ)     → Fromℕ n (n + m)
\end{code}

`Fromℕ` is parametrised over the "upper bound" `n`, and indexed over the
`ℕ` that we are bringing into `Fin n`.  We can then write a function
that tries to convert a number `m` to a `Fin n`:

\begin{code}
fromℕ : ∀ n m → Fromℕ n m
fromℕ zero    m    = no m
fromℕ (suc n) zero = yes zero
fromℕ (suc n) (suc m) with fromℕ n m
fromℕ (suc n) (suc .(toℕ m)) | yes m = yes (suc m)
fromℕ (suc n) (suc .(n + m)) | no  m = no m
\end{code}

### Type checking

Finally we can proceed to the actual type checking procedure.  As with
`Fromℕ`, we define a data type indexed over what will be the input of
our function---some piece of untyped `Syntax`:

\begin{code}
data Check {n} (Γ : Ctx n) : Syntax → Set where
  yes : (τ : Type) (t : Term Γ τ) → Check Γ (erase t)
  no  : {e : Syntax} → Check Γ e
\end{code}

`Check` is parametrised over contexts.  If type checking succeeds, we
return a type `τ` and a term `t : τ`, the erasure of `t` being the
`Syntax` we were type checking---the `yes` constructor.  We can fail at
any time using the constructor `no`.[^badno]

[^badno]: Notice that the `no` constructor does not include evidence
    that the term is ill-typed.  This could be fixed by having a data
    type representing ill-typed terms.

    Precise types are usually a good idea, not only because they
    describe the program better to humans, but also because machines can
    do more with it.  For example, when using
    [`Agsy`](http://wiki.portal.chalmers.se/agda/agda.php?n=Main.Auto)
    with `Check`, `bad` is always going to be a valid candidate, even if
    the term we are checking is well typed.

    Here we have chosen a simpler way for brevity, especially since
    the "error" data type would be fairly boring.

Our type checking procedure will work with a context and some `Syntax`:

\begin{code}
check : ∀ {n} (Γ : Ctx n) (t : Syntax) → Check Γ t
\end{code}

With `var`, we make sure that the index is in scope using `fromℕ`.  If
it is, the type is determined by what's in the context at the right
index:

\begin{code}
check {n} Γ (var v) with fromℕ n v
check {n} Γ (var .(toℕ v)) | yes v = yes (lookup v Γ) (var v refl)
check {n} Γ (var .(n + m)) | no  m = no
\end{code}

Literals are of type `nat`:

\begin{code}
check Γ (lit n) = yes nat (lit n)
\end{code}

For additions, we recursively type check the two sides.  If they are
both `nat`, the resulting term is `nat` too.  Otherwise, type checking
fails:

\begin{code}
check Γ (t ⊕ u) with check Γ t | check Γ u
check Γ (.(erase t) ⊕ .(erase u)) | yes nat t | yes nat u = yes nat (t ⊕ u)
check Γ (_ ⊕ _)                   | _         | _         = no
\end{code}

Note that when checking against the results of the recursive calls, we
use dotted matches to show the relationship between the input `Syntax`
terms and the type checked terms: the former is the erasure of the
latter.  This way we can invoke `yes` so that the result will be well
typed.

The interactive Agda mode is tremendously useful in these situations.
If we are in the situation

    check Γ (t ⊕ u) with check Γ t | check Γ u
    check Γ (t ⊕ u) | p1 | p2 = {!!}

We can pattern match by placing `p1` in the hole and then invoking `C-c
C-c`, which will result in

    check Γ (.(erase t) ⊕ u) | yes τ t | p2 = {!!}

Agda automatically places the dotted patterns.  Then after doing the
same for `p2` and matching `nat`, we are left with

    check Γ (.(erase t) ⊕ .(erase u)) | yes nat t | yes nat u = {!!}

The goal for that hole, as Agda reminds us, is

    Check Γ (erase t ⊕ erase u)

We can type `yes` and then `C-r` to refine the hole, and then `C-a` in
each hole to tell Agda to try to search for an appropriate term, which
in this case succeeds.  In general with strong enough types we only
write an outline of the function and then a lot of details can be
filled in automatically.

For what concerns application, we check that the term we are applying
something to has a function type, and that the argument's type is equal
to the domain of said function:

\begin{code}
check Γ (t · u) with check Γ t | check Γ u
check Γ (.(erase t) · .(erase u)) | yes (σ ⇒ τ) t | yes σ′ u with σ ≟ σ′
check Γ (.(erase t) · .(erase u)) | yes (σ ⇒ τ) t | yes .σ u | yes refl = yes τ (t · u)
check Γ (.(erase t) · .(erase u)) | yes (σ ⇒ τ) t | yes σ′ u | no  _    = no
check Γ (t · u)                   | _             | _       = no
\end{code}

Finally, with λs, we check that the body of the function is well typed
in the context plus the type of the argument.  If that's the case, the
resulting type is an arrow type:

\begin{code}
check Γ (lam σ t) with check (σ ∷ Γ) t
check Γ (lam σ .(erase t)) | yes τ t = yes (σ ⇒ τ) (lam σ t)
check Γ (lam σ t)          | no      = no
\end{code}

Type checking the syntax for `double` will give good results:

    > C-c C-n check [] (lam nat (var 0 ⊕ var 0))
    yes (nat ⇒ nat) (lam nat (var zero refl ⊕ var zero refl))

While ill-typed examples will fail:

    > C-c C-n check [] (lam nat (var 0 · var 0))
    no

    > C-c C-n check [] (lam (nat ⇒ nat) (var 0 ⊕ var 0))
    no

## Embedding terms

Now that we have a type checker, we can embed typed terms into Agda.
This technique is a common and cheap way to give semantics to
programming languages that we are modelling inside a theorem prover---we
borrow the host language semantics.

In our case, we can easily define a function taking `Type` into Agda's
`Set`:

\begin{code}
⟦_⟧ : Type → Set
⟦ nat   ⟧ = ℕ
⟦ σ ⇒ τ ⟧ = ⟦ σ ⟧ → ⟦ τ ⟧
\end{code}

Then, to transport an arbitrary `Term Γ τ` to `⟦ τ ⟧`, we need an
environment storing values for the bound variables:

\begin{code}
infixr 5 _∷_
data Env : ∀ {n} → Ctx n → Set where
  []  : Env []
  _∷_ : ∀ {n} {Γ : Ctx n} {τ} → ⟦ τ ⟧ → Env Γ → Env (τ ∷ Γ)
\end{code}

`Env Γ` stores values corresponding to the types stored in `Γ`.  We can
extend an existing `Env` by consing a value of the right type to it, and
extending the context.

We can also lookup values into the `Env` just like we can index into
`Vec`s:

\begin{code}
lookupEnv : ∀ {n} {Γ : Ctx n} (m : Fin n) → Env Γ → ⟦ lookup m Γ ⟧
lookupEnv zero    (x ∷ _)   = x
lookupEnv (suc n) (_ ∷ env) = lookupEnv n env
\end{code}

With these tools, we can define a function evaluating a `Term` into an
Agda value of the right type:

\begin{code}
_[_] : ∀ {n} {Γ : Ctx n} {τ} → Env Γ → Term Γ τ → ⟦ τ ⟧
env [ var v refl ] = lookupEnv v env
env [ lit n      ] = n
env [ t ⊕ u      ] = env [ t ] + env [ u ]
env [ t · u      ] = (env [ t ]) (env [ u ])
env [ lam _ t    ] = λ x → (x ∷ env) [ t ]
\end{code}

Now we can verify, for example, that `double` does the right thing:

\begin{code}
double′ : ⟦ nat ⇒ nat ⟧
double′ = [] [ double ] -- A closed term, in an empty environment

doubleTest : double′ 3 ≡ 6
doubleTest = refl
\end{code}

## Constant folding

To conclude, we will write a simple optimisation our terms, and prove
that it is sound by showing that it always preserves the semantics of
the original term, using the embedding above.

To represent "optimised" terms, we will use a record parametrised over
the original term and holding the optimised version, plus evidence that
for all environments evaluating the original and optimised term will
yield the same Agda value:

\begin{code}
record Optimised {n} {Γ : Ctx n} {σ} (t : Term Γ σ) : Set where
  constructor opt
  field
    optimised : Term Γ σ
    sound     : ∀ {env} → env [ t ] ≡ env [ optimised ]
\end{code}

However, to succeed, we need to postulate an axiom:

\begin{code}
postulate ext : ∀ {A B : Set} {f g : A → B} → ({x : A} → f x ≡ g x) → f ≡ g
\end{code}

This axiom says that if we have two functions `f` and `g`, and we have
evidence that for all inputs the functions have equal outputs, then the
functions themselves can be considered equal.  This is usually know as
[*functional
extensionality*](http://en.wikipedia.org/wiki/Extensionality), and we
cannot prove it in Agda and theorem provers based on similar
theories.[^intuito] Postulating extensionality is still (hopefully)
consistent with Agda's theory, although obviously we will not be able to
compute with it---definitions pattern matching on equality proofs will
get stuck on invocations of `ext`.

[^intuito]: It has been an ongoing challenge to find a workable theory
    where functional extensionality holds---a recent proposal is
    *[observational
    equality](http://www.cs.nott.ac.uk/~txa/publ/obseqnow.pdf)*.

    With "workable" we mean among other things a theory where type
    checking is decidable, something that we lose if for example we
    "solve" the problem in a naïve way by adding a rule to derive
    definitional equality (the meta-judgement in the type checker for
    term equality) from propositional equality.

    More broadly the theme of equality is a very debated one in type
    theory, with [one related
    development](http://homotopytypetheory.org/) getting a lot of people
    excited lately.

With this axiom and our record `Optimised`, we will write the
optimisation and the proof at the same time.  The optimisation will
consist of reducing constant expressions to number literals, so that for
example

    lit 3 ⊕ lit 7

will be reduced to

    lit 10

So, our function will take a term and produce an optimised version of
it:

\begin{code}
cfold : ∀ {n} {Γ : Ctx n} {τ} (t : Term Γ τ) → Optimised t
\end{code}

Variables and literals stay untouched, so the proof of equality will be
trivial:

\begin{code}
cfold (var v p) = opt (var v p) refl
cfold (lit x)   = opt (lit x)   refl
\end{code}

For applications, we call `cfold` recursively, and then we use `cong₂`
to combine the resulting proofs of soundness---since evaluating
functions means applying them, we use `_$_`:

    cong₂ : {A B C : Set} (f : A → B → C) {x y u v} → x ≡ y → u ≡ v → f x u ≡ f y v
    _$_   : ∀ {A : Set} {B : A → Set} → ((x : A) → B x) → ((x : A) → B x)
    cong₂ _$_ : {A B : Set} {f g : A → B} {x y : A} → f ≡ g → x ≡ y → (f $ x) ≡ (g $ y)

\begin{code}
cfold (t · u) with cfold t | cfold u
... | opt t′ p | opt u′ q = opt (t′ · u′) (cong₂ _$_ p q)
\end{code}

For λs, we need to use `ext` to prove that the optimised function is
equal to the original one:

\begin{code}
cfold (lam σ t) with cfold t
... | opt t′ p = opt (lam σ t′) (ext p)
\end{code}

Finally, with additions, we perform the actual optimisation.  If both
sides after optimisation are number literals we can replace them with
another literal.  Otherwise we leave the addition in place.  We also
combine the proofs of equality of the sides with `cong₂ _+_`, given that
the denotation uses `_+_`.[^pattern]

[^pattern]: If we had `var` to have `lookup v Γ` as an index, we would
  have been unable to pattern match on the optimised term here, since
  Agda would have got stuck trying to decide if there should be a case
  for `var`.

\begin{code}
cfold (t ⊕ u) with cfold t | cfold u
... | opt (lit n) p | opt (lit m) q = opt (lit (n + m)) (cong₂ _+_ p q)
... | opt t′      p | opt u′      q = opt (t′ ⊕ u′) (cong₂ _+_ p q)
\end{code}

And that's it.

You can comment this post [on
reddit](http://www.reddit.com/r/haskell/comments/1k3b8u/agda_by_example_lambdacalculus/).
