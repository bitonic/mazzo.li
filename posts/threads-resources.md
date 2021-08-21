---
title: "Threading responsibly"
date: 2017-06-25
---

Alternative clickbait title: `forkIO` considered harmful.

This is going to be a brief post elaborating on various comments made at
ZuriHac that surprised [some](https://github.com/ndmitchell)
[people](https://github.com/michalt). I promised some writing, so here
it is!

Managing resources
------------------

Resource management is an important subject in many languages and
Haskell is no exception. With "resource management" I'm referring to
handling all those objects that require some form of release.

Examples include:

         Resource Acquire      Release
  --------------- ------------ -----------
           Memory `malloc`     `free`
          Handles `open`       `close`
    Locks/`MVars` `takeMVar`   `putMVar`

and surely many others I can't think of now.

There are many ways to perform resource management. Modern C++ and Rust
have quite strong built-in support for timely resource management via
[RAII](https://en.wikipedia.org/wiki/Resource_acquisition_is_initialization).
In Haskell we use a combination of garbage collection and finalizers
when timely release is not crucial and
[`bracket`](https://www.stackage.org/haddock/lts-8.20/safe-exceptions-0.1.5.0/Control-Exception-Safe.html#v:bracket)
when it is.

Threads are resources
---------------------

In this post I want to argue that threads are also a resource, in the
sense that they require management after we've created them. A stray
thread will consume memory, CPU cycles, and really whatever resource it
might need to execute. Moreover, we most likely want to know if the
thread failed. Thus, creating a thread should *always* be paired with
some code that checks on it and tears it down if necessary.

However, this is not standard practice in most Haskell codebases I see,
even written by experienced Haskellers, and I'd like that to change. One
common objection when I bring this up is that people often use `forkIO`
with threads that should run for the entire execution of the program,
and thus we don't really need to manage them. However this is bad
practice and leads to code which is not composable: if I want to safely
reuse code written in this way I can't, since it leaks threads left and
right. Note that the same objection would apply to not always releasing
handles or other resources but it's widely accepted that it's best
practice to *always* release them properly, no matter their expected
lifetime.

Safely "releasing" threads
--------------------------

Now that we hopefully agree that not managing threads is a bad idea, how
should threads be managed? The "acquire" operation for threads, in
Haskell, is

    forkIO :: IO a -> IO ThreadId

but what shall "release" be? A first candidate might be

    cancelThread :: ThreadId -> IO ()
    cancelThread tid = throwTo tid ThreadKilled

This is a good attempt, and in fact to the best of my knowledge was all
we had until [recently](https://github.com/simonmar/async/issues/41).

Sadly `cancelThread` is problematic since it does not guarantee that the
thread has been shut down when it returns -- only that the exception has
been delivered. However the recipient of the exception could still be
running after the exception has been delivered -- most notably it could
be running finalizers in `bracket`! This subtlety has caused me and
Philip Kant countless hours of entertainment in a real system.

`forkIO` bad, `async` good
--------------------------

The solution is to *not* use `forkIO` and to *always* use
[`async`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html),
or
[`lifted-async`](https://www.stackage.org/haddock/lts-8.20/lifted-async-0.9.1.1/Control-Concurrent-Async-Lifted-Safe.html).
What `async` does is set up infrastructure to know when a thread has
terminated, thus allowing to write a
[`cancel`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:cancel)
that waits for the thread to be done before returning. `async` also
gives us an easy and safe way to get the result of the forked
computation and to propagate exceptions upstream. In short, `async`
should be your default way to perform concurrency, it's really an
amazing tool and yet another case of Haskell exceptionalism.

Then, we can update our table, with `cancel`, which guarantees that the
thread is dead by the time it returns:[^async-version]

         Resource Acquire                                                                                                    Release
  --------------- ---------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------
           Memory `malloc`                                                                                                   `free`
          Handles `open`                                                                                                     `close`
    Locks/`MVars` `takeMVar`                                                                                                 `putMVar`
          Threads [`async`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:async)   [`cancel`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:cancel)

`async` also provides a plethora of useful combinators that should be in
the standard toolkit of every Haskell programmers, such as
[`race`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:race),
[`concurrently`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:concurrently),
the
[`Concurrently Alternative Applicative`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#t:Concurrently),
and many others.

[^async-version]: Note that you'll need a relatively recent version of `async` to get
  the correct behavior, specifically one published after we fixed some
  problematic behavior in async, see
  <https://github.com/simonmar/async/pull/42> and
  <https://github.com/simonmar/async/pull/44>.

Some `async` patterns
---------------------

Finally, I want to show some patterns that are common and that will
hopefully give a bit of an idea of how `async` can be used to easily
implement complex tasks. If you're already convinced and have something
better to do you can stop reading, but if you want to see some more
complex examples using `async` keep reading.

Note that all the examples below are written using `IO`, but they would
work equally well with
[`MonadBaseUnlift m`](https://www.stackage.org/haddock/lts-8.20/monad-unlift-0.2.0/Control-Monad-Trans-Unlift.html#t:MonadBaseUnlift).

### Workers

When writing backend services one of the primitives I use most is
`withWorker` or some variation of it:

    -- | Runs a worker action alongside the provided continuation.
    -- The worker will be automatically torn down when the continuation
    -- terminates.
    withWorker ::
         IO Void -- ^ Worker to run
      -> IO a
      -> IO a
    withWorker worker cont = either absurd id <$> race worker cont

Where `race` is a wonderfully useful function from `async`, which runs
two threads and returns as soon as one of them finishes, `cancel`ing the
other:

    race :: IO a -> IO b -> IO (Either a b)

`withWorker` takes the "worker" action, which is an endless loop as
indicated by the type, and a continuation, and runs them side-by-side.

As soon as the continuation terminates the worker will be automatically
tore down as well by `race`, and the result of the continuation returned
upstream.

### `pooledMapConcurrently`

A useful combinator from `async` is
[`mapConcurrently`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:mapConcurrently),
which is much like
[`traverse`](https://www.stackage.org/haddock/lts-8.20/base-4.9.1.0/Prelude.html#v:traverse)
but executing each action in parallel:

    mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)

While this is often good enough if the provided action is IO-bound, it's
harmful if the action is CPU-bound: if we have a list of 1000 elements
and need to perform some lengthy number crunching on each of them, we
most likely want do not want to execute 1000 number crunching threads in
parallel, but rather 1 number crunching thread per core.

To solve this other common use case me and Patrick Chilton wrote
`pooledMapConcurrently`:

    -- | Like 'mapConcurrently' from async, but instead of
    -- one thread per element, only use one thread per capability.
    pooledMapConcurrently :: forall t a b.
         (Traversable t)
      => (a -> IO b) -> t a -> IO (t b)
    pooledMapConcurrently f xs = do
      numProcs <- getNumCapabilities
      -- prepare one IORef per result...
      jobs :: t (a, IORef b) <-
        for xs (\x -> (x, ) <$> newIORef (error "pooledMapConcurrently: empty IORef"))
      -- ...put all the inputs in a queue..
      jobsVar :: MVar [(a, IORef b)] <- newMVar (toList jobs)
      -- ...run `numProcs` threads in parallel, each
      -- of them consuming the queue and filling in
      -- the respective IORefs.
      forConcurrently_ $ [1..numProcs] $ \_ -> do
        let loop = do
              mbJob :: Maybe (a, IORef b) <- modifyMVar jobsVar $ \case
                [] -> return ([], Nothing)
                var : vars -> return (vars, Just var)
              case mbJob of
                Nothing -> return ()
                Just (x, outRef) -> do
                  y <- f x
                  writeIORef outRef y
                  loop
        loop
      -- read all the IORefs once we're done.
      for jobs (\(_, outputRef) -> readIORef outputRef)

Where
[`forConcurrently_`](https://www.stackage.org/haddock/lts-8.20/async-2.1.1.1/Control-Concurrent-Async.html#v:forConcurrently_)
is a slightly rearranged `mapConcurrently`:

    forConcurrently_ :: Foldable f => f a -> (a -> IO b) -> IO ()

What I like about this function is how short it is, considering how much
it's doing. We are traversing generically using `Traversable`,
implementing a queue using `MVar`, and coordinating threads threads
using `async`. The result will be fast, automatically handling
differences in the execution times of different invocations of the
provided function. It will also be exception safe: if any of the
function invocation fails, or if an asynchronous exception is received,
the whole thread hierarchy will be gracefuly shut down. Writing such a
function in other languages is almost impossible, but in Haskell knowing
the right tools it takes half an hour.

*Edit*: `quchen` on reddit
[suggested](https://www.reddit.com/r/haskell/comments/6jedlb/threading_responsibly_forkio_considered_harmful/djdz1e4/)
using semaphores -- and using that the function is even shorter:

        pooledMapConcurrently ::
             (Traversable t)
          => (a -> IO b) -> t a -> IO (t b)
        pooledMapConcurrently f xs = do
          numProcs <- getNumCapabilities
          sem <- newQSem numProcs
          forConcurrently xs $ \x -> do
            bracket
              (waitQSem sem)
              (\() -> signalQSem sem)
              (\() -> f x)

One advantage the original version executes left to right -- elements
appearing later will always *start* processing after earlier elements
have already started, which can be more amenable to debugging (for
example if you're looking at the output in a terminal, or reproducing a
crash). It also avoids high contention (one thread per element) on the
synchronization mechanism. However the semaphore version is certainly
more pleasant to read and probably preferrable.

### Pooled map and streaming

Finally, as a last example I have a variation of the concurrent pooled
map --
[`conduitPooledMapMBuffered`](https://gist.github.com/nh2/321567bb68e9efa6e299eb6b2410a1fb).
This function, due to Niklas Hambüchen, lets us easily write `Conduit`s
that compute their outputs in parallel, using one thread per capability,
while preserving the same output order that a non-parallel conduit would
produce. This is extremely useful in situations where we want to have
some CPU bound worker that is capable of streaming inputs and outputs
while efficiently utilizing the available cores.

It's an interesting example because there are many moving pieces --
concurrency, streaming, resource management through `ResourceT` -- and
everything is tied together nicely to guarantee good behavior. If you
want an example of complex logic written with `async`, check it out!
