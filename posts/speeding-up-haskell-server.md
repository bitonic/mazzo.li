---
title: Speeding up a distributed computation in Haskell 
date: 2017-01-18
---

_This blog post originally appeared on the [FP Complete blog](https://www.fpcomplete.com/blog/2017/01/speeding-up-distributed-computation)._

***

While helping a client ship a medical device we were tasked to
make its response time bearable. This was no easy feat, given that
each request to this device requires running a simulation that takes
hours if ran on a single CPU. This long response time would
make it impossible for doctors to use this device interactively,
which in turn would make the device much less desirable -- think of
a doctor having to wait hours between inputting the patient data
and getting results, as opposed to getting results immediately as
the data is available.

Luckily the simulations in question are embarrassingly parallel, and
thus one obvious path to reduce the response time is to run it
on multiple CPUs.

At the core of this device sits a Haskell program that performs the
simulation. Thus the first step was to exploit Haskell built-in
multi-core parallelism to achieve the parallelization. However the
results were unsatisfactory, since we were unable to scale decently
beyond 7 to 10 CPUs. Thus we created a custom distribution algorithm
where separate Haskell runtimes communicate with TCP sockets, similar
to what happens in Erlang. This also allowed us to scale beyond a
single machine.  We've described this effort in the past, see the
report [Scaling Up a Scientific
Computation](/posts/scaling-up-scientific.html) and
the talk [Parallelizing and distributing scientific software in
Haskell](/posts/haskell-parallelizing.html).

This first effort allowed us to run simulations in a much shorter
time, but it still did not allow us to scale nicely to hundreds of
CPUs. This article describes how we fixed that by bypassing one
of the high level facilities that Haskell provides.

High level languages are all about offering such facilities, to be
able to write correct programs quicker. Haskell offers a great
number of abstractions to help in this regard, such as garbage
collection and laziness, and GHC also is full of tools on top of
the language itself to write an ever greater number of programs in
a more comfortable way.

One of the features that makes GHC stand out is the sophistication
of the runtime it provides. Apart from being an impressive piece
of work even just for implementing Haskell efficiently, it also
offers features that are very useful for the kind of systems
programming that writing a distributed application requires.
Specifically, green threads and the GHC event manager make writing
a fast multi-threaded server much easier than in other languages.
For example the first versions of Warp, Haskell's most popular web
server, outperformed most web servers in just 500 lines of code,
largely thanks to these facilities.[^warp] Warp has since grown in
code size to add new features, but the core is still using the same
facilities and performing well.

[^warp]: You can find more info about the Warp web server and
how the features that GHC provided enabled it on this report:
<http://steve.vinoski.net/pdf/IC-Warp_a_Haskell_Web_Server.pdf>

Since the core of the software that we built is a server coordinating
the work of many slaves, for our first version we reached for these
facilities to write it. The server was reasonably fast and served
us for a while, but we hit a ceiling pretty quickly beyond which
we were unable to scale.

However, a nice thing about GHC Haskell is that it's very easy to
drop down to a lower level programming style when needed. This can
be accomplished through the excellent foreign function interface
to C paired with the low-level utilities in `base`. By doing so we
were able to scale to hundreds of cores and run simulations up to
5 times faster then the best time we achieved with the previous
version.

## The program

As mentioned, the server in question is the master process in a
distributed computing application. The application is essentially
a [particle
filter](https://en.wikipedia.org/wiki/Particle_filter), distributed
across many processes which might be on different machines. Since
we want multi-machine distribution, we use TCP sockets to communicate
between the processes doing the computation.

At the core of the program logic we have a function taking some
`State` and some `Input`, and generating some new states and an
output associated with each one:

```haskell
type Evolve = State -> Input -> [(State, Output)]
```

Note that a single state and input pair generates multiple states
and output.[^pf-multiple-outputs] We need to run one such function
on thousands of inputs:

[^pf-multiple-outputs]: The multiple outputs are due to the fact
that in a particle filter each state (or rather each "particle")
can be sampled 0 or multiple times.

```haskell
-- Apply the `Evolve` to every given `State`, return
-- the new states and output.
evolveMany :: Evolve -> [State] -> [Input] -> [[(State, Output)]]
evolveMany f = zipWith f
```

Given this initial specification, there are a couple of adjustments
we need to make if we want to be able to distribute the computation.
First, the function will have to live in `IO`, since communication
will happen through `Socket`s. Second, we won't refer to the states
directly, but rather refer to them using tokens provided by the
system. At the beginning we'll provide the initial states and get
back tokens in result, and at each call to `evolveMany` we'll get
-- instead of new `State`s -- new tokens.

We can do this because we do not care about the content of the
states (while we care about the outputs) and referring to them with
tokens rather than directly we can avoid transferring them to other
processes each time we need to operate on them, saving a lot of
bandwidth and speeding up the computation greatly.

Thus, we'll also need to book-keep which slave processes are holding
which state.

Finally, we'll need `Socket`s to communicate with the slave processes.

This gives us a new API:

```haskell
-- We use `Map` and `Set` from `containers` for illustrative purposes, `HashMap`
-- from `unordered-containers` or a mutable hash table from `hashtables`
-- will most likely be more performant.
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Some token representing a `State` on some slave.
data StateId

-- Some token representing a slave.
data SlaveId

-- Reset the states in the system to the given ones, returns a
-- 'StateId' for each state.
resetStates ::
     Map SlaveId Socket -- Connection to the slaves
  -> [State]
  -> IO (Map SlaveId (Set StateId), [StateId])
  -- Returns how the states have been repartitioned on the slaves
  -- and a list to know which `StateId` corresponds to which `State`.

-- Evolves the states with the given inputs, returns the outputs and
-- the new 'StateId's resulting from the evolution. 
evolveMany ::
     Map SlaveId Socket -- Connections to the slaves
  -> Map SlaveId (Set StateId) -- Which states are on which slave
  -> Map StateId Input -- Inputs to each state
  -> IO (Map SlaveId (Set StateId), Map StateId [(StateId, Output)])
  -- Returns the new mapping from slaves to states, and
  -- the outputs.
```

When using this API, the usual pattern is to call `resetStates` at
the beginning with the initial states and then a series of `evolveMany`
afterwards, each using the `StateId`s returned from `resetStates`
the first time and `evolveMany` afterwards.

The challenge is to implement `evolveMany` as efficiently as possible.

To give an idea of the time involved, we usually have around 2000
states, a few tens of calls to `evolveMany`, and each call to
`Evolve` takes a few tenths of seconds to complete, giving a
single-threaded run time of a few hours, e.g.

```
 2000 * -- Number of states
   80 * -- number of calls to evolveMany
0.03s = -- Evolve time 
1h 20m  -- Total running time
```

## High level overview of the implementation

`resetStates` just assigns a unique `StateId` to each state, and then
splits up and uploads the states evenly between the slaves.

All the complexity lies in `evolveMany`: the goal is to utilize the
slaves as efficiently as possible.

We found pretty early that naively evolving the states present on each
slave would not work, because:

* Each call to `Evolve` results in many (possibly 0) children
    states (since the return type is a list), and we
    cannot predict how many we'll get in advance. This would cause
    different slaves to have a different number of states after a few
    calls to `evolveMany`, which in turn would cause the slaves to not
    be used efficiently, since some would end up being idle;
* The runtime of an individual `Evolve` depends on the state
    and on the input, and we cannot predict it. This also can cause
    some slaves to finish earlier than others, causing inefficiencies.

More concretely, imagine a situation with 10 states, where 9 of the
states take 1 second while there is an odd state that takes 10
seconds. If we have 2 slaves at our disposal, the most efficient
distribution is to assign the slow state to one slave, and all the
others to another slave, with one slave taking 10 seconds and the
other taking 9. If we just distribute the states evenly between the
slaves, 1 slave will take 14 seconds and one 5. Since the total runtime
will be constrained by the slowest slave, we must be careful to avoid
such long tails.

So we switched to a simple but effective method to utilize the slaves
efficiently. The master process keeps track of the states present on
each slave, and asks the slaves to process them in batches, say of 5.
When a slave finishes its batch, it sends the output back to the master
and waits for further instructions. If the slave still has states to
evolve, the master sends a request for a new batch to be evolved. If the
slave does not have states to update the master will search for a slave
with states to spare, and request them. When a slave receives such a
request it sends back the states to the master, which will forward them
to the needy slave. When there are no more states to update, `evolveMany`
is done.

The algorithm can be summed up as two state machines, one for the master
and one for the slave:

```haskell
-- This is what the master sends to the slave.
data Request
  -- Evolve the selected states
  = EvolveStates [StateId]
  -- Add the given states
  | AddStates [(StateId, State)]
  -- Remove the requested states, and return them to the master
  | RemoveStates [StateId]

-- This is what the slaves reply to the master.
data Response
  = StatesEvolved [(StateId, [(StateId, Output)])]
  | StatesAdded
  | StatesRemoved [(StateId, State)]

-- The slave has a set of `State`s indexed by `StateId`, and it updates
-- it at each request from the master.
slaveStateMachine :: Map StateId State -> Request -> (Map StateId State, Response)

-- Some type to refer to slaves uniquely.
data SlaveId

-- The master keeps track of which states each slave has, and will update
-- it. It also records the outputs we have received from the slaves so far.
data MasterState = MasterState
  { msSlavesStates :: Map SlaveId (Set StateId)
  , msStatesToEvolve :: Map StateId Input
  , msEvolvedStates :: Map StateId [(StateId, Output)]
  }

-- At each response from a slave the master updates its state and then
-- might reply with a new `Request`. Note that the `Request` might not
-- be directed at the same slave that sent the `Response`, since sometimes
-- we need to steal slaves from other slaves since the slave at hand does
-- not have states to update.
masterStateMachine ::
     MasterState -> SlaveId -> Response
  -> (MasterState, Maybe (SlaveId, Request))
```

The most common pattern of interaction between slave and master
will be of a loop of `EvolveStates` and `StatesEvolved`:

<div class="center-image"><img src="/assets/images/work-queue-diagram-1.svg" alt="EvolveStates and StatesEvolved"></div>

This interaction between slave and master will continue until one
slave will runs out of states to evolve. In that case, the master
will have to reach out to some other slave to be able to provide
the needy slave with something to evolve. For example, this is what will
happen if slave 3 runs out of states and the master decides to ship some states
to it from slave 2:

<div class="center-image"><img src="/assets/images/work-queue-diagram-2.svg" alt="Running out of states"></div>

The exact implementation of the state machines is not relevant, but
given their types what's to note is that:

* The slave will be a very simple loop that just waits for a request, processes
    it, and then replies to the master.
* The master, on the other hand, is a bit more complicated: it needs to wait for
    responses from any slave, which means that we'll have to multiplex over multiple
    channels; and then it can reply to any slave.

## First attempt, and performance

Now that we have abstracted out the logic of the master and the slaves in
self-contained state machines, we can describe the slave and master processes.
We'll assume `IO` functions to send and receive messages.

The slave implementation is trivial and won't change:

```haskell
-- These functions will use `recv`/`send` to work with the `Socket`s,
-- and the `store` library to efficiently deserialize and serialize
-- the requests and responses.
receiveRequest :: Socket -> IO Request
sendResponse :: Socket -> Response -> IO ()

slave ::
     Socket -- Connection to master
  -> IO a
slave sock = loop mempty -- No states at the beginning
  where
    loop :: Map StateId State -> IO (Map StateId State)
    loop states = do
      req <- receiveFromMaster sock
      (states', resp) <- slaveStateMachine states req
      sendToMaster sock resp
```

Note that a slave process is not bound to a single call to `evolveMany`,
it just takes requests from a master.

The master on the other hand is essentially the implementation of
`evolveMany`, and we have a lot more options to implement it. Our
first version is a pretty idiomatic Haskell program, using one
thread per slave so that we can wait on all of them at once, with
the master state stored in an `MVar` that can be accessed from
all the slave threads:

<div class="center-image"><img src="/assets/images/work-queue-diagram-3.svg" alt="First implementation"></div>

Each slave thread will run code waiting on a slave, modifying the
shared state using the master state machine:

```haskell
import Control.Concurrent.MVar

receiveResponse :: Socket -> IO Response
sendRequest :: Socket -> Request -> IO ()

-- Terminates when there is nothing left to do.
slaveThread :: Map SlaveId Socket -> MVar MasterState -> SlaveId -> IO ()
slaveThread slaveSockets masterStateVar slaveId = do
  resp <- receiveResponse (slaveSockets Map.! slaveId)
  (masterState, mbReq) <- modifyMVar masterStateVar $ \masterState ->
    let (masterState', mbReq) =
          masterStateMachine masterState slaveId resp
    return (masterState', (masterState', mbReq))
  -- Send the request if needed
  mapM_
    (\(slaveId, req) -> sendRequest (slaveSockets Map.! slaveId) req)
    mbReq 
  -- Continue if there are still slates to evolve
  unless (Map.null (msStatesToEvolve masterState)) $
    slaveThread masterStateVar slaveId

-- Runs the provided actions in separate threads, returns as
-- soon as any exists
raceMany_ :: [IO ()] -> IO ()
raceMany_ xs0 = case xs0 of
  -- `race_` is from the `async` package.
  [] -> return ()
  [x] -> x
  x : xs -> race_ x (raceMany_ xs)
 
evolveMany ::
     Map SlaveId Socket
  -> Map SlaveId (Set StateId)
  -> Map StateId Input 
  -> IO (Map SlaveId (Set StateId), Map StateId [(StateId, Output)])
evolveMany slaveSockets slaveStates inputs = do
  masterStateVar <- newMVar MasterState
    { msSlavesStates = slaveStates
    , msStatesToEvolve = inputs
    , msEvolvedStates = mempty
    }
  -- Run one thread per slave until one receives the response
  -- after which there are no states to evolve
  raceMany_ (map (slaveThread masterStateVar) (Map.keys slaveStates))
  -- Return the results in the `MasterState`
  masterState <- readMVar masterStateVar
  return (msSlavesStates masterState, msEvolvedStates masterState)
```

This implementation is simple and quite obviously correct, and it's
also pretty fast. In fact, we were able to scale up to around 20
slaves quite well with it:

<div class="center-image"><img src="/assets/images/work-queue-single-machine-first.svg" alt="Performance up to 17 slaves, first implementation"></div>

Note that both axes for this and every other plot in this article
are logarithmic: if we scaled perfectly we'd get a straight line,
which we're pretty close to.

However, things go downhill if we try to scale beyond 20 slaves.
Here is a sample of the runtime with up to 450 slaves for six different
scenarios:

<div class="center-image"><img src="/assets/images/work-queue-first.svg" alt="Performance up to 450 slaves, first implementation"></div>

These measurements were all taken on clusters of c4.8xlarge AWS
instances with 18 physical cores, with up to 30 machines running
at once.[^cluster]

[^cluster]: The benchmarking was automated using terraform, which was
invaluable when evaluating the improvements.

It's evident that the distribution does not scale beyond around 40
slaves, and stalls completely between 50 and 100 slaves, after which
adding slaves is detrimental to the runtime. Note that for the
scenarios taking more time the scaling is better: this is because
for those scenarios each individual call to the `Evolve` function
takes longer, and thus the overhead of the distribution is less
substantial. This is the case for scenario D, which starts out being
the slowest with 17 slaves, taking more than 4000 seconds rather
than 800-1000 seconds, but scaling much better.

From this data it was clear that if we wanted to be able to leverage
a large number of machines to run our simulations in a minute or
less we had to improve the performance of `evolveMany`.

Small aside: note how these plots contains a line "with taskset"
and one without, with the one without performing noticeably worse.
The line with taskset indicates measurements taken where each Haskell
process is pinned to a physical CPU core: this improves performance
substantially compared to letting the kernel schedule them.[^runtimes]
After finding this out we ran all subsequent tests pinning slave
processes to physical cores. Hyperthreading was also detrimental
to the runtime, since the increased distribution overhead far
outweighed the gained CPU time; so we used only one
  process per physical CPU core and avoided hyperthreading.

[^runtimes]: Keep in mind that since we're distributing the work
manually using TCP sockets each slave is a separate OS process that
runs a dedicated Haskell runtime, which is why it makes sense to pin
it to a single core.

## Second attempt

By measuring how much time each slave spent working and how much
time it spent waiting for instructions from the master, it became
clear that the program was getting slower because the slaves spent
more and more time waiting for instructions, rather than actually
working. Thus, if we wanted proper scaling, we needed to lower the
latency between the time a response reached the master and the time
the slave received the next request.

Now, we tried to gain conclusive evidence of why our first version
of `evolveMany` is slow, but profiling these sort of applications
is quite hard unless you're intimately familiar with the Haskell
runtime -- which is almost like saying "if you are Simon Marlow".

We had however some hypotheses of why our program was slow. One
possibility is that the event manager can simply not handle hundreds
of connections at the same time efficiently, at least in our use
case.

Another suspicion is that the multi-threadedness of the first version
played at our disadvantage since there would be a lot of pointless
context-switches while one thread was already modifying the `MVar
MasterState`. In other words, any context switch between slave
threads while one slave thread is already holding the `MVar
MasterState` is (almost) wasted, since it'll be blocked on the `MVar
MasterState` right after receiving a slave response and will yield,
delaying the completion of the loop body in the thread that was
already processing the `MasterState`.

While our second version was based on these hypotheses we were
quite short on time and did not want to take the risk of rewriting
the program to find that we still could not scale as we desired.
Thus, we set ourselves to write the fastest possible version of
`evolveMany` that we could think of.

The main change we wanted was to turn the server from a multi-threaded
server multiplexing through the event manager to a single-threaded
application multiplexing the sockets directly.

In Linux, the `epoll` set of syscalls exist for this exact reason: you
can register multiple sockets to wait on with `epoll_ctl`, and
then wait for any of them to be ready using `epoll_wait`.

However in Haskell `epoll` is abstracted over by the GHC event
manager, so there is no library to use these facilities directly.
The GHC event manager does offer an interface to it in the form of
[`GHC.Event.registerFd`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/GHC-Event.html).
However all these functions are callback based -- they take a
function that will be called in a green thread when the socket is
ready. Thus we cannot easily write a single threaded program directly
using it. If we want to write a single-threaded loop we're forced
to go through an additional synchronization primitive such an `MVar`
to signal that a socket is ready to be read from in the callback
provided to `registerFd`.[^normal-block] We tried this approach and
got no performance improvement.

[^normal-block]: Note that the normal blocking read for Haskell
sockets is implemented using
[`threadWaitRead`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/Control-Concurrent.html#v:threadWaitRead),
which uses `registerFd` in exactly this way, by having the callback
to fill in an `MVar` that `threadWaitRead` will wait on.

Thus we decided to just write the loop using `epoll` directly, which
proved very painless given that the GHC codebase already contains
bindings to the `epoll` functions, as part of the event manager.
We released a simple library for people that need to do the same,
[`simple-poll`](https://github.com/fpco/simple-poll). Right now it
only supports `epoll`, and is thus limited to Linux, but it should
be easy to extend to other platforms by copy-pasting other bits of
code from the GHC event manager.

Updating the old loop to an explicit multiplexing style, we have:

```haskell
-- `System.Poll.EPoll` comes from the `simple-poll` package
import System.Poll.EPoll (EPoll)
import qualified System.Poll.EPoll as EPoll
import Network.Socket (Socket(MkSocket))
import System.Posix.Types (Fd(Fd))

-- Receives first responses to arrive from any of the slaves.
-- This amounts to calling `EPoll.wait` to get back a list of
-- sockets to read from, and then draining them in turn to
-- decode the `Response`.
-- 
-- Note that draining them might still not give us a response,
-- since the full response might not be available all at once,
-- and thus in the full version of the software this function will have
-- to hold somes state holding partially read messages.
-- 
-- Also note that in the real software it's much better to return
-- a list of `(SlaveId, Response)` pairs. We have it return only
-- one for simplicity.
receiveFromAnySlave ::
     EPoll
  -> Map Fd SlaveId
  -- Reverse lookup table from `Fd`s to `SlaveId`s. We need it
  -- since `EPoll.wait` gives us the `Fd`s which are ready to
  -- be read from, and from that we need to get back which
  -- `SlaveId` it corresponds to, to return it.
  -> IO (SlaveId, Response)

-- Utility to get a file descriptor out of a `Socket`
socketFd :: Socket -> Fd
socketFd (MkSocket fd _ _ _ _) = Fd fd

evolveMany ::
     Map SlaveId Socket -- All the connections to the slaves
  -> Map SlaveId (Set StateId) -- The states held by each slave
  -> Map StateId Input -- The inputs to each state
  -> IO (Map SlaveId (Set StateId), Map StateId [(StateId, Output)])
evolveMany slaveSockets slaveStates inputs = EPoll.with 256 $ \epoll -> do
  -- First register all the sockets with `epoll_ctl`. `epollIn` is to
  -- indicate that we want to be notified when a socket can be read from.
  forM_ slaveSockets $ \socket ->
    EPoll.control epoll Epoll.controlOpAdd (socketFd socket) EPoll.epollIn
  -- Then start the event loop
  masterState <- loop epoll MasterState
    { msSlavesStates = slaveStates
    , msStatesToEvolve = inputs
    , msEvolvedStates = mempty
    }
  return (msSlavesStates masterState, msEvolvedStates masterState)
  where
    fdToSlaveIds :: Map Fd SlaveId
    fdToSlaveIds =
      Map.fromList [(socketFd sock, slaveId) | (slaveId, sock) <- Map.toList slaveSockets]
    
    loop :: EPoll -> MasterState -> IO (Map StateId [(StateId, Output)])
    loop epoll masterState = do
      -- Get a response from some slave
      (slaveId, resp) <- receiveFromAnySlave epoll slaveSockets
      -- Update the state accordingly
      let (masterState', mbResp) =
            masterStateMachine masterState slaveId resp
      -- Send the new requests
      mapM_ (uncurry sendToSlave) mbResp
      -- Continue if we're not done
      unless (Map.null (msStatesToEvolve masterState')) (loop masterState')
```

Once we did this, the performance increased dramatically, fulfilling
our current scaling needs and probably getting quite close to optimal
scaling for our use case, although we have not researched what more
margin for improvements we have since we do not need them for now.

Going back to the original set of plots, the blue line shows the
improved performance with our second implementation:

<div class="center-image"><img src="/assets/images/work-queue-second.svg" alt="Improved performance"></div>

The plots clearly show a much nicer scaling pattern as the number
of slaves increases, and runtimes of often 100 seconds of less,
which represent a 2x to 5x improvement compared to the first version.

We also integrated other micro optimizations that yielded less substantial
improvements (in the 5 to 10%) range, such as

* Using mutable hashtables instead of unordered-containers for most
    of the bookkeeping.
* Reading from the `Socket` directly into a
    [`ByteBuffer`](https://www.stackage.org/haddock/lts-7.14/store-0.2.1.2/System-IO-ByteBuffer.html#t:ByteBuffer)
    and deserializing directly from there rather than copying into
    intermediate `ByteString`s, reducing allocations drastically to
    perform deserialization, since we allocate the buffer where the
    socket data is read into upfront.

## Conclusion

Our biggest takeaway from this experience is that in Haskell we can
have the confidence that we'll always be able to write the task at
hand to be as fast as possible with relative ease. Writing the
`epoll` based version took around a day, including factoring out the
bindings from the GHC event manager into a library.

Moreover, it's important to remember that the normal facilities for
fast IO in Haskell (green threads + transparent evented IO) is fast
enough for the overwhelming majority of cases, and much easier to
manage and think about than manual evented IO.  Michael Snoyman
[recently
compared](https://www.fpcomplete.com/blog/2017/01/green-threads-are-like-garbage-collection)
green threads to garbage collection, an apt comparison. Our software
is one of the cases where the abstraction prevents performance,
and thus we need to work without it.

Finally, it would be great to gain hard evidence on why the first
program was slow, rather than just hypotheses. We tried quite hard
to understand it but could not reach conclusive evidence in the
time we had. We hope to get to the bottom of this issue when we
have the time, and maybe make profiling these kind of programs easier
in the meantime.

## Acknowledgments

The work described was performed with Philipp Kant and Niklas
Hambüchen. Thanks to Michael Snoyman, Philipp Kant, and Niklas
Hambüchen for reviewing drafts of this blog post.
