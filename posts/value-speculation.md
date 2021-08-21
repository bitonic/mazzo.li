---
title: Beating the L1 cache with value speculation
date: 2021-07-20
tags: [post]
sidenotes: true
---

**Abstract**: If we have a heuristic to guess some value cheaply, we can remove a data dependency in a tight loop using the branch predictor. This allows the CPU to run more instructions in parallel, increasing performance. If this explanation does not make much sense to you, keep reading to learn about some of the magic making your CPU fast!

[Per Vognsen](https://twitter.com/pervognsen)'s twitter feed is full of neat low-level curiosities, usually leveraging CPU features for some performance benefit.

[Recently](https://twitter.com/pervognsen/status/1412611878140874757) he tweeted about a trick that I had never heard of -- value speculation.[^pvk] The trick exploits the branch predictor to guess values, enabling more instruction parallelism and therefore removing a bottleneck on the L1 cache. Note that the bottleneck is _not_ due to L1 cache misses, but on L1 cache _hits_ introducing unwanted data dependencies.

[^pvk]: Per, in turn, referenced [a blog post by Paul Khuong](https://pvk.ca/Blog/2020/07/07/flatter-wait-free-hazard-pointers/) with a real-world example deploying this trick. Paul, in turn, references side-channel attacks.

In this post I explain the machinery involved, including a primer on branch prediction and CPU caches, so that anybody with a passing knowledge of C and how code is executed on CPUs should be able to follow.

The code for the post is available [here](https://gist.github.com/bitonic/78887f5d3238bab5e31f3c5a41d404b2). All the numbers are from a Xeon E5-1650 v3, an Intel Haswell processor with L1 / L2 / L3 cache of 32kB, 256kB, and 15MB respectively. The code was compiled with `clang -O3`, and not with `gcc`, for reasons explained [later](#compiling).

Before starting, I'd like to stress that L1 cache _hits_ are almost certainly _not_ the bottleneck of your application! This is just a very neat trick that illuminates some CPU features, not a guide on how to improve the performance of your average piece of C code.

## The setup -- summing linked lists

We have a simple linked list data type, and a function summing all the elements of a given linked list:

```c
typedef struct Node {
  uint64_t value;
  struct Node *next; // NULL for the last node
} Node;

uint64_t sum1(Node* node) {
  uint64_t value = 0;
  while (node) {
    value += node->value;
    node = node->next;
  }
  return value;
}
```

So far so good. Our test case works as follows: build a linked list where the nodes live sequentially in contiguous memory, then see how long it takes to sum them all up:

```c
// Allocate 5MB of linked list nodes, and link them sequentially, with
// random data in the `value`s.
uint64_t n = 312500llu; // 312500 * sizeof(Node) = 312500 * 16 bytes = 5000000 bytes
Node *nodes = malloc(n * sizeof(Node));
for (uint64_t i = 0; i < n - 1; i++) {
  nodes[i].value = random_uint64();
  nodes[i].next = &nodes[i+1];
}
nodes[n-1].value = random_uint64();
nodes[n-1].next = NULL;

// Now sum.
sum1(&nodes[0]);
```

On a server with a relatively old Xeon E5-1650 v3, running `sum1` with the sample data takes 0.36 milliseconds, which means that we're processing our linked list at roughly 14GB/s. In the rest of the post will will identify the bottleneck and get around it with value speculation, bringing the throughput for this dataset to 30GB/s.

The impact of the fix varies depending on the size of the dataset. If it is already entirely in the CPU cache, the improvement is much more pronounced, since otherwise we are quickly constrained by how fast we can read data from RAM. This graph shows the performance improvement over differently sized datasets (higher is better):

<div class="center-image" style="margin-top:1rem; margin-bottom:1rem;">
![<small>Chart showing the performance of various versions of `sum`, including `sum1` as described above. Multiple iterations of the same functions are run, to ensure the data is already in the cache if possible.</small>](/assets/images/value-speculation-chart.svg)
</div>

The chart shows the performance of `sum1` together with the performance of two improved functions, `sum2` and `sum3`. We go from a throughput of 14GB/s in `sum1` to more than 45GB/s in `sum3` if the data fits entirely in the L1 cache (the 16kB dataset), with the performance decreasing slightly for datasets fitting in the L2 and L3 cache (128kB and 5MB datasets). If the dataset does not fit entirely in any CPU cache (~4GB dataset) we go from 10GB/s to 15GB/s, which is as fast as the RAM allows.[^ram-speed]

[^ram-speed]: See remarks in the [last section](#results) for more data on why I think 15GB/s is the limit without resorting to deeper changes.

## Instruction parallelism and branch prediction

<div> <!-- footnote group -->

Modern CPUs do not process instructions serially, but rather handle many at the same time. They read many instructions at once, break them down in stages, and then try to fill all the computation units they have with as many tasks from as many instructions as possible.[^ooo-pointers] For instance, modern Intel processors are designed for a throughput of 4 instructions per clock cycle, and AMD Zen processors for up to 5 or 6.[^agner-arch]

[^ooo-pointers]: To expand on this topic, you can start reading on
[out-of-order execution](https://en.wikipedia.org/wiki/Out-of-order_execution) and
[pipelining](https://en.wikipedia.org/wiki/Instruction_pipelining).

[^agner-arch]: Agner Fog's [microarchitecture document](https://www.agner.org/optimize/microarchitecture.pdf) contains tons of details about the pipeline characteristics for Intel and AMD x86 processors. The numbers on throughput for each architecture are usually in the "Pipeline" section.

However, branches pose a challenge when wanting to execute instructions in parallel. Let's go back to our function `sum1`:

</div>

```c
uint64_t sum1(Node* node) {
  uint64_t value = 0;
  while (node) {
    value += node->value;
    node = node->next;
  }
  return value;
}
```

and its very readable assembly version:

```asm
; rdi = node and rax = value.
; rax is the return value register (we're returning value)
sum1:
  xor     rax, rax                 ; value = 0
  test    rdi, rdi                 ; if node is NULL, exit, otherwise start loop
  je      end
loop:
  add     rax, qword ptr [rdi]     ; value += node->value
  mov     rdi, qword ptr [rdi + 8] ; node = node->next
  test    rdi, rdi                 ; if node is not NULL, repeat loop,
  jne     loop                     ; otherwise exit
end
  ret
```

The loop body is made out of 4 instructions, the last of which a jump. Without special measures, every instruction up to the `jne` must be executed before proceeding to the next instruction, since we need to know if we'll go to the beginning of the loop or continue. In other words the conditional jump would introduce a barrier in the instruction level parallelism internal to the CPU.

However, executing many instructions at once is so important that dedicated hardware -- the _branch predictor_ -- is present in all modern CPUs to make an educated guess on which way we'll go at every conditional jump. The details of how this works are beyond the scope of this blog post, but conceptually your CPU observes your program as it runs and tries to predict which branch will be taken by remembering what happened in the past.[^branch-predictor]

[^branch-predictor]: Apart from the ever useful Agner Fog (see Section 3 of the [microarchitecture document](https://www.agner.org/optimize/microarchitecture.pdf)), Dan Luu has a [nice blogpost](https://danluu.com/branch-prediction/) explaining less dryly various ways of performing branch prediction.

Even without knowing much about the branch prediction, we expect the predictor to do a great job for our test case -- we always go back to the beginning of the loop apart from when we stop consuming the list. On Linux, we can verify that this is the case with `perf stat`:

```
$ perf stat ./value-speculation-linux
...
         2,507,580      branch-misses             #    0.04% of all branches
```

The branch predictor gets it right 99.96% of the time. So the CPU can parallelize our instructions with abandon, right? ...right?

## Data dependencies tripping us up

Let's focus on the loop body of `sum1`:

```asm
; rdi = node and rax = value.
loop:
  add     rax, qword ptr [rdi]     ; value += node->value
  mov     rdi, qword ptr [rdi + 8] ; node = node->next
  test    rdi, rdi                 ; if node is not NULL, repeat loop,
  jne     loop                     ; otherwise exit
```

To increment `value` (`rax`), we need to know the value of `node` (`rdi`), which depends on the `mov` in the previous iteration of the loop. The same is true for the `mov` itself -- it is also dependent on the result of the previous `mov` to operate. So there's a _data dependency_ between each iteration of the loop: we must have finished reading `node->next` (`[rdi + 8]`) at iteration $n$ before we can start executing the `add` and `mov` at iteration $n+1$.

Moreover, reading the `node->next` (`[rdi + 8]`) is slower than you might think.

<div class="center-image" style="margin-top:1rem; margin-bottom:1rem;">
![<small>Diagram showing the CPU caches for the processor used in this post. Generated with `lstopo`.</small>](/assets/images/lstopo-ram256g-1.svg)
</div>

<div> <!-- footnote group -->

Modern CPUs are a lot better at adding numbers than reading from memory. For this reason, a series of fast caches exist between the CPU and main memory. All reading and writing from main memory normally goes through the cache -- if the data we are interested in is not already present, the CPU will load a chunk of memory (a "cache line", 64 bytes on x86) which contains our desired data into the cache.[^bypassing-cache] The fastest cache is usually called L1 (successive caching layers being predictably called L2, L3, ...).

[^bypassing-cache]: I say "normally" because the cache can be avoided using streaming SIMD instructions, which can write or copy memory bypassing the cache. However these methods are opt-in, and by default all memory goes through the cache.

Our setup is the best-case scenario when it comes to CPU caches -- we read a bunch of memory sequentially, utilizing every byte along the way. However, even if the L1 cache is very fast, it is not free: it takes around 4 CPU cycles to read from it. This will make our `mov` and `add` take at least 4 cycles to complete. The other two instructions, `je` and `test`, will take only one cycle.[^agner-instrs]

[^agner-instrs]:
    Again, Agner Fog's [page on performance](https://www.agner.org/optimize/) is the best resource I could find to source these numbers. For example, if one wanted to find these numbers for a Haswell CPU:

    * The L1 latency (4 cycles) is in section 10.11 of the [microarchitecture guide](https://www.agner.org/optimize/microarchitecture.pdf);
    * The numbers of cycles it takes to execute `mov`, `add`, `test`, and `jne` are in the Haswell section of the [instruction tables](https://www.agner.org/optimize/instruction_tables.pdf).

So the number of cycles needed to go through a single loop iteration is bounded by the 4 cycles it takes to read from L1 cache. The data I get from the Xeon I tested the program with is roughly consistent with this:

</div>

```
16kB, 10000 iterations
  sum1:  8465052097154389858,  1.12us,  14.25GB/s,  3.91 cycles/elem,  1.03 instrs/cycle,  3.48GHz,  4.01 instrs/elem
128kB, 10000 iterations
  sum1:  6947699366156898439,  9.06us,  14.13GB/s,  3.95 cycles/elem,  1.01 instrs/cycle,  3.49GHz,  4.00 instrs/elem
5000kB, 100 iterations
  sum1:  2134986631019855758,  0.36ms,  14.07GB/s,  3.96 cycles/elem,  1.01 instrs/cycle,  3.48GHz,  4.00 instrs/elem
4294MB, 1 iterations
  sum1: 15446485409674718527,  0.43 s,   9.94GB/s,  5.60 cycles/elem,  0.71 instrs/cycle,  3.48GHz,  4.00 instrs/elem
```

The important numbers are `cycles/elem` and `instrs/cycle`. We spend roughly 4 cycles per list element (that is to say, per loop iteration), corresponding to a throughput of roughly 1 instruction per cycle. Given that the CPU in question is designed for a throughput of 4 instructions per cycle, we're wasting a lot of the CPU magic at our disposal, because we're stuck waiting on the L1 cache.

## Value speculation bailing us out

We finally get to the trick. As discussed, we are stuck waiting on reading what the next node address is. However, in our setup we allocate the list in a contiguous block of memory, and therefore the nodes are always next to each other.

So here's the key idea: try to guess the next node by just bumping the previous value. If the guess is wrong, set the node to the "real" next value. In C, this is how it would look like:

```c
uint64_t faster_sum(Node* node) {
  uint64_t value = 0;
  Node* next = NULL;
  while (node) {
    value += node->value;
    next = node->next;
    // Guess the next value
    node++;
    // But fix it up if we guessed wrong (in case the nodes are not
    // next to each other).
    if (node != next) {
      node = next;
    }
  }
  return value;
}
```

This looks quite bizarre. We are still reading `node->next` in the comparison `node != next` to make sure our guess is right. So at first glance this might not seem like an improvement.

This is where the branch predictor comes in. In the case of lists where most nodes _are_ next to each other (as is the case in our test code), the branch predictor will guess that the `if (node != next) { ... }` branch is not taken, and therefore we'll go through loop iterations without having to wait for the L1 read.

Note that when the branch predictor _is_ wrong (for example when the list ends, or if we have non-contiguous nodes) the CPU will need to backtrack and re-run from the failed branch prediction, which is costly (15 to 20 cycles on our processor[^branch-miss-penalty]). However, if the list is mostly contiguous, the trick works and makes our function 50-200% faster.

[^branch-miss-penalty]: See "Misprediction penalty" for Haswell processor in Agner Fog's
[microarchitecture document](https://www.agner.org/optimize/microarchitecture.pdf).

However there is one last challenge remaining to reach the final code and show you numbers -- convincing compilers that our code is worth compiling.

## Getting compilers to emit the right code {#compiling}

Let's go back to the code we showed for value speculation in C:

```c
uint64_t faster_sum(Node* node) {
  uint64_t value = 0;
  Node* next = NULL;
  while (node) {
    value += node->value;
    next = node->next;
    node++;
    if (node != next) {
      node = next;
    }
  }
  return value;
}
```

Both `gcc` and `clang` easily deduce that the guessing is semantically pointless, and compile our trick away, making the compiled version of `faster_sum` the same as `sum1`. This is an instance where the compiler smartness undoes human knowledge about the underlying platform we're compiling for.

Per Vognsen's gist uses the following trick to get compilers to behave -- this is the first improvement to our `sum1`, `sum2`:

```c
static uint64_t sum2(Node *node) {
  uint64_t value = 0;
  while (node) {
    value += node->value;
    Node *predicted_next = node + 1;
    Node *next = node->next;
    if (next == predicted_next) {
      // Prevent compilers optimizing this apparently meaningless branch away
      // by making them think we're changing predicted_next here.
      //
      // This trick, however, does not work with GCC, only with clang. GCC here
      // derives that `next` and `predicted_next` are the same, and therefore
      // merges them into the same variable, which re-introduces the data
      // dependency we wanted to get rid of.
      asm("" : "+r"(predicted_next));
      node = predicted_next;
    } else {
      node = next;
    }
  }
  return value;
}
```

However `gcc` still doesn't fully fall for it, as explained in the comment.[^compilers] Moreover, `clang`'s generated loop is not as tight as it could, taking 10 instructions per element. So I resorted to manually writing out a better loop, which we'll call `sum3`:[^assembly]

[^compilers]: This is why I stuck to `clang` for this post. I don't know what compiler Per is using for his tests.

[^assembly]: Here I show the assembly version in Intel syntax, but [in the code](https://gist.github.com/bitonic/78887f5d3238bab5e31f3c5a41d404b2#file-value-speculation-linux-c-L121) I write inline assembly, using AT&T syntax since it is better supported.

```asm
; rax = value, rcx = next, rdi = node
; Note that rax is the return value register (we are returning the value)
sum3:
  xor     rax, rax                   ; value = 0
  xor     rcx, rcx                   ; next = NULL
  test    rdi, rdi                   ; if node is null, go to the end,
  je      end                        ; otherwise start loop
loop_body:
  add     rax, qword ptr [rdi]       ; value += node->value
  mov     rcx, qword ptr [rdi + 8]   ; next = node->next
  add     rdi, 16                    ; node++
  cmp     rcx, rdi                   ; if node is equal to next,
  je      loop_body                  ; restart loop, otherwise fix up node
  mov     rdi, rcx                   ; node = next
  test    rdi, rdi                   ; if node is not NULL restart the loop,
  jne     loop_body                  ; otherwise exit.
end:
  ret
```

The code relies on the fact that `node` can't be `NULL` after we increment it if it is equal to `next`, avoiding an additional test, and taking only 5 instructions per element (from `loop_body` to `je loop_body` in the happy path).[^rihalto]

[^rihalto]: The original version of `sum3` took 6 instructions per cycle, until [Rihalto pointed out](https://twitter.com/RhialtoTheM/status/1418926515526459394) a needless jump.

## Results {#results}

These are the final numbers for our four functions:

```
16kB, 10000 iterations
  sum1:  8465052097154389858,  1.12us,  14.25GB/s,  3.91 cycles/elem,  1.03 instrs/cycle,  3.48GHz,  4.01 instrs/elem
  sum2:  8465052097154389858,  0.57us,  27.97GB/s,  1.99 cycles/elem,  5.02 instrs/cycle,  3.48GHz, 10.01 instrs/elem
  sum3:  8465052097154389858,  0.36us,  44.96GB/s,  1.24 cycles/elem,  4.05 instrs/cycle,  3.48GHz,  5.01 instrs/elem
128kB, 10000 iterations
  sum1:  6947699366156898439,  9.05us,  14.14GB/s,  3.95 cycles/elem,  1.01 instrs/cycle,  3.49GHz,  4.00 instrs/elem
  sum2:  6947699366156898439,  4.51us,  28.38GB/s,  1.97 cycles/elem,  5.09 instrs/cycle,  3.49GHz, 10.00 instrs/elem
  sum3:  6947699366156898439,  3.79us,  33.80GB/s,  1.65 cycles/elem,  3.03 instrs/cycle,  3.49GHz,  5.00 instrs/elem
5000kB, 100 iterations
  sum1:  2134986631019855758,  0.35ms,  14.09GB/s,  3.95 cycles/elem,  1.01 instrs/cycle,  3.48GHz,  4.00 instrs/elem
  sum2:  2134986631019855758,  0.19ms,  26.27GB/s,  2.12 cycles/elem,  4.72 instrs/cycle,  3.48GHz, 10.00 instrs/elem
  sum3:  2134986631019855758,  0.17ms,  28.93GB/s,  1.93 cycles/elem,  2.60 instrs/cycle,  3.48GHz,  5.00 instrs/elem
4294MB, 1 iterations
  sum1: 15446485409674718527,  0.44 s,   9.66GB/s,  5.76 cycles/elem,  0.69 instrs/cycle,  3.48GHz,  4.00 instrs/elem
  sum2: 15446485409674718527,  0.33 s,  13.19GB/s,  4.22 cycles/elem,  2.37 instrs/cycle,  3.48GHz, 10.00 instrs/elem
  sum3: 15446485409674718527,  0.30 s,  14.20GB/s,  3.91 cycles/elem,  1.28 instrs/cycle,  3.47GHz,  5.00 instrs/elem
```

<div class="center-image" style="margin-top:1rem;">
![](/assets/images/value-speculation-chart.svg)
</div>

The numbers are provided [by the Linux `perf_event_open` syscall](https://gist.github.com/bitonic/78887f5d3238bab5e31f3c5a41d404b2#file-value-speculation-linux-c-L262).

The first three datasets are meant to fit in the L1 / L2 / L3 cache. In those cases, the improvements are very pronounced, and `sum3` is crunching the data at around 4 instructions per second, which should be close to the limit on the processor I tested the code on. When the data does not fit in the cache, the bottleneck becomes filling it, and we process the data at roughly 15 GB/s.

I believe that this is as fast as one can go with "simple" single-threaded reading from RAM,
and it's consistent with data from `sysbench`:

    $ sysbench memory --memory-block-size=1G --memory-oper=read --threads=1 run
    ...
    102400.00 MiB transferred (15089.75 MiB/sec)
    ...

The RAM-reading speed could probably be improved using SIMD streaming instructions or by reading from multiple threads, although the implementation would be significantly more complicated.

And so we complete our journey into this low-level trick! If you want more of this, I can't reccomend [Per's account](https://twitter.com/pervognsen) enough -- figuring out how his tricks works has been very educational.

Thanks to [Alexandru Scvortov](https://scvalex.net/), [Niklas Hambüchen](https://nh2.me/), Alex Appetiti, and [Carter T Schonwald](https://twitter.com/cartazio) for reading drafts of this post. Niklas also clarified some details regarding RAM speeds, and suggested `sysbench` to measure single threaded RAM reading speed in particular. Also thanks to Per Vognsen and Jason Rohem for spotting a few typos, and to [Rihalto](https://twitter.com/RhialtoTheM) for pointing out a better `sum3` and some misleading wording.

## Bonus track -- a compiler friendly C version

[Alexander Monakov suggested](https://twitter.com/_monoid/status/1418663360871141376) a more robust C function which works well with both `gcc` and `clang`, performs as well as `sum3`, and does not resort to any assembly:

```c
uint64_t sum5(Node *node) {
  uint64_t value = 0;
  Node *next = NULL;
  for (; node; node = node->next) {
    for (;;) {
      value += node->value;
      if (node + 1 != node->next) {
        break;
      }
      node++;
    }
  }
  return value;
}
```
