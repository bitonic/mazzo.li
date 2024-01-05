---
title: How to stop Linux threads cleanly
date: 2024-01-07
tags: [post]
sidenotes: true
description: Stopping a Linux thread is a surprisingly annoying affair. In this post I present common pitfalls and some solutions -- although no truly satisfactory method exists.
image: https://mazzo.li/assets/images/stopping-linux-threads.png
---

<div>

Let's say you're writing a long running multi-threaded application,  on Linux. Maybe it's a database or a server of some sort. Let's also imagine that you're not running on some managed runtime (maybe the JVM, Go, or BEAM), but rather managing threads spawned using the [`clone`](https://man7.org/linux/man-pages/man2/clone.2.html) syscall. Think of threads created in C with [`pthread_create`](https://man7.org/linux/man-pages/man3/pthread_create.3.html), or using C++'s [`std::thread`](https://en.cppreference.com/w/cpp/thread/thread/thread).[^processes]

[^processes]: Most content in this blog post applies to processes too -- in Linux the only real difference between processes and threads is that threads share virtual memory.

    Most of the content will also apply to any language beyond C and C++ which uses Linux threads directly, such as Rust's [`thread::spawn`](https://doc.rust-lang.org/std/thread/fn.spawn.html) and zig's `std::Thread::spawn`.

Once you get into the business of starting threads, you're probably also in the business of stopping them. However the former is much easier than the latter. With "stopping" we mean stopping the thread while giving it a chance to run some cleanup operations before fully terminating. Or in other words, we want to terminate a thread while ensuring that memory is freed, locks are released, logs are flushed, and so on.[^cleanup]

[^cleanup]: In C++, cleanup will probably be done by way of destructors, in which case the goal becomes to stop a thread running all outstanding destructors before the thread terminates.

    Shutting down a thread _without_ cleanup is easily done with [`pthread_kill(tid, SIGKILL)`](https://man7.org/linux/man-pages/man3/pthread_exit.3.html).

This task is sadly not as straightforward as it should be, and there definitely isn't a one-size-fits-all solution. This blog post aims to give an overview of the problem space and to highlight some pitfalls in an area with no shortage, and present [a little magic trick at the end.](#rseq)

</div>

## (Quasi-)busy looping

If you can afford it, you can structure each thread as such:

```c
while (true) {
  if (stop) { break; }
  // Perform some work completing in a reasonable time
}
```

`stop` here is a per-thread boolean. When we want to stop a thread, we set `stop` to `true`, and then call [`pthread_join`](https://man7.org/linux/man-pages/man3/pthread_join.3.html) or equivalents to ensure that the thread has actually terminated.

Here's a contrived but working example in C++:

```cpp
#include <thread>
#include <atomic>
#include <stdio.h>
#include <unistd.h>

static std::atomic<bool> stop = false;

int main() {
  std::thread thr([] {
    // prints every second until stopped
    for (int i = 0; !stop.load(); i++) {
      printf("iterated %d times\n", i);
      sleep(1);
    }
    printf("thread terminating\n");
  });
  // waits 5 seconds, then stops thread
  sleep(5);
  stop.store(true);
  thr.join();
  printf("thread terminated\n");
  return 0;
}
```

<div>

Which prints:

<div>

```
iterated 0 times
iterated 1 times
iterated 2 times
iterated 3 times
iterated 4 times
thread terminating
thread terminated
```

</div>

If you can write or refactor your code to work in such time slices, then terminating threads is very easy.

Note that the loop body does not need to be fully non blocking -- it just needs to be terminated as quickly as we want our termination to be quick. For instance, if our thread is reading from a socket, we could set `SO_TIMEOUT` to be 100 milliseconds so that we know that every iteration of the loop will terminate quickly.[^non-blocking]

[^non-blocking]: If you write all code in a full non-blocking runtime you probably end up where many runtimes end, having some coroutine abstraction to express operations that might need to wait and performing all the scheduling yourself.

    You probably have a readily available framework for this approach in your language of choice already ([Seastar](https://seastar.io/) in C++, [async Rust](https://rust-lang.github.io/async-book/), and so on).

    That approach has a lot of merit, but it requires structuring your entire application following a given framework. In this post we're concerned with programs written using Linux threads directly, with the Kernel scheduling them.

</div>

## What if I want to block forever?

<div>

Quasi-busy loops are all well and good, but they're sometimes not desirable. The most common roadblock is foreign code that we don't control which does not fit this pattern -- think of a third-party library doing some blocking network call.

As we'll see later, there's essentially no clean way to stop a thread running code we don't control, but there are other reasons to not want to write all our code with the quasi-busy loop pattern.

If we have many threads even relatively slow timeouts might cause significant scheduling overhead due to spurious wakeups, especially on an already busy system. The timeouts will also make debugging and inspecting the system considerably more annoying (e.g. imagine what the output of `strace` would look like).

So it is worth thinking about how to stop a thread while it is blocked on a syscall. The most straightforward way to do that is through signals.[^signals-care]

[^signals-care]: Note that even if you use quasi-busy loops (and really if you're writing any software) you probably need to care about signals.

    For instance any application which uses buffered prints (say with `printf`) is liable to lose output if it gets stopped by a signal and it does not have signal handlers installed to flush the stdio buffers.

</div>

## We need to talk about signals

<div>

Signals are the main way to interrupt execution of a thread without explicit coordination of the interrupted thread, and are therefore very relevant to the topic of this blog post. They're also a bit of a mess. These two facts generate unhappiness.

For a good overview on signals I recommend the surprisingly informative [man page](https://man7.org/linux/man-pages/man7/signal.7.html), but I'll give a sufficient overview here. If you already know how signals work,[^tired] you can skip to [the next section](#thread-cancellation).

[^tired]: Congratulations, you must be tired.

Signals can arise because of some hardware exception[^hw-signal] or be initiated by software. The most familiar instance of a software-initiated signal is your shell sending SIGINT to the foreground process when you press `ctrl-c`. All signals initiated by software originate from a handful of syscalls -- for instance [`pthread_kill`](https://man7.org/linux/man-pages/man3/pthread_kill.3.html) will send a signal to a thread.[^thread-process-directed]

[^hw-signal]: Dividing by zero might generate SIGFPE, trying to access unmapped memory will generate SIGSEGV, and so on.

[^thread-process-directed]: Signals can be thread-directed (e.g. through `pthread_kill`), or process directed (e.g. through [`kill`](https://man7.org/linux/man-pages/man2/kill.2.html)). A thread-directed signal will be delivered to the thread. When a process-directed signal is sent, the kernel picks a thread in the process to handle it, without any guarantees on which thread will be picked.

Hardware initiated signals are generally handled immediately, while software initiated signals are handled when a CPU is about to re-enter user mode after the kernel has done some work.[^user-mode] In any event, when a signal needs to handled in a given thread:

[^user-mode]: Generally speaking this will happen in two instances: when a system call has finished executing, or when the kernel is scheduling a thread.

1. If the signal has been blocked by the receiving thread, it'll wait to be handled until it is unblocked;

2. If the signal is not blocked, it might be:

    a. ignored;
    b. handled in the "default" manner;
    c. handled using some custom signal handler.

</div>

Which signals are blocked is controlled by modifying the _signal mask_ using [`sigprocmask`](https://man7.org/linux/man-pages/man2/sigprocmask.2.html)/[`pthread_sigmask`](https://man7.org/linux/man-pages/man3/pthread_sigmask.3.html), and which action is taken if the thread is not blocked is controlled by [`sigaction`](https://man7.org/linux/man-pages/man2/sigaction.2.html).

Assuming that the signal is _not_ blocked, paths 2.a and 2.b will be managed entirely by the kernel, while path 2.c will cause the kernel to pass control to a user-space signal handler which will do something with the signal.

Importantly, if some thread is in a syscall (for instance blocked while reading from a socket), and a signal needs to be handled, the syscall will return early with error code `EINTR` after the signal handler has run.

The signal handler code is subject [to various constraints](https://man7.org/linux/man-pages/man7/signal-safety.7.html), but otherwise it can do as it pleases, including deciding to not give back control to the code that was executing before. By default, most signals just cause the program to stop abruptly, possibly with a core dump. In the next few sections we're going to explore various ways to use signals to stop our threads.

## Thread cancellation, a false hope {#thread-cancellation}

Let's first examine a way to stop threads, implemented through signals, which would seem to do exactly what we want: thread cancellation.

The API for thread cancellation is very promising. [`pthread_cancel(tid)`](https://man7.org/linux/man-pages/man3/pthread_cancel.3.html) will "cancel" thread `tid`. The way `pthread_cancel` works boils down to:

1. A special signal is sent to thread `tid`;
2. The libc you're using (say [glibc](https://en.wikipedia.org/wiki/Glibc) or [musl](https://en.wikipedia.org/wiki/Musl)) sets up a handler so that when the cancel signal is received the thread winds down.

There are additional details, but that's essentially all there is to it. However, trouble lies ahead.

### Resource management + thread cancellation = 😢

Recall that signals can essentially arise anywhere in your code. So if we have code such as

<div>
```c
lock();
// critical work here
unlock();
```
</div>

we might get a signal in the critical section. In the case of thread cancellation, our thread might get cancelled while we're holding a lock as above, or with some memory to be freed, or in general with some outstanding resource, and our cleanup code will never run. This is not good.

There are some mitigating circumstances, although none sufficient:

* Thread cancellation can be temporarily disabled. So we could disable it any time we are in such a critical section.

    However some "critical sections" are very long (consider the lifespan of some allocated memory), and moreover we'd have to make sure to decorate all relevant code by enabling/disabling cancellation at the right time.

* Linux threads include facilities to add/remove global cleanup handlers with [`pthread_cleanup_push`](https://man7.org/linux/man-pages/man3/pthread_cleanup_push.3.html) and `pthread_cleanup_pop`. These cleanup handlers _are_ run when a thread is cancelled.

    However to ensure safety using these functions one would have to again decorate every critical section with not only with a push/pop, but also temporarily disabling cancellations to avoid races as we setup the cleanup.

    Again, this would be very error prone and would slow down our code considerably.

* By default the signal sent by thread cancellation is only received at "cancellation points", which to a first approximation are the syscalls that might block -- see [`pthreads(7)`](https://man7.org/linux/man-pages/man7/pthreads.7.html).

    So really we would only run into trouble if we have such a syscall in the critical sections. But again, we'd have to manually ensure that either critical section have no cancellation points, or that they're made safe otherwise (possibly with the two measures described above).

### Thread cancellation is incompatible with modern C++

<div>

If you're a C++/Rust programmer, you might have sneered at the explicit locking above -- you've got RAII to handle such cases:

<div>
```cpp
{
  const std::lock_guard<std::mutex> lock(mutex);
  // critical work here
  // The destructor for `lock` will do the unlocking
}
```
</div>

You might have also been wondering what happens if a thread cancellation arrives in the RAII-managed critical section here.

The answer is that thread cancellation will trigger a stack unwinding very much like throwing an exception would (in fact it's implemented with a special exception), which means that destructors _will_ be run on cancellation. This mechanism is known as _forced unwinding_. Great, right?[^musl-cancellation]

[^musl-cancellation]: Note that this unwinding is not mandated by any standard, and is really a glibc/libstdc++ feature. For instance if you use musl no unwinding will be performed, and destructors won't run.

    There's also [other weirdness](https://udrepper.livejournal.com/21541.html) to be aware of when relying on this unwinding.

Well, since thread cancellation is implemented using exceptions, and thread cancellation can happen in arbitrary places, [we're always liable to a cancellation happening in a `noexcept` block](https://gcc.gnu.org/legacy-ml/gcc/2017-08/msg00121.html), which is undefined behavior, and which in practice will cause your program to crash.[^sigabrt]

[^sigabrt]: Ironically this is in turn done with a signal -- SIGABRT will be raised, which by default will result in the process terminating with a core dump.

So since C++11, and especially since C++14 where destructors are marked as `noexcept` by default, thread cancellation is essentially useless in C++.[^c-attribute-cleanup]

[^c-attribute-cleanup]: When working with C, rather than C++, one could perform cleanup using [`__attribute__((cleanup))`](https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Common-Variable-Attributes.html#index-cleanup-variable-attribute). This would 
work very much like destructors, but without the `noexcept` troubles.

    This would work rather well in theory, but in practice, that's just not how C code is written, so we'd be swimming against the tide if we wanted to write a C project entirely in this style. Moreover this approach is flawed anyhow, as I explain in [the next subsection](#forced-unwinding-is-unsafe-anyway).

</div>

### Forced unwinding is unsafe anyway

<div>

However note that even if this mechanism worked in C++, it'd still not be safe in many situations. Consider situations like:

```cpp
{
  const std::lock_guard<std::mutex> lock(mutex);
  balance_1 += x;
  balance_2 -= x;
}
```

If we get forcefully unwound after `balance_1 += x`, our invariants go out of the window. This is why Java's form of forced unwinding, [`Thread.stop`](https://docs.oracle.com/javase/8/docs/technotes/guides/concurrency/threadPrimitiveDeprecation.html), was deprecated.[^rust-poison]

[^rust-poison]: Rust poisons locks that were held during a panic, which would at least help preserve safety (but not correct functioning of our program) in cases such as these, assuming that something like thread cancellation was implemented in the Rust runtime as equivalent to a panic.

</div>

## You can't cleanly stop threads running code you don't control

As a brief aside, the nature of signals (and by extension thread cancellation) implies that it's impossible to cleanly stop code that you don't control. You cannot guarantee that memory isn't leaked, files are closed, global locks are released, and so on.

If you need to interrupt foreign code reliably, it's better to isolate it in its own process. It might still leak temporary files and other such persistent resources, but most relevant state would be cleaned up by the operating system when the process dies.

## Controlled thread cancellation

<div>

Hopefully you're now convinced that unrestricted thread cancellation is not a great idea in most circumstances. However we can pick the circumstances explicitly by enabling thread cancellation only at specific times. So our event loop becomes:

```c
pthread_setcancelstate(PTHREAD_CANCEL_DISABLE);
while (true) {
  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE);
  // syscall that might block indefinitely, e.g. reading
  // from a socket
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE);
  // Perform some work completing in a reasonable time
}
```

We turn off thread cancellation by default, but turn it back on as we do our blocking syscall.[^many-syscalls]

[^many-syscalls]: Note that we can have as many syscalls that might block indefinitely as we like in our loop, as long as they run with cancellation enabled, and as long as we have a section with cancellation enabled and a cancellation point at least once in each loop iteration.

Refactoring our code to fit this pattern might seem onerous. However many applications with long lived threads already contain loops with a blocking syscall at the beginning (reading from a socket, sleeping on a timer, and so on), followed by some work that will not block indefinitely.

</div>

## Homegrown thread cancellation

<div>

However once we've done this, it might be worth getting rid of thread cancellation entirely. Relying on the stack unwinding to free resources would not be portable to alternative libcs, and we'd need to be [fairly careful](https://udrepper.livejournal.com/21541.html) if we wanted to perform some explicit cleanup actions outside destructors.

So instead we can work with signals directly. We can pick SIGUSR1 as our "stopping" signal, install a handler which sets our stopping variable, and check the variable before doing blocking syscalls.[^usr1]

[^usr1]: USR1 is convenient since it's not used for anything by default, so we can use it for our purposes. We could then also block SIGINT/SIGTERM, and only enable them on the main thread, which would then coordinate teardown of the children.

[Here's a worked out example in C++.](https://gist.github.com/bitonic/d3281b2d0fd95b4fd788aa7e013d1fb9/7f5a705198f5f9c3c250d24ec085bb75796a4752) The interesting parts of the code are setting up the signal handler:

</div>

```cpp
// thread_local isn't really necessary here with one thread,
// but it would be necessary if we had many threads we wanted
// to kill separately.
static thread_local std::atomic<bool> stop = false;

static void stop_thread_handler(int signum) {
  stop.store(true);
}

int main() {
  // install signal handler
  {
    struct sigaction act = {{ 0 }};
    act.sa_handler = &stop_thread_handler;
    if (sigaction(SIGUSR1, &act, nullptr) < 0) {
      die_syscall("sigaction");
    }
  }
  ...
```

And the code checking the flag before running the syscall:

```cpp
ssize_t recvlen;
if (stop.load()) {
  break;
} else {
  recvlen = recvfrom(sock, buffer.data(), buffer.size(), 0, nullptr, nullptr);
}
if (recvlen < 0 && errno == EINTR && stop.load()) {
  // we got the signal while running the syscall
  break;
}
```

However, the code checking the flag and starting the syscall is racy:

```cpp
if (stop.load()) {
  break;
} else {
  // signal handler runs here, syscall blocks until
  // packet arrives -- no prompt termination!
  recvlen = recvfrom(sock, buffer.data(), buffer.size(), 0, nullptr, nullptr);
}
```

<div>

There's no easy way to check the flag and run the syscall atomically.[^rseq]

[^rseq]: However there is a hard way to do this, as we'll see in the [last section](#rseq).

[Another approach to this problem](https://gist.github.com/bitonic/d3281b2d0fd95b4fd788aa7e013d1fb9/139cc58cbdbba88ef311f7ea1b47a06050f03016) would be to have USR1 blocked normally, and unblock it only when the syscall runs, similarly to what we did with the temporary thread cancellation. If the syscall terminates with `EINTR`, we know that we should quit.[^stop-variable]

[^stop-variable]: It would be good practice to still have a stop variable which is set in the handler, to be able to distinguish interruptions due to our handler being run and other interruptions. I'm omitting it in the code for brevity.

Sadly the race is still there, just between the unblocking and running the syscall:

</div>

```cpp
ptread_sigmask(SIG_SETMASK, &unblock_usr1); // unblock USR1
// signal handler runs here, syscall blocks until
// packet arrives -- no prompt termination!
ssize_t recvlen = recvfrom(sock, buffer.data(), buffer.size(), 0, nullptr, nullptr);
ptread_sigmask(SIG_SETMASK, &block_usr1); // block USR1 again
```

### Changing the sigmask atomically

<div>

However, there often _is_ an easy to atomically change the sigmask and run a syscall:

* [`select`](https://man7.org/linux/man-pages/man2/select.2.html)/[`poll`](https://man7.org/linux/man-pages/man2/poll.2.html)/[`epoll_wait`](https://man7.org/linux/man-pages/man2/epoll_wait.2.html) have `pselect`/`ppoll`/`epoll_pwait` variants which take a `sigmask` argument;
* `read`/`write` and similar syscalls can be replaced by their non-blocking versions and a blocking `ppoll`;
* To sleep one can use [`timerfd`](https://man7.org/linux/man-pages/man2/timerfd_create.2.html) or just `ppoll` with no file descriptors but with a timeout;
* The newly added [`io_uring_enter`](https://man7.org/linux/man-pages/man2/io_uring_enter.2.html) supports this use case out of the box.

The syscalls above already cover a very large footprint.[^eventfd]

[^eventfd]: Note that if we are able to rely exclusively on file descriptor based syscalls, we can do away with signals entirely, and use [`eventfd`](https://man7.org/linux/man-pages/man2/eventfd.2.html) to signal that we should terminate.

</div>

In this style, the [receive loop of the program becomes](https://gist.github.com/bitonic/d3281b2d0fd95b4fd788aa7e013d1fb9/9bd6b18f6329a0b07cba5cb38d3c45bbf27ae968):

```cpp
struct pollfd pollsock = {
  .fd = sock,
  .events = POLLIN,
};
if (ppoll(&pollsock, 1, nullptr, &usr1_unmasked) < 0) {
  if (errno == EINTR) {
    break;
  }
  die_syscall("ppoll");
}
ssize_t recvlen = recvfrom(sock, buffer.data(), buffer.size(), 0, nullptr, nullptr);
```

### Making it work with any syscall {#rseq}

<div>

Sadly, not all syscalls have variants which let us atomically change the sigmask as they execute. [`futex`](https://man7.org/linux/man-pages/man2/futex.2.html), the main syscall used to implement userspace concurrency primitives, is a notable example of a syscall which does _not_ include such a facility.[^futex-fd]

[^futex-fd]: Interestingly, `FUTEX_FD` would allow us to use a futex with `ppoll`, but `FUTEX_FD` was removed in Linux 2.6.25 due to its raciness, in a rare case of Linux breaking a user-facing API.

In the case of `futex` one can interrupt threads through `FUTEX_WAKE`, but it turns out we can setup a mechanism to safely check the boolean stop flag atomically with starting _any_ syscall.[^pete]

[^pete]: The idea for this trick is due to Peter Cawley.

To recap, the problematic code [looks like this](https://gist.github.com/bitonic/d3281b2d0fd95b4fd788aa7e013d1fb9/7f5a705198f5f9c3c250d24ec085bb75796a4752):

</div>

```cpp
if (stop.load()) {
  break;
} else {
  // signal handler runs here, syscall gets stuck
  recvlen = recvfrom(sock, buffer.data(), buffer.size(), 0, nullptr, nullptr);
}
```

If we could know that no signal handler is ran between the flag check and the syscall, then we'd be safe.

<div>

Linux 4.18 introduced a syscall, [`rseq`](/assets/other/rseq.html) ("restartable sequences"), which lets us achieve this, although with some effort.[^rseq-doc] The `rseq` machinery works as follows:

[^rseq-doc]: Documentation on `rseq` is still rather scant, but you can find a decent intruduction with a minimal example [here](https://www.efficios.com/blog/2019/02/08/linux-restartable-sequences/).

* You write some code which you want to run atomically with regards to preemption or signals -- the critical section.

* Before the critical section is entered, we inform the kernel that the critical section is about to run by writing to a bit of memory shared between the kernel and userspace.

* This bit of memory contains:

    a. `start_ip`, the instruction pointer which marks the begin of the critical section;
    b. `post_commit_offset`, the length of the critical section;
    c. `abort_ip`, the instruction pointer to jump to if the kernel needs to preempt the critical section.

* If the kernel has preempted a thread, or if a signal needs to be delivered to the thread, it checks if the thread is in a `rseq` critical section, and if it does sets the program counter for the thread to `abort_ip`.

</div>

The process above forces the critical section to be a single contiguous block (from `start_ip` to `start_ip+post_commit_offset`) which we must know the address of. These requirements force us to write it in inline assembly.

<div>

Note that rather than disabling preemption entirely, `rseq` lets us specify some code (the code starting at `abort_ip`) to perform some cleanup if the critical section is interrupted. The correct functioning of the critical section therefore often depends on a "commit instruction" at the very end of the critical section which makes the changes in the critical section visible.

In our case the "commit instruction" is `syscall` -- the instruction which will invoke the syscall that we're interested in.[^no-syscalls]

[^no-syscalls]: Note that a `rseq` critical sections cannot contain syscalls. However, if the very last instruction of the critical section is a syscall, we can never have the thread being in a syscall and in a critical section a the same time: once the `syscall` instruction runs and the syscall proper starts the program counter is already past the critical section.

Which leads us to the following widget for a 6-argument syscall stub which atomically checks a stop flag and executes a `syscall`:

</div>

```cpp
// We only provide a wrapper for 6-argument syscalls here,
// one would have to have wrappers for 1 to 6 arguments --
// would be very easy to do so since the only thing that
// changes is the parameter preparation.
//
// Returns -1 and sets errno to EINTR if `*stop` was true
// before starting the syscall.
long syscall_or_stop6(bool* stop, long n, long a, long b, long c, long d, long e, long f) {
  long ret;
  register long rd __asm__("r10") = d;
  register long re __asm__("r8")  = e;
  register long rf __asm__("r9")  = f;
  __asm__ __volatile__ (
    R"(
      # struct rseq_cs {
      #     __u32   version;
      #     __u32   flags;
      #     __u64   start_ip;
      #     __u64   post_commit_offset;
      #     __u64   abort_ip;
      # } __attribute__((aligned(32)));
      .pushsection __rseq_cs, "aw"
      .balign 32
      1:
      .long 0, 0                # version, flags
      .quad 3f, (4f-3f), 2f     # start_ip, post_commit_offset, abort_ip
      .popsection

      .pushsection __rseq_failure, "ax"
      # sneak in the signature before abort section as
      # `ud1 <sig>(%%rip), %%edi`, so that objdump will print it
      .byte 0x0f, 0xb9, 0x3d
      .long 0x53053053
      2:
      # exit with EINTR
      jmp 5f
      .popsection

      # we set rseq->rseq_cs to our structure above.
      # rseq = thread pointer (that is fs) + __rseq_offset
      # rseq_cs is at offset 8
      leaq 1b(%%rip), %%r12
      movq %%r12, %%fs:8(%[rseq_offset])
      3:
      # critical section start -- check if we should stop
      # and if yes skip the syscall
      testb $255, %[stop]
      jnz 5f
      syscall
      # it's important that syscall is the very last thing we do before
      # exiting the critical section to respect the rseq contract of
      # "no syscalls".
      4:
      jmp 6f

      5:
      movq $-4, %%rax # EINTR

      6:
    )"
    : "=a" (ret) // the output goes in rax
    : [stop] "m" (*stop),
      [rseq_offset] "r" (__rseq_offset),
      "a"(n), "D"(a), "S"(b), "d"(c), "r"(rd), "r"(re), "r"(rf)
    : "cc", "memory", "rcx", "r11", "r12"
  );
  if (ret < 0 && ret > -4096) {
    errno = -ret;
    ret = -1;
  }
  return ret;
}

// A version of recvfrom which atomically checks
// the flag before running.
static long recvfrom_or_stop(bool* stop, int socket, void* buffer, size_t length) {
  return syscall_or_stop6(stop, __NR_recvfrom, socket, (long)buffer, length, 0, 0, 0);
}
```

We're using glibc's [recently added support for `rseq`](https://lwn.net/Articles/883104/), which provides a `__rseq_offset` variable containing the offset where the critical section information lives, relative to the thread pointer. All we need to do in the critical section is check the flag, skip the syscall if it's set, and perform the syscall if it is. If the flag is set we pretend the syscall has failed with `EINTR`.

You can find the full code for the previous example using this trick to call `recvfrom` [here](https://gist.github.com/bitonic/d3281b2d0fd95b4fd788aa7e013d1fb9). I'm not necessarily advocating the use of this technique, but it's definitely an interesting curiosity.

## Wrapping up

It's quite frustrating that there's no agreed upon way to interrupt and stack unwind a Linux thread and to protect critical sections from such unwinding. There are no technical obstacles to such facilities existing, but clean teardown is often a neglected part of software.

Haskell is one language where these capabilities do exist in the form of [asynchronous exceptions](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#v:throwTo), although one still needs to be careful to protect critical sections appropriately.

## Acknowledgements

[Peter Cawley](https://twitter.com/corsix) provided input on many details treated in this blog post and read its draft. He also suggested `rseq` as a possible solution. Many thanks also go to [Niklas Hambüchen](https://nh2.me/), [Alexandru Sçvortov](https://scvalex.net/), [Alex Sayers](https://www.asayers.com/), and Alex Appetiti for reading drafts of this blog post.