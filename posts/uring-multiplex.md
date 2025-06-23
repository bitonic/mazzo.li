---
title: Waiting for many things at once with `io_uring`
date: 2024-09-22
tags: [post, short]
sidenotes: true
description: When doing systems programming we often need to wait for something to happen. Common examples might be waiting for some data to come through a socket or waiting on a lock. We also often want to wait on any of several conditions to become true. A web server might be handling many sockets at once, waiting for any number of them to become readable or writeable. This short blog post is concerned with the latter scenario in Linux. Until recently there was no generic framework which allowed us to wait on many arbitrary events, but now there is, thanks to `io_uring`.
---

When doing systems programming we often need to wait for something to happen. Common examples might be waiting for some data to come through a socket or waiting on a lock. We also often want to wait on any of several conditions to become true. A web server might be handling many sockets at once, waiting for any number of them to become readable or writeable.

This short blog post is concerned with the latter scenario in Linux. Until recently there was no generic framework which allowed us to wait on many arbitrary events, but now there is, thanks to `io_uring`.

***

The way one usually waits for something to happen is through a system call. For instance:

* [`read()`](https://man.archlinux.org/man/read.2.en) waits until there's data in a file descriptor;
* [`nanosleep()`](https://man.archlinux.org/man/nanosleep.2.en) waits until some time has passed;
* [`FUTEX_WAIT`](https://man.archlinux.org/man/futex.2.en) waits until the value at a certain address changes -- a syscall which can be used to implement most user space concurrency primitives such as locks;
* [`wait()`](https://man.archlinux.org/man/waitid.2.en) waits for a child processes to terminate;
* [`F_SETLKW`](https://man.archlinux.org/man/fcntl.2.en#F_SETLKW) acquires a file lock, waiting if it's currently held by someone else.

This list is by no means exhaustive but it gives an overview of what kind of waiting we might want to do.

In many scenarios we might need to wait on many events at once. We've already mentioned the most common example: handling many file descriptors. In that case we're in luck, since we can use syscalls such as [`epoll_wait`](https://man.archlinux.org/man/epoll_wait.2.en) for that exact purpose.

Moreover many "waiting" syscalls can be phrased in terms of waiting on a file descriptor. Sleeping for a certain time can be done via [`timerfd_create`](https://man.archlinux.org/man/timerfd_create.2.en), signals can be waited on through [`signalfd`](https://man.archlinux.org/man/signalfd4.2.en), and so on.

However some instances of waiting do _not_ fall within the purview of file descriptors. The most notable exception is [`futex()`](https://man.archlinux.org/man/futex.2.en). The futex is the most foundational concurrency primitive offered by the Linux kernel, and it can be used to implement most of the user space concurrency primitives you're already familiar with, such as locks and semaphores.

For instance we might implement a queue using a futex, so that a consumer can read from it, blocking until at least an element is present.

<div>

It's then easy to imagine a situation where a process might want to wait on an element to appear on any of several queues.[^futex2] Or maybe a process is waiting for some sockets to be writeable at the same time as waiting for elements to appear on a queue.

[^futex2]: This specific use case is served by the recent [`futex2` API](https://docs.kernel.org/userspace-api/futex2.html), which still does not help when mixing futex and non-futex waiting.

Sadly waiting on a futex through a file descriptor is not possible.[^futexfd]

[^futexfd]: It _used_ to be possible, from Linux 2.6.0 up to and including Linux 2.6.25, but this functionality was removed, in a rare case of Linux breaking user space programs.

    The problem is that when waiting on a futex through a file descriptor, you can't specify what the previous value is, which makes the users of [`FUTEX_FD`](https://man.archlinux.org/man/futex.2.en#FUTEX_FD) inherently racy since they might get indefinitely blocked due to a missed wakeup.

This is where `io_uring` comes in handy -- not only for futexes but for all cases of wanting to wait on many things at once.

`io_uring` allows us to set up two queues between user space and the kernel: one to ask the kernel for something to be done (the submission queue), and one to get answers (the completion queue). Once this system is established, instead of performing individual syscalls [we just submit a request corresponding to that syscall](https://man.archlinux.org/listing/liburing), and then wait for the answer.

Notably we can submit many things at once, and wait for any of them to complete. The literature on `io_uring` usually focuses on its performance advantages: we can reduce many syscalls to one,[^no-syscalls] and we avoid copying buffers from user space to kernel space since the the relevant buffers when reading or writing are shared between user space and the kernel.

[^no-syscalls]: In fact, syscalls can be eliminated entirely by [setting up a kernel thread which spins on the submission queue to pick up requests](https://man.archlinux.org/man/extra/liburing/io_uring_setup.2.en#IORING_SETUP_SQPOLL), and spinning on the completion queue to pick up responses. This assumes that we can dedicate cores to these tasks, but that's often the case for single-tenant applications, where the latency improvements might be desirable.

    Note that this case is not really what we're concerned with in this blog post. If we're OK with busy looping, we don't really need the kernel's cooperation to implement this kind of multiplexing.

However by this point you might have realized the other advantage: we can submit an arbitrary number of requests and wait on any of them. This essentially allows to multiplex any Linux waiting task which is supported by `io_uring`. Crucially, `io_uring` [recently added support for futexes](https://man.archlinux.org/man/extra/liburing/io_uring_prep_futex_wait.3.en), an important addition to a [very large list of supported functionality](https://man.archlinux.org/listing/liburing).

This should allow for the creation of a generic async framework where one writes code which looks almost identical to code using blocking Linux syscalls, but which instead submits and completes actions through `io_uring`. Concurrency primitives can then be built on top of `futex` and friends, allowing for seamless interaction between IO and user space synchronization.

</div>

## Acknowledgements

Thanks to Peter Cawley for reading drafts of this blog posts.
