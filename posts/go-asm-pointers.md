---
title: How to store Go pointers from assembly
date: 2025-06-23
tags: [post]
sidenotes: true
image: https://mazzo.li/assets/images/go-gc.png
description: The standard Go toolchain comes with an assembler out of the box. Said assembler is highly idiosyncratic, using syntax inherited from Plan 9 and choosing its own names for platform-specific instructions and registers. But it's great to have it readily available. More mundanely, Go comes with a garbage collector. This post explains how to make these two components play nice, if we want to manipulate Go pointers from our assembly.
---

The standard Go toolchain comes with [an assembler](https://go.dev/doc/asm) out of the box. Said assembler is highly idiosyncratic, using syntax inherited from Plan 9 and choosing its own names for platform-specific instructions and registers. But it's great to have it readily available.

More mundanely, Go comes with a garbage collector. This post explains how to make these two components play nice, if we want to manipulate Go pointers from our assembly.

## Preamble: Go's GC write barriers

Go's garbage collector strives to minimize long pauses, which calls for concurrent garbage collection: the garbage is picked up while your code is running. Even if you're not a garbage collection expert you would have recognized this as a tricky problem. As the Go GC is marking reachable objects, new objects might become reachable due to code running alongside the GC.

<div>

A common technique deployed to address this problem consists of instrumenting all pointer stores to inform the GC that the destination is now being pointed to.[^stores] This instrumentation will augment all assignments with a bit of code informing the GC of the new reference. Or more concretely code like

[^stores]: With "pointer stores" we mean all assignments such as `x = y` where `y` contains pointers. For simplicity we're just going to consider cases where `y` is a single pointer.

```go
x = y
```

will become more like

```go
add_to_gc_queue(y)
x = y
```

</div>
<div>
Where `add_to_gc_queue(y)` makes it so that `y` will be picked up by the GC even if `x` had already been examined. The widget above is often called a "write barrier" in the context of garbage collection.[^synch]

[^synch]: Not to be confused with many other barriers used in various kinds of concurrent programming.

As you can imagine this instrumentation has a cost, a cost that is particularly dear when it comes to the very common task of storing pointers in stack variables. So Go chooses to _not_ instrument stores where the receiver of the store is on the stack. Instead until Go 1.8, the GC stopped the world before collection, taking care to re-scan stacks for goroutines which had run after the time they had been first examined. This ensured that no new stack references would ... go undetected.

</div>

This final stack rescanning procedure could often take uncomfortably long, and therefore Go switched to a broader write barrier, which roughly consists of adding both the old and the new reference to the GC queue:

```go
add_to_gc_queue(*x)
add_to_gc_queue(y)
*x = y
```

Above we have `x` to be a pointer to a pointer to highlight that the receiver of the pointer itself lives on the heap, rather than on the stack.

[The details are fiddly](https://github.com/golang/proposal/blob/master/design/17503-eliminate-rescan.md), but involving both the old and new pointer let the Go developers remove the final stack-scanning, decreasing the duration of stop-the-world pauses by two orders of magnitude.

## The problem

<div>

When the Go compiler generates code, it automatically instruments all the relevant stores. However, fun awaits when we want to perform some pointer stores ourselves.

As a motivating example, consider a concurrent hash table storing key-value pairs together:

```go
type slot struct {
  key   uint64
  value *entry
}
```

We might want to leverage 128-bit atomic load/stores -- which are not available directly in Go -- by writing our get/put functions in assembly.[^atomic] However we face a challenge: when writing to a `slot` from assembly, we need to inform Go's GC of the store that just happened, or else pay the cost of rare and hard-to-debug faults, given that we're breaking invariants central to the correctness of Go's garbage collection.

[^atomic]: As a side remark, 128-bit atomics are underappreciated. They have the curious property of having become recently available on x86_64 CPUs going back more than 10 years, given that Intel and AMD [retroactively guaranteed aligned 128-bit loads and stores to be atomic on all AVX CPUs.](https://stackoverflow.com/a/7647825)

    128-bit atomics are also available on all aarch64 CPUs supporting [`FEAT_LRCPC`, introduced in ARM v8.4.](https://github.com/taiki-e/portable-atomic/issues/10#issuecomment-1113930266)

    Given their usefulness (such as the example cited in this section), I'd expect to see them gain wide adoption in concurrent data structures soon.

In practice, the assembly for a store not happening on the stack [looks like this:](https://godbolt.org/z/nbqeo3rGv)

</div>

```
  // We're storing the pointer contained in BX into the memory
  // pointed to by AX:
  //
  //     *AX = BX
  //
  // First, check if GC is currently active, i.e. if we need to
  // even bother informing the GC of our store.
  CMPL    runtime.writeBarrier(SB), $0
  // If it is _not_ active, just jump to the store
  // directly.
  JEQ     doStore
  // Otherwise, ask the GC to give us space for two pointers
  // in the store buffer, the data structure recording stores
  // happening while GC is taking place. `gcWriteBarrier2`
  // returns where to store the pointers in R11.
  CALL    runtime.gcWriteBarrier2(SB)
  // Move the new pointer into the first location.
  MOVQ    BX, (R11)
  // Move the previous pointer *AX in the second location.
  MOVQ    (AX), CX
  MOVQ    CX, 8(R11)
doStore:
  // *AX = BX
  MOVQ    BX, (AX)
```

All we need to do is replicate this widget in our own assembly, replacing the scalar store above with our wider store for our concurrent hash map.

The problem is that somewhat understandably the Go designers really don't want you to do that, and they therefore started [forbidding linking to runtime symbols.](https://github.com/golang/go/issues/67401) This is to avoid having libraries relying on runtime internals, and being henceforth unable to change them.

Fortunately, they [whitelisted functions which were previously referenced by popular Go packages](https://github.com/golang/go/blob/49cdf0c42e320dfed044baa551610f081eafb781/src/runtime/stubs.go#L407-L416), and it just so happened that a [single package](https://github.com/bytedance/sonic/blob/de4f017fca6448580003b6cc661bed8fded68d1d/internal/rt/gcwb.go#L28-L32) used exactly the symbols we need.

To link to them, you must blacklist future Go versions:

```go
//go:build go1.21 && !go1.26
package foo

//go:linkname gcWriteBarrier2 runtime.gcWriteBarrier2
func gcWriteBarrier2()

//go:linkname runtimeWriteBarrier runtime.writeBarrier
var runtimeWriteBarrier uintptr
```

Presumably the plan is to eventually completely phase out these functions too. But in the meantime, you can hand-craft write barriers with abandon.

## Bonus: allocating aligned memory in Go

While investigating the above, a more fundamental problem arose: allocating our `slot`s to be 128-bit aligned, to be able to use AVX instructions on them. Go makes this surprisingly tricky. Allocating a `[]byte` and using [`unsafe.Slice()`](https://pkg.go.dev/unsafe#Slice) to create an aligned `[]slot` from it is doomed to fail, since the Go runtime identifies allocated regions by their original type, and therefore will be blind to the pointers in our array of slots.

Regrettably, there seem to be no "blessed" ways of doing this, but after a few experiments [Peter Cawley](https://www.corsix.org/) produced this devious bit of code to bend Go's malloc to our will:

```go
type slot struct {
  key   uint64
  value *entry
}

type slotShifted struct {
  entry *entry
  key   uint64
}

// Allocate slots so that they're 16-byte aligned.
func allocateSlots(capacity int) (slots []slot) {
  candidate := make([]slot, capacity)
  if (uintptr(unsafe.Pointer(&candidate[0])) & 15) == 0 {
    // This is exactly what we want.
    slots = candidate
    return
  }
  candidateShifted := make([]slotShifted, capacity+1)
  if (uintptr(unsafe.Pointer(&candidateShifted[0])) & 15) == 8 {
    // This can be made to work; throw away the first 8 bytes and
    // last 8 bytes of candidateShifted.
    slots = unsafe.Slice((*slot)(unsafe.Pointer(uintptr(unsafe.Pointer(&candidateShifted[0]))+8)), capacity)
    return
  }
  // This can happen if adding 1 to capacity pushes us up a size class,
  // and the new size class has a malloc header whereas the old size
  // class does not.
  candidate = make([]slot, capacity+1)
  if (uintptr(unsafe.Pointer(&candidate[0])) & 15) == 0 {
    // This is what we want, albeit one element too big, so throw
    // away the extra element.
    slots = candidate[0:capacity:capacity]
    return
  }
  panic("could not allocate slots")
}
```

Note that while the runtime tracks where pointers are based on Go's type system, it does not care about the precise source of pointers, making the above trick viable. There are surely other ways to do this, but the above is possibly the cutest.

## Acknowledgements

Many thanks to everybody I nerd-sniped by bringing up this topic, and to [Peter Cawley](https://www.corsix.org/) and Samir Jindel in particular.