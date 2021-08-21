---
title: When `static` makes your C code 10 times faster
date: 2021-07-03
---

_Addendum_: [Many Hacker News commenters](https://news.ycombinator.com/item?id=27729377) correctly point out that the right way to indicate to the compiler that a value does not change is `const`. This post is _not_ about C best practices, but just about how one can easily get to the bottom of this kind of surprising behavior by looking at the compiler output. This example in particular was extracted from a larger piece of code I encountered. That said, I should have noted that the `const` keyword exists and one should use it!

A couple of days ago I ran into a low-level C performance problem. The investigation was brief and satisfying, because inspecting the assembly immediately revealed the issue. So if you have never debugged assembly before, keep reading -- I annotate and explain everything thoroughly.

After some narrowing down, the problem came down to this function:

```c
uint64_t modulus = 1ULL << 31; // 2^31

uint64_t loop(uint64_t N, uint64_t S, uint64_t P, uint64_t Q) {
  for (uint64_t i = 0; i < N; i++) {
    S = (S*P+Q) % modulus;
  }
  return S;
}
```

It loops through the first `N` integers, updating `S` with a multiply-and-add at each step, mod'ing by `modulus`, a global variable.

Making `modulus` static makes the function almost 10 times faster, as shown by this program -- I'm reporting it all for completeness, but all it does is setup the two functions, and time them both with the same user-provided arguments:

```c
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

uint64_t modulus = 1ULL << 31; // 2^31

uint64_t loop(uint64_t N, uint64_t S, uint64_t P, uint64_t Q) {
  for (uint64_t i = 0; i < N; i++) {
    S = (S*P+Q) % modulus;
  }
  return S;
}

static uint64_t modulus_static = 1ULL << 31; // 2^31

uint64_t loop_static(uint64_t N, uint64_t S, uint64_t P, uint64_t Q) {
  for (uint64_t i = 0; i < N; i++) {
    S = (S*P+Q) % modulus_static;
  }
  return S;
}

int main(int argc, char *argv[]) {
  if (argc != 5) { return 1; }

  uint64_t N = strtoll(argv[1], NULL, 10);
  uint64_t S = strtoll(argv[2], NULL, 10);
  uint64_t P = strtoll(argv[3], NULL, 10);
  uint64_t Q = strtoll(argv[4], NULL, 10);
  printf("N: %" PRIu64 ", S: %" PRIu64 ", P: %" PRIu64 ", Q: %" PRIu64 "\n", N, S, P, Q);

  struct timespec begin, end;
  clock_gettime(CLOCK_MONOTONIC_RAW, &begin);
  printf("result (non-static): %" PRIu64 "\n", loop(N, S, P, Q));
  clock_gettime(CLOCK_MONOTONIC_RAW, &end);
  printf(" took %d milliseconds\n", (end.tv_nsec - begin.tv_nsec) / 1000000LL + (end.tv_sec  - begin.tv_sec) * 1000LL);

  clock_gettime(CLOCK_MONOTONIC_RAW, &begin);
  printf("result (static): %" PRIu64 "\n", loop_static(N, S, P, Q));
  clock_gettime(CLOCK_MONOTONIC_RAW, &end);
  printf(" took %d milliseconds\n", (end.tv_nsec - begin.tv_nsec) / 1000000LL + (end.tv_sec  - begin.tv_sec) * 1000LL);

  return 0;
}
```

Which produces this output:

```
% gcc static-number-perf.c -O2 -o static-number-perf
% ./static-number-perf
% ./static-number-perf 100000000 569099406 1607140150 1607140150
N: 100000000, S: 569099406, P: 1607140150, Q: 1607140150
result (non-static): 19433698
 took 992 milliseconds
result (static): 19433698
 took 128 milliseconds
```

That is, `loop_static` is 7.75 times faster than `loop`.

You might want to pause reading and think why this might happen.

I suspected that the problem had to do the fact that the compiler can make less assumptions about what's in a non-static variable, given that other code could concievably access it (more on this later).

Reading the x86-64 assembly for the two functions [on the truly useful `godbolt`](https://godbolt.org/z/Pf3MjrjW8) reveals the issue. First, let's look at the non-static version:

```asm
loop:
        mov     r8, rdi         ; 1st argument (`N`) into r8
        mov     r9, rdx         ; 2rd argument (`P`) into r9
        test    rdi, rdi        ; check if 1st argument (`N`) is 0
        je      .L4             ; if it is, jump to L4 (exit)
        mov     rdi, QWORD PTR modulus[rip] ; `modulus` into rdi
        mov     rdx, rsi        ; `S` into rdx
        xor     esi, esi        ; zero esi (`i`)
.L3:
        imul    rdx, r9         ; multiply rdx (`S`) with r9 (`P`)
        add     rsi, 1          ; increment loop counter (`i`)
        lea     rax, [rdx+rcx]  ; add rdx (`S`) and 4th argument (`Q`) and put them in rax
        xor     edx, edx        ; zero rdx
        div     rdi             ; divides rax (`S`) by rdi (`modulus`), the remainder will be in `rdx`
        cmp     r8, rsi         ; check if we're finished with the loop (`i == N`)
        jne     .L3             ; if we're not, go back
        mov     rax, rdx        ; otherwise store the value in rax (return value) and return
        ret
.L4:
        mov     rdx, rsi        ; move S into rdx
        mov     rax, rdx        ; move rdx into rax (return value)
        ret                     ; exit
```

A very straightforward translation of the C code -- the only trick being using `lea` (a function thought for pointers) to perform addition of numbers with less register shuffling.

Here comes the static version:

```asm
loop_static:
        mov     rax, rsi        ; 2nd argument (`S`) into rax (return value)
        test    rdi, rdi        ; check if 1st argument (`N`) is 0
        je      .L2             ; if it is, exit immediately (`S` is already in rax)
        xor     esi, esi        ; zero out (`i`)
.L3:
        imul    rax, rdx        ; multiply rax (`S`) by the third argument (`P`)
        add     rsi, 1          ; increment loop counter (`i`)
        add     rax, rcx        ; add rax (`S`) to the fourth argument (`Q`)
        and     eax, 2147483647 ; mod eax (`S`) with our number -- this works because it's a power of two!
        cmp     rdi, rsi        ; compare rdi (`N`) to rsi (`i`) to see if we're done
        jne     .L3             ; if they're not equal, exit.
.L2:
        ret                     ; exit
```

When `modulus` is static, `gcc` / `clang` know that it is private to the current compilation unit, and therefore they can inline the value itself. Then, they turn the expensive `div` with a much cheaper `and` -- since mod'ing by a power of two is equal to bitwise and of that number minus one. All you need to do is keep the bits lower than that power of two, which is what the `and` will do.

Note how the original value of modulus was $2^{31}$, that is, $2147483648$, while the constant in the assembly above is $2147483647$. So we go from `10000000000000000000000000000000` to `1111111111111111111111111111111`.

`gcc` / `clang` can't assume much on the value of the non-static version since the resulting object file could be linked with another object injecting code which runs before main, e.g. through the constructor of a global object in C++ or through `__attribute__((constructor))`.

I _think_ they could concievably assume that the value of `modulus` won't be changed in this case, since we're producing an executable directly, but it's probably annoying to have an optimization looking so far into the future of the compiler pipeline.
