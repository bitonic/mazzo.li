---
title: Reliably allocating huge pages in Linux
date: 2021-11-22
tags: [post, short]
description: Let's say that you have a program which relies on huge pages for performance. I couldn’t find a resource fully explaining how to allocate huge pages at runtime, making sure that the huge page allocation was successful, so here it is.
---

Let's say that you have a program which relies on [huge pages](https://www.kernel.org/doc/Documentation/vm/hugetlbpage.txt) for performance. I couldn't find a resource fully explaining how to allocate huge pages at runtime, making sure that the huge page allocation was successful, so here it is.

High level steps (or [skip to the code](#code)):

1. Make sure that [transparent huge pages](https://www.kernel.org/doc/Documentation/admin-guide/mm/transhuge.rst) are enabled:[^kernel-transp]

    ```
    % cat /sys/kernel/mm/transparent_hugepage/enabled
    always [madvise] never
    ```

    `madvise` or `always` are what we want.

2. Run the program where you want to perform this check as root.[^root]

3. Allocate memory using [`aligned_alloc` or `posix_memalign`](https://man7.org/linux/man-pages/man3/posix_memalign.3.html), with a 2MiB alignment --- the huge page size. Linux also supports 1GiB huge pages on some systems, but here we'll be working with 2MiB pages:[^madvise][^mmap]

    ```c
    void* buf = aligned_alloc(1 << 21, size);
    ```

4. Instruct the kernel to allocate the page using a huge pages with [`madvise`](https://man7.org/linux/man-pages/man2/madvise.2.html):

    ```c
    madvise(buf, size, MADV_HUGEPAGE)
    ```

    It is important to issue this command before the page is allocated (next step). Also, this step is not needed if transparent huge pages are set to `always`.

5. For each 2MiB chunk in your buffer:

    * Allocate the page backing your buffer --- setting the first byte for each page would be enough:

        ```c
        memset(buf, 0, 1);
        ```

    * Get the page frame number (PFN) by reading [`/proc/self/pagemap`](https://www.kernel.org/doc/Documentation/vm/pagemap.txt).

    * See if the [`KPF_THP`](https://github.com/torvalds/linux/blob/v5.10/include/linux/kernel-page-flags.h) flag is set for the PFN retrieved above in [`/proc/kpageflags`](https://www.kernel.org/doc/Documentation/vm/pagemap.txt).

[The gory details:]{#code}

```c
#include <errno.h>
#include <fcntl.h>
#include <linux/kernel-page-flags.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define fail(...) do { fprintf(stderr, __VA_ARGS__); exit(EXIT_FAILURE); } while (0)

// normal page, 4KiB
#define PAGE_SIZE (1 << 12)
// huge page, 2MiB
#define HPAGE_SIZE (1 << 21)

// See <https://www.kernel.org/doc/Documentation/vm/pagemap.txt> for
// format which these bitmasks refer to
#define PAGEMAP_PRESENT(ent) (((ent) & (1ull << 63)) != 0)
#define PAGEMAP_PFN(ent) ((ent) & ((1ull << 55) - 1))

static void check_huge_page(void* ptr);

int main(void) {
  // allocate 10 huge pages
  size_t huge_page_size = 1 << 21;
  size_t size = huge_page_size * 10;
  void* buf = aligned_alloc(1 << 21, size);
  madvise(buf, size, MADV_HUGEPAGE);
  // allocate and check each page
  for (void* end = buf + size; buf < end; buf += huge_page_size) {
    // allocate page
    memset(buf, 0, 1);
    // check the page is indeed huge
    check_huge_page(buf);
  }
  printf("all good, exiting\n");
  return 0;
}

// Checks if the page pointed at by `ptr` is huge. Assumes that `ptr` has already
// been allocated.
static void check_huge_page(void* ptr) {
  int pagemap_fd = open("/proc/self/pagemap", O_RDONLY);
  if (pagemap_fd < 0) {
    fail("could not open /proc/self/pagemap: %s", strerror(errno));
  }
  int kpageflags_fd = open("/proc/kpageflags", O_RDONLY);
  if (kpageflags_fd < 0) {
    fail("could not open /proc/kpageflags: %s", strerror(errno));
  }

  // each entry is 8 bytes long
  uint64_t ent;
  if (pread(pagemap_fd, &ent, sizeof(ent), ((uintptr_t) ptr) / PAGE_SIZE * 8) != sizeof(ent)) {
    fail("could not read from pagemap\n");
  }

  if (!PAGEMAP_PRESENT(ent)) {
    fail("page not present in /proc/self/pagemap, did you allocate it?\n");
  }
  if (!PAGEMAP_PFN(ent)) {
    fail("page frame number not present, run this program as root\n");
  }

  uint64_t flags;
  if (pread(kpageflags_fd, &flags, sizeof(flags), PAGEMAP_PFN(ent) << 3) != sizeof(flags)) {
    fail("could not read from kpageflags\n");
  }

  if (!(flags & (1ull << KPF_THP))) {
    fail("could not allocate huge page\n");
  }

  if (close(pagemap_fd) < 0) {
    fail("could not close /proc/self/pagemap: %s", strerror(errno));
  }
  if (close(kpageflags_fd) < 0) {
    fail("could not close /proc/kpageflags: %s", strerror(errno));
  }
}
```

Some useful resources apart what was already linked:

* [`page-info`](https://github.com/travisdowns/page-info), a small library by Travis Downs to get most of the information out of `/proc/<pid>/pagemap` and `/proc/kpageflags`.
* [This useful StackOverflow answer](https://stackoverflow.com/a/47823238/524111), also by Travis Downs, describing the method in this article, but without code details.
* [`transhuge-stress.c`](https://github.com/torvalds/linux/blob/2c85ebc57b3e1817b6ce1a6b703928e113a90442/tools/testing/selftests/vm/transhuge-stress.c), a useful stress test for page tables found in the kernel tree.

[^kernel-transp]: `CONFIG_TRANSPARENT_HUGEPAGE` needs to be enabled in the kernel config for things to work, but this has been the case for all the systems I've tried, and I didn't bother checking what happens to `/sys/kernel/mm/transparent_hugepage/enabled` if it's not enabled.

[^root]:
    Getting the page frame number (PFN) from `/proc/self/pagemap` [requires `CAP_SYS_ADMIN` capability](https://www.kernel.org/doc/Documentation/vm/pagemap.txt), therefore it would be possible to read it as a normal user by issuing

    ```
    % sudo setcap cap_sys_admin+ep <executable>
    ```

    And then enable dumping explicitly with

    ```c
    prctl(PR_SET_DUMPABLE, 1, 0, 0)
    ```

    The "dumpable" flag regulates whether the `/proc/[pid]` files are owned to the user or to `root`, as described in the [man page for `/proc/[pid]`](https://man7.org/linux/man-pages/man5/proc.5.html):
    
    > The files inside each `/proc/[pid]` directory are normally owned by the effective user and effective group ID of the process.  However, as a  security measure, the ownership is made `root:root` if the process's "dumpable" attribute is set to a value other than 1.
    
    The dumpable flag is normally set, but if we set the capability like described above it is not, as described in this [StackOverflow answer](https://unix.stackexchange.com/a/459682/364148).

    However, even after doing all this work, we still won't be able to read from `/proc/kpageflags`, which is only readable by `root` 🙃.

[^madvise]:
    The man page for [`madvise`](https://man7.org/linux/man-pages/man2/madvise.2.html) states (emphasis mine):

    > Enable  Transparent  Huge Pages (THP) for pages in the range specified by addr and length.  Currently, Transparent Huge Pages work only  with  private  anonymous  pages (see `mmap(2)`).  The kernel will regularly scan the areas marked as huge page candidates to replace  them  with  huge  pages. **The kernel will also allocate huge pages directly when the region is naturally aligned to the huge page size (see `posix_memalign(2)`).**

[^mmap]: Travis Downs [points out](https://twitter.com/trav_downs/status/1462929358155223043) that `mmap` might be a safer option, since `aligned_alloc` and friends might preemptively allocate pages.