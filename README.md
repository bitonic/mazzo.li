## Building

To build the Hakyll website (requires [Nix](https://nixos.org/)):

```
$ # Builds and exists
$ make hakyll-build
$ # Builds continuously
$ make hakyll-build
```

To build the JavaScript scripts:

```
$ make yarn-install
$ # Build dev build
$ make webpack-build
$ # Build dev build continuosly
$ make webpack-watch
$ # Build prod build
$ make webpack-build-prod
```

## License

Copyright (c) 2012-2021 Francesco Mazzoli

The content of the website (text and media) is licensed under the [Creative Commons Attribution-ShareAlike License](https://creativecommons.org/licenses/by-sa/3.0/). Note that some of the media (the binary files) aren't in this repo, but they are licensed under the same license.

The code is licensed under the ISC license:

> Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.
> 
> THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

