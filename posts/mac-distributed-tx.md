---
title: Message authentication codes for safer distributed transactions
date: 2024-06-03
tags: [post, short]
sidenotes: true
description: I've been developing and quickly deploying a distributed system, which is a class of software where bugs are expensive. A few hundred petabytes later, we haven't lost a single byte of data, also thanks to a simple trick which catches a large class of bugs when delegating responsibilities to possibly buggy software. It's a neat use of cryptography beyond security, so here's a small description.
---

I've been developing and quickly deploying a distributed system, which is a class of software where bugs are expensive. A few hundred petabytes later, we haven't lost a single byte of data, also thanks to a simple trick which catches a large class of bugs when delegating responsibilities to possibly buggy software.[^josh] It's a neat use of cryptography beyond security, so here's a small description.

[^josh]: The idea has probably been discovered many times, but in my case it is due to Josh Leahy.

<div>

The filesystem in question separates file system metadata and file contents. To traverse directories and list their contents, the clients talk to metadata servers. To fetch the contents of a file, the metadata servers return references to chunks of data stored in storage nodes. The clients then go and fetch the chunks from the storage nodes[^reed-solomon]

[^reed-solomon]: Each file will be split over multiple chunks using [Reed-Solomon coding](/posts/reed-solomon.html), so that an arbitrary number of storage nodes can fail while still being able to read the files.

To write a new file, the clients communicate their wish to create it to the metadata server. The metadata server picks some storage nodes in which the chunks making up the file should be written. The client goes off and writes said chunks to the storage nodes, and tells the metadata server when it has done so. The file writing procedure is then complete.

</div>

This scheme is nice since it allows to separate the databasey, "smart" part of the system (the metadata servers) from the bandwidth and storage hungry, "dumb" part of the system (the storage nodes).

However we're faced with an issue: what if a buggy client says it has written a chunk, when it actually hasn't successfully done so? We now have dangling references to non-existant chunks in our metadata servers. Or conversely, buggy clients might write chunks which are not referenced anywhere in the metadata server, causing chunk leaks. Or, they could delete chunks for files which are not scheduled for deletion, again causing dangling references. There are many parallels with memory management, albeit on a distributed setting.

To solve this problem, instead of blindly trusting the clients, the metadata server and storage nodes will demand proof. Each storage node is assigned a secret key which is shared with the metadata server. When the metadata server tells the clients to go off and write chunks, it'll give them a [message authentication code](https://en.wikipedia.org/wiki/Message_authentication_code) (MAC) to present to the storage node to do so.

The "write request" MAC is produced by the metadata server by running a unique chunk id and a hash of the chunk contents through the respective storage node secret key. When the storage nodes receive a write request, they check the chunk id and contents against the received the MAC and reject the write request if they don't match -- ruling out leaks or wrong references. The storage nodes then return a "write proof" MAC, also predicated on chunk id and hash, which certifies that the chunk has been written. The client returns the proof to the metadata server, and the file is certified as written.

[^deletion]: A similar scheme is adopted for chunk deletions, to ensure that no chunk leaks are present.

Note that this scheme does not need to solve any "security" concern. For instance unless the hash is cryptographically secure,[^crc] the client could still intentionally craft bogus data different from what it was tasked to persist, successfully write it to the storage node, and get a valid proof. But it makes it exceedingly unlikely to _mistakenly_ (rather than maliciously) certify a file as written when it hasn't -- all the while keeping the metadata and chunk handling separate.

[^crc]: Unlike for instance a cheap CRC32, which is what we picked for this filesystem.

To give a concrete example of the class of bugs that this helped us with, we had a rare condition where the kernel module for the filesystem (that is, the client) mistakenly ignored errors coming from the storage nodes, skipping to the next successful response instead. We immediately caught the problem since the out-of-sync stream resulted in the wrong proofs being forwarded: otherwise, we would have blindly certified errored writes, breaking key filesystem invariants in the process.

## Acknoledgements

Thanks to [Alexandru Scvorţov](https://scvalex.net/) for reading drafts of this blog post.
