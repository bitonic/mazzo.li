---
title: "Backing up WhatsApp data through the multi-device web client"
date: 2021-10-26
tags: [post]
sidenotes: true
description: Systematically backing up WhatsApp messages and media in an open format is impractical. However the multi-device beta makes the WhatsApp web client an almost first-class WhatsApp client. I describe how the web client stores messages and media, and a proof of concept program, `wadump`, which can extract and display them.
image: https://mazzo.li/assets/images/whatsapp-multi-device.png
---

Systematically backing up WhatsApp messages and media in an open format is impractical. However the [multi-device beta](https://faq.whatsapp.com/web/download-and-installation/how-to-join-or-leave-the-multi-device-beta/?lang=en) makes the WhatsApp web client an almost first-class WhatsApp client. I describe how the web client stores messages and media, and a proof of concept program, [`wadump`](https://github.com/bitonic/wadump), which can extract and display them.

***

After I wrote the tool and the post, I realized that somebody had [already reverse engineered the WhatsApp web client protocol](https://github.com/sigalor/whatsapp-web-reveng). Some of the information in this post (especially regarding media encryption) is explained more completely in that project.

***

As you might have noticed from some of my other [blog](./gdpr-success-story.html) [posts](./hetzner-zfs.html), I care about controlling my data. This is not out of privacy concerns, but because I want to be sure that I will never lose access to it. This fear is not abstract: I did lose access to a lot of my data a long time ago, and never got over it 🙃.

<div>

These days I'm quite happy with my data situation, with one big exception: WhatsApp. For better of worse a large portion of my communication goes through it, and I would hate to lose some of my WhatsApp text and audio messages.[^photos] I do back them up to Google Drive using the built-in backup functionality, but that data depends on me having access to my Google account, and is also not usable without the official WhatsApp app.[^good-intentions]

[^photos]: The situation for photos and videos is already acceptable, since they get saved as normal photos and videos, which are easy to back up. However they are not easy to link with the surrounding messages.

[^good-intentions]: I do appreciate about WhatsApp's focus on security and privacy, and its developers are probably well intentioned when they make it hard to extract data. I just wish there was a "I know what I am doing" button for users who want that capability.

In other words, I think there's a chance that I won't have access to my "official" WhatsApp data in a few decades.

On Android other users have decrypted the `.crypt12` files that WhatsApp uses, but I never bothered to root my phone, which is a requirement to get the decryption keys, and I'm generally not very familiar with Android as a platform.

This is why I was very excited to learn that [WhatsApp is rolling out a "real" desktop application](https://faq.whatsapp.com/general/download-and-installation/about-multi-device-beta/?lang=en), which does not need the phone to operate. Since the desktop application can send and receive messages independently, it must also store them independently. And since the desktop application is just a web application, it should be relatively easy to programmatically extract the messages for backup purposes.

</div>

<div>

This article describes my finding on how the WhatsApp web application stores messages and media. It also describes a program, [`wadump`](https://github.com/bitonic/wadump), which automatically dumps all the WhatsApp data present in the web client, together with a rudimentary viewer for said data:[^generated-data]

[^generated-data]: In the video below I've replaced contact names, text messages, and images with placeholders, although the video and audio are unchanged.

<div>
  <video controls  style="width: 100%; max-width: 100%">
    <source src="/assets/other/wadump-demo.mp4" type="video/mp4">
    Your browser does not support the video tag.
  </video>
</div>

If you're in a hurry you can just head directly to [the GitHub repo](https://github.com/bitonic/wadump) which contains instructions on how to run it.

</div>

## Disclaimer & Limitations

I am not affiliated with Facebook or WhatsApp, and this investigation was done purely to preserve my personal data better. I don't know if backing up your data this way breaches WhatsApp's terms of service. Use at your own risk!

This information is useful at the time of writing, but it could very well not be in the near future if the web client changes the way it stores its data.

Finally, `wadump` currently [cannot reliably extract all media](#media-troubles).

## How multi-device WhatsApp works

This is explained well in the [Facebook Engineering blog post about the multi-device beta](https://engineering.fb.com/2021/07/14/security/whatsapp-multi-device/), but the key concept is simple.

Before the multi-device web app, the web client relied on the phone to send and receive messages, deferring all E2EE with our contacts to the phone:

<div class="center-image" style="margin-bottom:1rem">
![<small>Image taken from the [Facebook Engineering blog post](https://engineering.fb.com/2021/07/14/security/whatsapp-multi-device/).</small>](/assets/images/whatsapp-legacy.png)
</div>

The multi-device beta changes this, so that many devices (up to 4) can send and receive messages directly. If we have more than one device, say a laptop and a phone, our contacts will encrypt their messages once per device, and similarly when we send a message from one of our devices we will route it both to the recipient and to our other devices:

<div class="center-image" style="margin-bottom:1rem">
![<small>Image taken from the [Facebook Engineering blog post](https://engineering.fb.com/2021/07/14/security/whatsapp-multi-device/).</small>](/assets/images/whatsapp-multi-device.png)
</div>

<div>

While the changes in how communication between clients are interesting, in this blog post we're interested in how the web client stores local messages, rather.

When the web client first connects, the phone (which remains a "privileged" client in terms of key and device management) sends an initial bundle of messages to the web client, together with other information such as our contacts. After this initial setup is done, the web client is operational, and it will receive and store all new messages that get sent to our number.[^partial-history]

[^partial-history]: Unfortunately there does not seem to be a way to get the phone to send _all_ its messages to the web client on connection. This means that we'll only have access to the initial bundle of messages and all subsequent messages, but not the ones prior to that.

</div>

## How data is stored by the WhatsApp client

The data stored by the web client can be divided in three parts:

* Metadata stored as cleartext in IndexedDB;
* Encrypted text chat content, also stored in IndexedDB;
* Encrypted media files, stored in WhatsApp's CDN, and cached locally.

[IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) is a persistent storage facility for web applications --- think of an in-browser key-value store. Web applications can create databases and tables (which IndexedDB calls "object stores"), and store JavaScript objects indexed by some key in them.

The cleartext metadata includes a lot of information, mostly stored in the `model-storage` database, including:

* Our contact list, including names and phone numbers, in object store `contact`;
* The list of groups we're in, in object store `group-metadata`;
* Information about the messages we've sent and received, including senders and recipients, their time, and in which chat they appeared, in object stores `message` and `chat`.

You can check this data out for yourself in the "IndexedDB" section of the "Application" tab of the Chrome dev tools.

The cleartext data can be saved by just [dumping the relevant object stores](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L384). Retrieving the actual contents of the messages, wether text or media, is trickier.

## Decrypting text chats

Messages are stored as JSON objects in the `message` object store. For text messages, the content of the message is encrypted using AES-CBC, as implemented by [the `SubtleCrypto` API](https://developer.mozilla.org/en-US/docs/Web/API/AesCbcParams):

```javascript
{
  id: ...,
  t: 1517917600, // seconds epoch
  from: ...,
  to: ...,
  msgRowOpaqueData: { 
    // An identifier telling us which key to use to decrypt this message
    _keyId: 1,
    // The initialization vector to be used by AES-CBC decryption, see
    // <https://developer.mozilla.org/en-US/docs/Web/API/AesCbcParams>
    iv: ArrayBuffer(16),
    // The actual encrypted data
    _data: ArrayBuffer(80), 
  },
  ... omitted fields ...
}
```

AES-CBC is a symmetric encryption scheme --- the same key is used to encrypt and decrypt messages. But where does this key come from?

### Storing, retrieving, and deriving keys

<div>

To operate, the web client stores some cryptographic keys in IndexedDB. These keys are needed both to perform E2EE when sending and receiving messages, but also to encrypt the messages we store locally. Presumably this is done so that third-parties cannot read messages even if they have access to our machine --- if the application isn't currently running anyway.[^media-encryption]

[^media-encryption]: Interestingly this does not hold for encrypted media files, as we'll see later.

The keys are stored through the [`CryptoKey` API](https://developer.mozilla.org/en-US/docs/Web/API/CryptoKey), which lets us use the keys in JavaScript without being able to leak their contents, inadvertently or otherwise.

</div>

The keys that we are interested in are in `keys` object store in the `wawc_db_enc` IndexedDB database. Each object in this object store looks like this:

```javascript
{
  id: number,          // the key id, as referenced from the messages
  key: CryptoKey,
  _expiration: number, // milliseconds epoch
}
```

In my case I have only one key, with `id` 1.

### Deriving the right key

The keys in IndexedDB are not used directly to encrypt and decrypt messages. Rather they are be used to _derive_ an AES-CBC key for message encryption, using the [HKDF](https://en.wikipedia.org/wiki/HKDF) key derivation function. Key derivation functions are used to turn some secret (maybe a passphrase or, like in our case, another key) into a key suitable for the task at hand.

HKDF in particular lets us integrate other bits of information (the "salt" and the "info"), and allows us to specify the length of the output key.

WhatsApp web uses [the `deriveKey` function  from `SubtleCrypto`](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey) to turn our "master" key into an AES-CBC key suitable for local symmetric encryption of our text messages. It looks like this:

```javascript
// https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey
crypto.subtle.deriveKey(
  // The algorithm used to derive the key (HKDF with SHA256 as hash
  // function), together with its parameters -- the salt and the info.
  // We'll see below where these come from.
  {
    name: "HKDF",
    hash: "SHA-256",
    salt,
    info,
  }
  // The key stored in `wawc_db_enc`
  key,
  // The specification of the derived key
  { name: "AES-CBC", length: 128 },
  // Whether the key can be extracted (e.g. whether we can dump its contents
  // with `crypto.subtle.exportKey`)
  false,
  // What we'll use the key for -- in this case we'll perform symmetric
  // encryption with it, to store and retrieve our text messages
  [ "encrypt", "decrypt" ],
);
```

Given the code above, all we need to generate the appropriate key is the `salt` and the `info` parameters to the HKDF function. The `salt` is non-secret data which [gives HKDF some desirable properties](https://datatracker.ietf.org/doc/html/rfc5869#section-3.1), while the `info` parameter [is used to "bind" the key for a certain application](https://datatracker.ietf.org/doc/html/rfc5869#section-3.2).

Typically the `salt` is generated at random, while the `info` contains some info on where the key is used, such as protocol number, user identity, and so on.

### Finding the `info` and `salt`

<div>

The `info` is readily available: it is stored in the browser [`localStorage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage), with key `WebEncKeySalt`, base64 encoded.

Retrieving the `salt` requires more work. It is sent in a welcome message on the WebSockets connection that WhatsApp web client uses to communicate with the server.[^where-salt]

[^where-salt]: Note that the `salt` must be stored on WhatsApp's servers, rather than on the phone, since it is sent over even if the phone is turned off. After WhatsApp uses it to derive the key, it is forgotten, which makes it hard to retrieve after the fact.

We could read and interpret the WebSockets message at startup, but that would require writing a parser for WhatsApp's protocol, and making sure to run the code as soon as the web client starts. However, there is a simpler way to access the information we need to decrypt the messages.

</div>

### Retrieving the derived key by monkey patching

WhatsApp web uses `crypto.subtle.decrypt` to decrypt messages. Luckily, `crypto.subtle.decrypt` is not a read-only property, and we can [replace it](https://en.wikipedia.org/wiki/Monkey_patch) with a function which waits until it is called with arguments which are capable of decoding messages.

[It looks like this](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L342):

* We read a message from the `message` object store;
* We replace `crypto.subtle.decrypt` with a function which tries to decode the message above with the arguments passed to decrypt;
* Once we've found a key that works, we store it in a global variable, restore the original function, and start decoding the messages in bulk.

This relies on the same key working for all messages, which is the case for me.

Using this method we avoid having to derive the key ourselves, at the cost of requiring some user interaction to trigger message decryption in the web client.

### Decoding the content of the messages

Once [`msgRowOpaqueData._data` is decrypted](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L275), the resulting cleartext is a [Protocol Buffer](https://developers.google.com/protocol-buffers) message with this schema:

```
message WhatsAppMessage {
  string body = 1;
  string caption = 2;
  string loc = 4;
  double lng = 5;
  double lat = 7;
  int32 paymentAmount1000 = 8;
  string paymentNoteMsgBody = 9;
  string canonicalUrl = 10;
  string matchedText = 11;
  string title = 12;
  string description = 13;
  bytes futureproofBuffer = 14;
}

message FullWhatsAppMessage {
  WhatsAppMessage currentMsg = 1;
  WhatsAppMessage quotedMsg = 1;
}
```

The `quotedMsg` contains the message we're replying to, if any. To decode the protobuf messages without depending on external code, `wadump` contains [a minimal protobuf decoder](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L81).

## Downloading and decrypting media

Now that we've covered text chats, let's talk about media.

Media (audio messages, images, videos, documents) is predictably not stored inline in the protobuf message above. Rather, it is encrypted and stored in WhatsApp's CDNs, and retrieved and decrypted when needed. A media message looks like this in the `message` object store:

```javascript
{
  id: ...,
  t: 1627663880, // seconds epoch
  from: ...,
  to: ...,
  // Might be "video", "audio", etc.
  type: "image",
  mimetype: "image/jpeg",
  // base64 encoded SHA256 hash of the decrypted message
  filehash: "dzBnGb+PQVMhdlkuoUkXB+3xMWEJsCadLRJ9JVOdTeQ=",
  // base64 encoded key used to decrypt the message
  mediaKey: "e4Q1drkxa4ST0+Ac9Ug86W0etx3WbDaZ0IGRs/3ZVI4=",
  // CDN path to the encrypted message
  directPath: "/v/t62.7118-24/35487369_1391679344547224_9081397384390117319_n.enc?ccb=11-4&oh=a8323bff0917bdc1827929420adec846&oe=61287211&_nc_hot=1627663880",
  // Image metadata...
  size: 165863,
  height: 1281,
  width: 1600,
  ... omitted fields ...
}
```

The main CDN domain for WhatsApp seems to be `https://mmg.whatsapp.net`. Appending the `directPath` to it will give us the URL to hit to download the encrypted file. Once that is done, we need to decrypt it.

### Deriving the media encryption keys

AES-CBC is used to also decrypt media files. Like with messages, HKDF with SHA256 as hash function is used to derive the key. This time the parameters to HKDF are:

* The key is the 32 bytes of data encoded in base64 in the `mediaKey` field of the message object.
* The `info` is derived from the `type` of the message. For `"image"` it is `"WhatsApp Image Keys"`, for `"video"` it is `"WhatsApp Video Keys"`, [and so on](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L155).
* The `salt` is not used, in which case HKDF just uses [a dummy salt filled with 0s](https://datatracker.ietf.org/doc/html/rfc5869#section-2.2).
* The output key length is 112 bytes.

In this case HKDF is [performed directly on the raw data from `mediaKey`](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L205), without going through `crypto.subtle.importKey` and `crypto.subtle.deriveKey`.

Once the 112 bytes long key is generated, [it is chopped up](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L215) to get the initialization vector and key for AES-CBC decryption:

```javascript
const mediaKeys = {
  iv: key.slice(0, 16),
  encKey: key.slice(16, 48),
  // Other keys, unused for our purposes
  macKey: key.slice(48, 80),
  refKey: key.slice(80, 112)
}
```

Note how in this case, unlike with text messages, we only need cleartext data to decrypt media files.

Once we have the data above, we can go ahead and use it to [decrypt the encrypted bytes](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L223):

```javascript
const key = await crypto.subtle.importKey("raw", mediaKeys.encKey, "AES-CBC", false, ["decrypt"]);
// the last 10 bytes contain the mac
bytes = bytes.slice(0, -10);
const cleartext = await crypto.subtle.decrypt({ name: "AES-CBC", iv: mediaKeys.iv }, key, bytes);
```

### Media caching

The web client caches decrypted media files in the `"lru-media-array-buffer-cache"` cache, through the [`CacheStorage` API](https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage).

When looking for media files, [`wadump` looks there first](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L244), and stores downloaded files in the cache after download, as the web client would. This is to avoid sending off thousands of CDN requests each time we want to dump the media messages.

### Media troubles

While the method above works for recent media files, it seems that the WhatsApp CDN does not store media forever. In those cases requesting the file results in a 410 or 404. When that happens the web client re-requests the media file from the phone and gets a new link from the WebSocket channel.

I have not studied in detail the protocol that is used on the WebSocket channel, and I have not experimented with using it independently from the web client. Therefore `wadump` can currently only retrieve relatively recent or cached media messages.

Another mystery is how the encrypted `msgRowOpaqueData` is used for for media messages, since it is still present. It might be used to request the missing files above, but again, I have not figured that out yet.

## Wrapping up the dump, and browsing it

Once `wadump` has collected all the information that it can find, [it stores everything in a tarball](https://github.com/bitonic/wadump/blob/3cf6b7897e0dd1c07ee275ecc4ca4884463b5dd1/dump-messages.js#L116), which it then offers for download.

The tarball contains the following files:

```
message.json
chat.json
contact.json
group-metadata.json
media/<filehash>
```

The `chat.json`/`contact.json`/`group-metadata.json` files simply contain a dump of the respective object stores. `message.json` contains a dump of the `message` object store, but with the `msgRowOpaqueData` field replaced by a `msgRow` field with the decrypted text message.

The `media` folder contains all the media files we could retrieve, named with their `filehash`.

`wadump` also contains a small utility to easily browse the contents of the tarball archive --- head to [the README](https://github.com/bitonic/wadump/blob/master/README.md) for more information.

## Closing thoughts & future work {#closing-thoughts}

<div>

It was fun to figure out how the WhatsApp web client works, but currently `wadump` isn't a tool I would rely on.[^limitations] With enough interest a browser extension could be created to keep the WhatsApp web client data in sync with the local file system, possibly using the new [File System Access API](https://developer.mozilla.org/en-US/docs/Web/API/File_System_Access_API). This would also work around the limitations in media downloads by downloading new media files soon after they appear in a message.

[^limitations]: See the README for a [list of limitations](https://github.com/bitonic/wadump#bugs--limitations).

Obviously writing software targeting a proprietary protocol will always be an uphill struggle, but projects like [youtube-dl](https://youtube-dl.org/) prove that with effort it is possible to maintain useful tools of this kind.

</div>

## Acknowledgements

Thanks to Alexandru Scvorţov and Alex Appetiti for reading drafts of this article.