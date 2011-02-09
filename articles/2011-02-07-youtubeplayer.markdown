---
title: Better YouTube embeds
---

I always hated the default YouTube embedded player. Here is a reminder
on how it looks:

<div style="text-align:center">
<object width="640" height="385"><param name="movie" value="http://www.youtube.com/v/XDZ31YQvxWY?fs=1&amp;hl=en_US&amp;rel=0"></param><param name="allowFullScreen" value="true"></param><param name="allowscriptaccess" value="always"></param><embed src="http://www.youtube.com/v/XDZ31YQvxWY?fs=1&amp;hl=en_US&amp;rel=0" type="application/x-shockwave-flash" allowscriptaccess="always" allowfullscreen="true" width="640" height="385"></embed></object>
</div>

It doesn't even look that bad, but embedded videos nowadays all look
the same, and that's annoying.

Luckily YouTube provides a [JavaScript API] that lets you control the
embedded player in every aspect via JS functions call. On the other
hand the API is quite ugly, and makes you do horrible things like
defining global functions with a pre-defined name, or handling events
passing the function name as a string.

[JavaScript API]: http://code.google.com/apis/youtube/js_api_reference.html

### The YouTubePlayer MooTools class

So I decided to make things a little bit less ugly, and I wrote a
[MooTools] class that hides all the uglyness and makes you happy. You
can download it, browse the code, fork it and many other things on
[github].

[MooTools]: http://mootools.net/
[github]: https://github.com/rostayob/YouTubePlayer

For all the people who don't like to read, here's an example on what
you can do with it:

<div id="player">
<noscript>You need Javascript to play YouTube videos!</noscript>
</div>

### Creating the YouTubePlayer object

To use the class, all you have to to is to create a new element, and
then inject it somewhere:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.javascript}
var player = new YouTubePlayer({
                 width: 425,
                 height: 356,
                 videoId: 'XDZ31YQvxWY',
                 id: 'videoPlayer',
                 embedded: false,
                 suggestedQuality: 'large'
             });

$('playerDiv').grab(player);
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The initializer accepts all the [Swiff] options, plus the following:

* `videoId`: A string containing the id if the video (if the URL is
     "http://www.youtube.com/watch?v=Em80ViNVlHM" then your id is
     "Em80ViNVlHM").
* `embedded`: A boolean that determines wheter the video is in the old
  embedded style or as a bare video. Both versions are controllable
  via the js api.
* `suggestedQuality`: A string with the quality you would like to have
  with your video. The "suggested" is because that quality might not
  be available. In that case, the quality will be set to the next
  lowest level that is available.  The quality level available are
  `small`, `medium`, `large`, `hd720`, `hd1080`, `highres` or
  `default`. `default` is not an actual quality level, but simply
  instructs YouTube to select the most appropriate playback quality,
  which will vary for different users, videos, systems and other
  playback conditions.

[Swiff]: http://mootools.net/docs/core/Utilities/Swiff

### Controlling the player

<script type="text/javascript" src="$root/js/libs/mootools-more.js"></script>
<script type="text/javascript" src="https://github.com/rostayob/YouTubePlayer/raw/master/YouTubePlayer.js"></script>
<script type="text/javascript" src="$root/js/youtubeplayer-article.js"></script>
