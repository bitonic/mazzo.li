---
title: Controlling YouTube videos with JS
---

Some days ago I had some ideas in mind that involved using the
[YouTube Javascript API], and I quickly realized how messy the whole
thing was. While the API lets you control really everything about the
playback of the video, it also makes you do horrible things in the process, like
defining global functions with a pre-defined name, or handling events
passing the function names as strings.

[YouTube JavaScript API]: http://code.google.com/apis/youtube/js_api_reference.html

### The YouTubePlayer MooTools class

So I decided to make things a little bit less ugly, and I wrote a
[MooTools] class that hides all the bad bits and makes you happy. You
can download it, browse the code, fork it and many other things on
[github].

[MooTools]: http://mootools.net/
[github]: https://github.com/rostayob/YouTubePlayer

For all the people who don't like to read, here's an example on what
you can do with it:

<div id="demo1">
You need Javascript to play YouTube videos!
</div>

For a more comprehensive example, check the [demo] at the bottom of
the page.

[demo]: #demo

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
     "`http://www.youtube.com/watch?v=Em80ViNVlHM`" then your id is
     "`Em80ViNVlHM`").
* `embedded`: A boolean that determines wheter the video is in the old
  embedded style or as a bare video. Both versions are controllable
  via the js api.
* `suggestedQuality`: A string with the quality you would like to have
  with your video. The "suggested" is because that quality might not
  be available. In that case, the quality will be set to the next
  lowest level that is available.  The quality levels available are
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

### <a name="demo">A more serious demo</a>

To test all the features I created a demo that mimicks the
[official one], plus the possibility of changing the player type
without reloading the page.

[official one]: http://code.google.com/apis/youtube/youtube_player_demo.html

<div id="demo2">
You need Javascript to play YouTube videos!
</div>
