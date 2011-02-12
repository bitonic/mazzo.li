// BEWARE! The code below is really, REALLY, ugly, I hacked a demo
// quickly in a few hours and I put the old YouTubePlayer demo
// here. Forgive me.

document.addEvent('domready', function() {
    demo1();

    demo2();
});

function demo1() {
    var player = new YouTubePlayer({
        width: 680,
        height: 380,
        videoId: 'XDZ31YQvxWY',
        id: 'videoPlayer',
        embedded: false,
        suggestedQuality: 'large'
    });

    $('demo1').empty();
    $('demo1').setStyles({
        'background-color': 'black',
        margin: '10px auto',
        'font-size': '20px',
        'text-shadow': '1px 1px 0 #476871, 2px 2px 0 #476871',
        'font-family': 'Consolas, Inconsolata, "Liberation Mono", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", Menlo, Monaco, monospace',
        'width': '680px'
    });

    $('demo1').grab(player);

    var bar = new Element('div', {
        styles: {
            color: 'white',
            padding: '0 5px'
        }
    });

    $('demo1').grab(bar);

    // -------------------------------------------------------------------------
    // Separator...
    var barSeparator = function() {
        return new Element('div', {
            styles: {
                float: 'left',
                margin: '0 3px'
            },
            html: '&middot;'
        });
    };

    // -------------------------------------------------------------------------
    // Play/pause
    var barPlayPause = new Element('div', {
        styles: {
            float: 'left',
            cursor:'pointer',
            width: '55px',
            'text-align': 'center',
            'margin-bottom': '2px'
        },
        html: 'Play'
    });

    barPlayPause.addEvent('click', function() {
        player.playVideo()
    });

    player.addEvent('stateChange', function(state) {
        barPlayPause.empty();
        barPlayPause.removeEvents();
        if (state != 1) {
            barPlayPause.set('html', 'Play');
            barPlayPause.addEvent('click', function() {
                player.playVideo();
            });
        } else {
            barPlayPause.set('html', 'Pause');
            barPlayPause.addEvent('click', function() {
                player.pauseVideo();
            });
        }
    });
    
    bar.grab(barPlayPause);
    bar.grab(barSeparator());

    // -------------------------------------------------------------------------
    // Volume
    var barVolume = new Element('div', {
        styles: {
            float: 'left'
        }
    });

    var barVolumeMute = new Image();
    var barVolumeMute = new Element('img', {
        src: '../images/youtubeplayer-article-volume.png',
        styles: {
            cursor: 'pointer'
        },
        mute: false
    });

    var barVolumeContainer = new Element('div', {
        styles: {
            position: 'absolute',
            width: '26px',
            'background-color': 'transparent'
        }
    });

    var barVolumeSlider = new Element('div', {
        styles: {
            float: 'left',
            height: '70px',
            margin: '0 0 5px 0'
        }
    });

    var barVolumeKnob = new Element('div', {
        styles: {
            width: '16px',
            height: '3px',
            margin: '0 0 0 4px',
            'background-color': 'white',
            border: '1px solid #476871',
            'cursor': 'pointer'
        }
    });


    barVolumeMute.addEvent('click', function() {
        if (barVolumeMute.mute) {
            barVolumeMute.src = '../images/youtubeplayer-article-volume.png';
            player.unMute();
        } else {
            barVolumeMute.src = '../images/youtubeplayer-article-volumemute.png';
            player.mute();
        }
        barVolumeMute.mute = !barVolumeMute.mute;
    });

    barVolumeContainer.grab(barVolumeMute);
    barVolumeContainer.grab(barVolumeSlider);
    barVolumeSlider.grab(barVolumeKnob);

    barVolume.grab(barVolumeContainer);

    bar.grab(barVolume);

    var volumeSlider = new Slider(barVolumeSlider, barVolumeKnob, {
        mode: 'vertical',
        onChange: function(value){
            player.setVolume(100 - value);
        }
    });
    volumeSlider.dragging = false;
    volumeSlider.mouseIn = false;
    barVolumeSlider.setStyle('display', 'none');


    barVolumeMute.addEvent('mouseenter', function() {
        barVolumeSlider.setStyle('display', 'block');
        barVolumeContainer.setStyle('background-color', 'black');
        volumeSlider.mouseIn = true;
    });

    barVolumeContainer.addEvent('mouseleave', function() {
        volumeSlider.mouseIn = false;
        if (!volumeSlider.dragging) {
            barVolumeSlider.setStyle('display', 'none');
            barVolumeContainer.setStyle('background-color', 'transparent');
            
        }
    });

    volumeSlider.drag.addEvent('start', function() {
        volumeSlider.dragging = true;
    });

    volumeSlider.drag.addEvent('complete', function() {
        volumeSlider.dragging = false;
        if (!volumeSlider.mouseIn) {
            barVolumeSlider.setStyle('display', 'none');      
            barVolumeContainer.setStyle('background-color', 'transparent');      
        }
    });

    player.addEvent('playerReady', function() {
        volumeSlider.set(100 - player.getVolume());
    });

    bar.grab(barSeparator().setStyle('margin-left', '27px'));

    // -------------------------------------------------------------------------
    // The quality
    var barQuality = new Element('div', {
        styles: {
            float: 'left',
            width: '60px'
        }
    });

    var barQualityContainer = new Element('div', {
        styles: {
            position: 'absolute',
            'background-color': 'black'
        }
    });

    var qualityFormat = function(quality) {
        if (quality == 'medium')
            return '360p'

        if (quality == 'large')
            return '480p'

        if (quality == 'small')
            return '240p'

        return quality
    }
    
    player.addEvent('playbackQualityChange', function(quality) {
        var qualities = player.getAvailableQualityLevels();

        barQualityContainer.empty();

        var qul = new Element('ul', {
            styles: {
                'list-style': 'none',
                margin: '0 6px 3px 3px',
                padding: '0'
            }
        });

        qul.addEvent('mouseenter', function() {
            $$('.clickableQuality').each(function(el) {
                el.setStyle('display', 'block');
            });
        });

        qul.addEvent('mouseleave', function() {
            $$('.clickableQuality').each(function(el) {
                el.setStyle('display', 'none');
            });
        });
        
        barQualityContainer.grab(qul);

        qualities.each(function(q) {
            if (q == quality) {
                qul.grab(new Element('li', {
                    text: qualityFormat(q)
                }), 'top');
            } else {
                qul.grab(new Element('li', {
                    text: qualityFormat(q),
                    styles: {
                        cursor: 'pointer'
                    },
                    'class': 'clickableQuality'
                }).addEvent('click', function() {
                    player.setPlaybackQuality(q);
                }));
            }
        });

        $$('.clickableQuality').each(function(el) {
            el.setStyle('display', 'none');
        });
    });

    barQuality.grab(barQualityContainer);
    bar.grab(barQuality)
    bar.grab(barSeparator().setStyle('margin-left', '55px'));

    // -------------------------------------------------------------------------
    // The slider
    var barSlider = new Element('div', {
        styles: {
            'width': '478px',
            'float': 'right',
            'background-image': 'url(../images/youtubeplayer-article-slider.png)',
            'margin': '0 3px 0 0'
        }
    });

    var barSliderKnob = new Element('span', {
        text: '|',
        styles: {
            cursor: 'pointer',
            width: '4px'
        }
    });
    
    barSlider.seekPlayer = false;
    
    barSlider.grab(barSliderKnob);

    bar.grab(barSlider);

    var actualSlider = new Slider(barSlider, barSliderKnob, {
        steps:500,
        onChange: function(value) {
            if (barSlider.seekPlayer) {
                player.seekTo(value * player.getDuration() / 500, true)
            }
        }
    });

    actualSlider.drag.addEvent('start', function() {
        barSlider.seekPlayer = true;
        player.pauseVideo();
    });

    actualSlider.drag.addEvent('complete', function() {
        barSlider.seekPlayer = false;
        player.playVideo();
    });


    var delay = 900;
    window.setInterval(function() {
        if (!barSlider.seekPlayer) {
            actualSlider.set(500 * player.getCurrentTime() / player.getDuration())
        }
    }, delay);

    // To clear stuff...
    $('demo1').grab(new Element('hr', {
        styles: {
            clear: 'both',
            height: 0,
            visibility: 'hidden'
        }
    }));
}

function demo2() {
    // yay
    $('demo2').set('html', '<div id="left"><div id="demo2player">You need Flash player 8+ and JavaScript enabled to view this video.</div><h4>Playback controls</h4><a href="javascript:void(0)" id="play">Play</a> | <a href="javascript:void(0)" id="pause">Pause</a> | <a href="javascript:void(0)" id="stop">Stop</a> | Seek to: <input id="seekTo" type="text" size="3"/> seconds <input type="submit" value="Go" id="seekToGo"/><br/><h4>Volume controls</h4><a href="javascript:void(0)" id="mute">Mute</a> | <a href="javascript:void(0)" id="unmute">Unmute</a> | Set volume: <input type="text" size="4" id="volume"/> (0-100) [<span id="currentVolume"></span>] <input type="submit" value="Go" id="setVolume"/><h4>Player size</h4>Width: <input type="text" size="4" id="width"/> | Height: <input type="text" size="4" id="height"/> <input type="submit" value="Go" id="setSize"/><h4>Events</h4><ol id="events"></ol><h4>Video URL</h4><span id="videoUrl"></span><br/><strong>Embed Code</strong><br/><textarea id="embed" cols="50" rows="7"></textarea></div><div id="right"><h4>Select your player</h4>Player type: <select id="playerType"><option value="embedded">Embedded</option><option value="chromeless">Chromeless</option></select><h4>Load or cue a video</h4>Video id or URL: <input type="text" id="idOrUrl" size="20"/><br/>Start at: <input type="text" id="startAt" size="4" value="0"/> seconds<br/>Suggested quality: <select id="newQuality"><option value="default">default</option><option value="small">small</option><option value="medium">medium</option><option value="large">large</option><option value="hd720">hd720</option><option value="hd1080">hd1080</option><option value="highres">highres</option></select><br/><input type="submit" value="Load video" id="loadVideo"/><input type="submit" value="Cue video" id="cueVideo"/><h4>Update current video</h4>Set quality: <select id="setQuality"><option value="default">default</option><option value="small">small</option><option value="medium">medium</option><option value="large">large</option><option value="hd720">hd720</option><option value="hd1080">hd1080</option><option value="highres">highres</option></select><br/><h4>Playback statistics</h4>Duration: <span id="duration"></span><br/>Current time: <span id="currentTime"></span><br/>Player state: <span id="playerState"></span><br/><br/>Total bytes: <span id="totalBytes"></span><br/>Start bytes: <span id="startBytes"></span><br/>Bytes loaded: <span id="bytesLoaded"></span><br/><br/>Quality level: <span id="currentQuality"></span><br/>Available levels: <span id="availableQualities"></span><br/></div><hr style="clear:both;visibility:hidden"/>');

    $$('#demo2 h4').each(function(e) {
        e.setStyle('margin', '0.3em 0');
    });

    $('left').setStyles({
        'float': 'left',
        'margin': '0',
        'text-align': 'center'
    });

    $('right').setStyle('text-align', 'center');

    $('demo2').setStyle('margin', '1em 0 0 0');

    // Creating the player object
    var video = new YouTubePlayer({
        width: 425,
        height: 356,
        videoId: 'u1zgFlCw8Aw',
        id: 'video1',
        embedded: true
    });

    window.playerType = 'embedded';
    $('idOrUrl').set('value', 'u1zgFlCw8Aw');

    $('demo2player').empty();
    $('demo2player').grab(video);

    $('startAt').set('value', '0');

    var refresh = [];

    // Playback controls
    $('pause').addEvent('click', function() {
        video.pauseVideo();
    });

    $('play').addEvent('click', function() {
        video.playVideo();
    });

    $('stop').addEvent('click', function() {
        video.stopVideo();
    });

    $('seekToGo').addEvent('click', function() {
        video.seekTo(parseInt($('seekTo').get('value')));
    });

    // Volume controls
    $('mute').addEvent('click', function() {
        video.mute();
    });

    $('unmute').addEvent('click', function() {
        video.unMute();
    });

    refresh.push(function() {
        $('currentVolume').set('text', video.getVolume());
    });

    $('setVolume').addEvent('click', function() {
        video.setVolume(parseInt($('volume').get('value')));
    });

    // Player size
    $('width').set('value', video.get('width'));
    $('height').set('value', video.get('height'));

    $('setSize').addEvent('click', function() {
        var width = parseInt($('width').get('value'));
        var height = parseInt($('height').get('value'));
        video.setSize(width, height);
    });

    // History
    // Change the video url and embed code
    var urlAndEmbed = function() {
        $('videoUrl').set('text', video.getVideoUrl());
        $('embed').set('html', video.getVideoEmbedCode());
    };

    // Counts the events
    window.events = 0;
    video.addEvent('stateChange', function(state) {
        window.events++;
        $('events').grab(new Element('li', {
            text: 'The state changed changed to "' + state +
                '" (' + YouTubePlayer.states[parseInt(state)] + ').',
            value: window.events
        }), 'top');
    });

    video.addEvent('error', function(error) {
        window.events++;
        $('events').grab(new Element('li', {
            text: 'Error: "' + error + '".',
            value: window.events
        }), 'top');
    });

    video.addEvent('playbackQualityChange', function(quality) {
        window.events++;
        $('events').grab(new Element('li', {
            text: 'The playback quality changed to "' + quality + '".',
            value: window.events
        }), 'top');
    });

    video.addEvent('playerReady', function() {
        window.events++;
        $('events').grab(new Element('li', {
            text: 'Player ready.',
            value: window.events
        }), 'top');
        
        urlAndEmbed();
    });


    // Player type
    $('playerType').addEvent('change', function() {
        var options = video.options;
        options.width = video.get('width');
        options.height = video.get('height');
        if ($('playerType').get('value') == 'embedded') {
            options.embedded = true;
        } else {
            options.embedded = false;
        }

        video = new YouTubePlayer(options);

        $('demo2player').empty();
        $('demo2player').grab(video);
    });

    // Load/cue video
    $('loadVideo').addEvent('click', function() {
        video.loadVideo($('idOrUrl').get('value'),
                        parseInt($('startAt').get('value')),
                        $('newQuality').get('value'));

        urlAndEmbed();
    });

    $('cueVideo').addEvent('click', function() {
        video.cueVideo($('idOrUrl').get('value'),
                       parseInt($('startAt').get('value')),
                       $('newQuality').get('value'));

        urlAndEmbed();
    });

    // Set the quality
    $('setQuality').addEvent('change', function() {
        video.setPlaybackQuality($('setQuality').get('value'));
    });

    // Playback statistics
    refresh.push(function() {
        $('duration').set('text', video.getDuration());
        $('currentTime').set('text', video.getCurrentTime());
        var state = video.getPlayerState();
        $('playerState').set('text',
                             state + ' (' + YouTubePlayer.states[state] + ')');
        $('totalBytes').set('text', video.getVideoBytesTotal());
        $('startBytes').set('text', video.getVideoStartBytes());
        $('bytesLoaded').set('text', video.getVideoBytesLoaded());
        $('currentQuality').set('text', video.getPlaybackQuality());
        if (video.getAvailableQualityLevels()) {
            $('availableQualities').set(
                'text',
                video.getAvailableQualityLevels().join(', '));
        }
    });

    var delay = 300;
    window.setInterval(function() {
        for (var i = 0; i < refresh.length; i++) {
            refresh[i]();

        }
    }, delay);
}