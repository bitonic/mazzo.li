document.addEvent('domready', function() {
    var player = new YouTubePlayer({
        width: 680,
        height: 380,
        videoId: 'XDZ31YQvxWY',
        id: 'videoPlayer',
        embedded: false
    });

    $('player').setStyles({
        'background-color': 'black',
        margin: '10px 0',
        height: '417px',
        'font-size': '20px',
        'text-shadow': '1px 1px 0 #476871, 2px 2px 0 #476871'
    });

    $('player').grab(player);

    var bar = new Element('div', {
        styles: {
            color: 'white',
            padding: '0 5px'
        }
    });


    // -------------------------------------------------------------------------
    // Separator...
    var barSeparator = function() {
        return new Element('div', {
            styles: {
                float:'left',
                margin: '0 3px'
            },
            html: '&middot;'
        });
    };

    // -------------------------------------------------------------------------
    // Play/pause
    var barPlayPause = new Element('div', {
        styles: {
            float:'left',
            cursor:'pointer'
        },
        html: 'Play'
    });

    barPlayPause.addEvent('click', function() {
        player.playVideo()
    });

    player.addEvent('stateChange', function(state) {
        barPlayPause.empty();
        barPlayPause.removeEvents();
        if (state == 1) {
            barPlayPause.set('html', 'Pause');
            barPlayPause.addEvent('click', function() {
                player.pauseVideo();
            });
        } else {
            barPlayPause.set('html', 'Play');
            barPlayPause.addEvent('click', function() {
                player.playVideo();
            });
        }
    });
    
    bar.grab(barPlayPause);
    bar.grab(barSeparator());

    // -------------------------------------------------------------------------
    // Volume
    var barVolume = new Element('div', {
        styles: {
            float: 'left',
        },
    });

    var barVolumeMute = new Image();
    barVolumeMute.src = '../images/youtubeplayer-article-volume.png';
    barVolumeMute.mute = false;
    barVolumeMute.setStyle('cursor', 'pointer');

    var barVolumeKnob = new Element('div', {
        styles: {
            width: '16px',
            height: '3px',
            margin: '0 0 0 4px',
            'background-color': 'white',
            border: '1px solid #476871',
            'display': 'none',
            'cursor': 'pointer'
        }
    });

    barVolumeSlider = new Element('div', {
        styles: {
            float: 'left',
            position: 'absolute',
            'background-color': 'black'
        },
    });

    barVolumeMute.addEvent('mouseenter', function() {
        barVolumeSlider.setStyle('height', '100px');
        barVolumeKnob.setStyle('display', 'block');
    });

    barVolumeSlider.addEvent('mouseleave', function() {
        barVolumeSlider.setStyle('height', '28px');
        barVolumeKnob.setStyle('display', 'none');
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

    barVolumeSlider.grab(barVolumeMute);
    barVolumeSlider.grab(barVolumeKnob);

    barVolume.grab(barVolumeSlider);

    new Slider(barVolumeSlider, barVolumeKnob, {
        mode: 'vertical'
    });

    bar.grab(barVolume);

    $('player').grab(bar);
});