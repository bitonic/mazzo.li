document.addEvent('domready', function() {
    var player = new YouTubePlayer({
        width: 680,
        height: 380,
        videoId: 'XDZ31YQvxWY',
        id: 'videoPlayer',
        embedded: false,
        suggestedQuality: 'large'
    });

    $('player').setStyles({
        'background-color': 'black',
        margin: '10px 0',
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

    $('player').grab(bar);

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
            height: '31px'
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
            float: 'left'
        },
    });

    var barVolumeMute = new Image();
    barVolumeMute.src = '../images/youtubeplayer-article-volume.png';
    barVolumeMute.mute = false;
    barVolumeMute.setStyle('cursor', 'pointer');

    var barVolumeContainer = new Element('div', {
        styles: {
            position: 'absolute',
            'background-color': 'black',
            width: '26px'
        },
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

    barVolumeMute.addEvent('mouseenter', function() {
        barVolumeSlider.setStyle('display', 'block');
    });

    barVolumeContainer.addEvent('mouseleave', function() {
        barVolumeSlider.setStyle('display', 'none');
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
    barVolumeSlider.setStyle('display', 'none');

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

    // To clear stuff...
    $('player').grab(new Element('hr', {
        styles: {
            clear: 'both',
            height: 0,
            visibility: 'hidden'
        }
    }));
});