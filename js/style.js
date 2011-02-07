// Various functions relative to the styling of the page.

window.addEvent('domready', function() {
    var title = $$('a')[0];

    var rand = Math.random();
    if (rand >= 0.66) {
        colorsTrans(title);
    } else if (rand >= 0.33) {
        colorsRun(title);
    } else {
        colorBlink(title);
    }
});

function colorsTrans(el) {
    el.setStyle('color', nextColor());

    var colTween = new Fx.Tween(el, {
        duration: '1000'
    });

    var setColor = function() {
        colTween.start('color', nextColor());
    }

    colTween.addEvent('complete', setColor);

    setColor();
}

function colorsRun(el) {
    var colors = [];
    var spans = [];

    for (var i = 0; i < el.get('text').length; i++) {
        colors[i] = nextColor();
        spans[i] = new Element('span', {
            text: el.get('text').charAt(i)
        });
    }

    el.empty();

    for (var i = 0; i < spans.length; i++) {
        el.grab(spans[i]);
    }

    var changeColors = function() {
        for (var i = 0; i < spans.length; i++) {
            spans[i].setStyle('color', colors[i]);
        }

        var c = colors.shift();
        colors.push(nextColor());

        setTimeout(changeColors, 100);
    };

    changeColors();
}

function colorBlink(el) {
    var colTween = new Fx.Tween(el, {
        duration: '200'
    });
    
    var blackTween = new Fx.Tween(el, {
        duration: '200'
    });

    colTween.addEvent('complete', function() {
        setTimeout(function() {
            blackTween.start('color', document.body.getStyle('background-color'));
        }, 200);
    });

    blackTween.addEvent('complete', function() {
        colTween.start('color', nextColor());
    });

    blackTween.start('color', document.body.getStyle('background-color'));
}

function nextColor() {
    var r = Math.floor(Math.random() * 256)
    var g = Math.floor(Math.random() * 256)
    var b = Math.floor(Math.random() * 256)

    if (r + g + b > 200) {
        return nextColor();
    }

    return '#' + r.toString(16) + g.toString(16) + b.toString(16);
}