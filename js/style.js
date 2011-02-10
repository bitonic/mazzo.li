// Various functions relative to the styling of the page.

var shadowHeight = 6;
var cookieName = 'effects';
var effects = [colorsTrans, colorsRun, colorsBlink, shadowColor];

window.addEvent('domready', function() {
    var title = $$('a')[0];

    getEffects()(title);
});

function getEffects() {
    var effectsCookie = Cookie.read(cookieName);
    
    if (effectsCookie) {

        var list = eval(effectsCookie);

        function fail() {
            Cookie.dispose(cookieName);
            return getEffects();
        }

        if (list.length > 0) {
            var ev = list.pop();
            
            if (typeOf(ev) == 'number' && ev >= 0 && ev < effects.length) {
                Cookie.write(cookieName, JSON.stringify(list));
                return effects[ev];
            } else {
                return fail();
            }
        } else {
            return fail();
        }

    } else {

        var i;
        var list = [];
        for (i = 0; i < effects.length; i++) {
            list[i] = i;
        }

        var j, t;
        for (i = 1; i < list.length; i++) {
            j = Math.floor(Math.random() * (1 + i));
            if (j != i) {
                t = list[i];
                list[i] = list[j];
                list[j] = t;
            }
        }

        var ev = list.pop();
        Cookie.write(cookieName, JSON.stringify(list));
        return effects[ev];
    }
}

function colorsTrans(el) {
    el.setStyle('color', nextColor());

    var colTween = new Fx.Tween(el, {
        duration: '1000'
    });

    function setColor() {
        colTween.start('color', nextColor());
    }

    colTween.addEvent('complete', setColor);

    setColor();
}

function colorsRun(el) {
    var colors = [];
    var spans = spannify(el);

    for (var i = 0; i < spans.length; i++) {
        colors[i] = nextColor(200);
    }

    function changeColors() {
        for (var i = 0; i < spans.length; i++) {
            spans[i].setStyle('color', colors[i]);
        }

        colors.shift();
        colors.push(nextColor(200));

        setTimeout(changeColors, 100);
    };

    changeColors();
}

function colorsBlink(el) {
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

function shadowColor(el) {
    var colors = [];

    for (var i = 0; i < shadowHeight; i++) {
        colors[i] = nextColor();
    }

    function colorShadow() {
        colors.shift();
        colors.push(nextColor());

        var shadow = '1px 1px 0px black';

        for (var i = 0; i < shadowHeight; i++) {
            shadow += ', ' + (i + 2) + 'px ' + (i + 2) + 'px 0px ' + colors[i]; 
        }

        el.setStyle('text-shadow', shadow);

        setTimeout(colorShadow, 100);
    };

    colorShadow();
}

function spannify(el) {
    var spans = [];
    
    for (var i = 0; i < el.get('text').length; i++) {
        spans[i] = new Element('span', {
            text: el.get('text').charAt(i)
        });
    }

    el.empty();

    for (var i = 0; i < spans.length; i++) {
        el.grab(spans[i]);
    }

    return spans;
}

function nextColor(treshold_up, treshold_down) {
    if (treshold_up == undefined) {
        treshold_up = 256;
    }
    if (treshold_down == undefined) {
        treshold_down = 0;
    }

    var r = Math.floor(Math.random() * 256);
    var g = Math.floor(Math.random() * 256);
    var b = Math.floor(Math.random() * 256);

    if (r + g + b > treshold_up || r + g + b < treshold_down) {
        return nextColor(treshold_up, treshold_down);
    }

    return 'rgb(' + r + ',' + g + ',' + b + ')';
}


// function shadowHeights(el) {
//     var spans = [];

//     for (var i = 0; i < el.get('text').length; i++) {
//         spans[i] = new Element('span', {
//             text: el.get('text').charAt(i)
//         });
//     }

//     el.empty();

//     el.setStyle('text-shadow', 'none');

//     for (var i = 0; i < spans.length; i++) {
//         el.grab(spans[i]);
//     }        

//     var buildShadow = function(height) {
//         var shadow = '1px 1px 0px black';
        
//         for (var i = 0; i < height; i++) {
//             shadow += ', ' + (i + 2) + 'px ' + (i + 2) + 'px ' + '0px #476871';
//         }

//         return shadow;
//     };
    
//     function changeHeights() {
//         for (var i = 0; i < spans.length; i++) {
//             spans[i].setStyle('text-shadow',
//                               buildShadow(Math.floor(Math.random() * (shadowHeight + 1))));
//         }        
//         setTimeout(changeHeights, 300);
//     }

//     changeHeights();
// }
