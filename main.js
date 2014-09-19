window.onload = function () {
    
    Elm.fullscreen(Elm.Coax, {
        size: { 
            width: window.innerWidth,
            height: window.innerHeight
        },
        randomSeed: Date.now()
    });
};
