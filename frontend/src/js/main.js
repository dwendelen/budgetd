import '../css/720_24.css';
import '../css/styles.css';

var Elm = require('../elm/Index.elm');

function uuid()
{
    var seed = Date.now();
    if (window.performance && typeof window.performance.now === "function") {
        seed += performance.now();
    }

    var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
        var r = (seed + Math.random() * 16) % 16 | 0;
        seed = Math.floor(seed/16);

        return (c === 'x' ? r : r & (0x3|0x8)).toString(16);
    });

    return uuid;
}

var node = document.getElementById('main');
var app = Elm.Main.embed(node);

//app.ports.requestUuid.subscribe(function() {
//    //var newUuid = uuid.v4();
//    var newUuid = uuid();
//    app.ports.receiveUuid.send(newUuid);
//});