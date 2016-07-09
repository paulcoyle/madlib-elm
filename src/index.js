require( './css/main.styl' );

var Elm = require('./Main');
var app = Elm.Main.embed(document.getElementById('main'));

app.ports.checkTextareaHeight.subscribe(function(id) {
  var textarea = document.getElementById(id);

  if (textarea && textarea.scrollHeight) {
    // https://github.com/elm-lang/core/issues/595
    setTimeout(function() {
      app.ports.textareaHeight.send([id, textarea.scrollHeight]);
    }, 0);
  }
});
