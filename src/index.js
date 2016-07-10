require( './css/main.styl' );

var svgFiles = require.context('./svg', true, /.+\.svg$/);
svgFiles.keys().forEach(svgFiles);

var Elm = require('./Main');
var app = Elm.Main.embed(document.getElementById('main'));
