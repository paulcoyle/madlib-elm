import { fetchNlpData, simplifyNlpData } from './corenlp.js'

const Elm = require('./Main');

require( './css/main.styl' );

let svgFiles = require.context('./svg', true, /.+\.svg$/);
svgFiles.keys().forEach(svgFiles);

let app = Elm.Main.embed(document.getElementById('main'));

app.ports.parse.subscribe((tup) => {
  let id = tup[0]
  let content = tup[1]

  fetchNlpData(content)
    .then((data) => {
      app.ports.parsed.send([id, true, simplifyNlpData(data)]);
    })
    .catch(() => {
      app.ports.parsed.send([id, false, []]);
    })
});
