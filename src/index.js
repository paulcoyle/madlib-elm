import { fetchNlpData, simplifyNlpData } from './corenlp.js'

const Elm = require('./app/Main')

require('./css/main.styl')

let svgFiles = require.context('./svg', true, /.+\.svg$/)
svgFiles.keys().forEach(svgFiles)

let app = Elm.Main.embed(document.getElementById('main'))

require.ensure([], function(require) {
  let adjectives = require('./corpus/adj.json')
  app.ports.corpus.send(corpusToElm("Adjective", adjectives))
})
require.ensure([], function(require) {
  let nouns = require('./corpus/noun.json')
  app.ports.corpus.send(corpusToElm("Noun", nouns))
})
require.ensure([], function(require) {
  let verbs = require('./corpus/verb.json')
  app.ports.corpus.send(corpusToElm("Verb", verbs))
})

setTimeout(() => {
  app.ports.seed.send(Date.now())
}, 0)

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
})

app.ports.positionControls.subscribe((id) => {
  let frag = document.getElementById(id)
  let controls = document.getElementById('frag-controls')

  if (!frag || !controls) {
    return
  }

  // Group measurements to minimize reflows
  let fragDims = measure(frag)
  let controlDims = measure(controls)
  let parentDims = measure(controls.parentNode)

  let x = centerHorizontal(fragDims, controlDims)
  let y = fragDims.y + fragDims.h + 5

  if (x < 0) {
    x = 0
  } else if ((x + controlDims.w) > parentDims.w) {
    x = parentDims.w - controlDims.w
  }

  if (y > (parentDims.h - controlDims.h)) {
    y = fragDims.y - controlDims.h - 5
  }

  controls.style.transform = `translate(${x}px, ${y}px)`
})

function measure(element) {
  return {
    x: element.offsetLeft,
    y: element.offsetTop,
    w: element.offsetWidth,
    h: element.offsetHeight
  }
}

function centerHorizontal(target, positioned) {
  return target.x - ((positioned.w - target.w) / 2)
}

function corpusToElm(name, corpus) {
  return [
    name,
    corpus.map((c) => [c.length, c.words])
  ]
}
