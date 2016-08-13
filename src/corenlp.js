import 'whatwg-fetch'

export const typeNoun = 'NOUN'
export const typeVerb = 'VERB'
export const typeAdjective = 'ADJ'
export const typeNotApplicable = 'NA'

// Expects a string and returns a fetch Promise either resolving
// with the JSON result, or rejecting with no content.
export function fetchNlpData(content) {
  let settings = escape(JSON.stringify(getSettings()))

  return fetch('http://corenlp.corpsedisposal.com:9000/?properties=' + settings, {
    method: 'POST',
    body: content + '='
  })
  .then((response) => {
    return response.json()
  })
}

// Returns the most basic set of annotations; merely for use in
// this project.
function getSettings() {
  return {
    annotators: 'tokenize,ssplit,pos',
    date: date(),
    'coref.md.type': 'dep',
    'coref.mode': 'statistical'
  }
}

// Plucked from the CoreNLP client to ensure the date format is the same.
function date() {
  function f(n) {
    return n < 10 ? '0' + n : n;
  }
  var date = new Date();
  var M = date.getMonth() + 1;
  var D = date.getDate();
  var Y = date.getFullYear();
  var h = date.getHours();
  var m = date.getMinutes();
  var s = date.getSeconds();
  return "" + Y + "-" + f(M) + "-" + f(D) + "T" + f(h) + ':' + f(m) + ':' + f(s);
}

export function simplifyNlpData(data) {
  return data.sentences.reduce((accum, sentence) => {
    if (isGoodSentence(sentence)) {
      sentence.tokens.forEach((token) => {
        var converted = convertToken(token)

        if (converted.kind !== typeNotApplicable) {
           foldSpecialToken(accum, converted, token)
        } else {
           foldNormalToken(accum, converted, token)
        }
      })
    }

    return accum
  }, [])
    .map((token) => {
      if (token.kind === typeNotApplicable) {
        token.value = token.value.trim()
      }

      return token
    })
}

function foldSpecialToken(corpus, converted, original) {
  corpus.push(converted)
}

function foldNormalToken(corpus, converted, original) {
  appendToCorpus(corpus, original.before + converted.value + original.after)
}

function appendToCorpus(corpus, text) {
  if (text.length > 0) {
    var appendee = getOrCreateLastToken(corpus)
    appendee.value = (appendee.value + text).replace(/\s{2,}/, ' ')
  }
}

function getOrCreateLastToken(corpus) {
  let last = corpus[corpus.length - 1]

  if (!last || last.kind !== typeNotApplicable) {
    corpus.push(emptyToken())
  }

  return corpus[corpus.length - 1]
}

function convertToken(token) {
  let kind = typeNotApplicable

  // Contractions appear to produce a separate fragment for the
  // existential portion.  I don't want to deal with those.
  if (token.word.match(/^\'/) === null) {
    kind = classifyByPos(token.pos)
  }

  return {
    kind: kind,
    value: token.word
  }
}

function emptyToken() {
  return {
    kind: typeNotApplicable,
    value: ''
  }
}

function classifyByPos(pos) {
  if (pos.match(/^V/) !== null) {
    return typeVerb
  } else if (pos.match(/^N/) !== null) {
    return typeNoun
  } else if (pos.match(/^J/) !== null) {
    return typeAdjective
  } else {
    return typeNotApplicable
  }
}

// The service seems to always return an erroneously parsed
// sentence with an '=' as the first and only token.
function isGoodSentence(sentence) {
  return !(sentence.tokens.length === 1 &&
           sentence.tokens[0].originalText === '=')
}
