/**
 * This is just a simple interface to the Stanford CoreNLP hosted
 * web api.  There is no documentation for it so this was simply
 * reverse engineered.  Thankfully, they allow CORS requests from
 * any origin! :)
 */
import 'whatwg-fetch'

export const typeNoun = 'NOUN'
export const typeVerb = 'VERB'
export const typeAdjective = 'ADJ'
export const typeNotApplicable = 'NA'

// Expects a string and returns a fetch Promise either resolving
// with the JSON result, or rejecting with no content.
export function fetchNlpData(content) {
  let settings = escape(JSON.stringify(getSettings()))
  let postData = new URLSearchParams(content) // yeah, they do it weird

  return fetch('http://corenlp.run/?properties=' + settings, {
    method: 'POST',
    body: postData
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
  return {
    kind: classifyByPos(token.pos),
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
