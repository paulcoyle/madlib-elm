const fs = require('fs')

let done = false

packWordsInFile('wordsrc/adj.txt')
  .then(saveAsJson.bind(null, 'src/corpus/adj.json'))
  .then(() => packWordsInFile('wordsrc/noun.txt'))
  .then(saveAsJson.bind(null, 'src/corpus/noun.json'))
  .then(() => packWordsInFile('wordsrc/verb.txt'))
  .then(saveAsJson.bind(null, 'src/corpus/verb.json'))
  .then(() => {
    done = true
  })

areWeDone()

function areWeDone() {
  if (!done) {
    setTimeout(areWeDone, 100)
  }
}

function packWordsInFile(filePath) {
  return new Promise(function (resolve, reject) {
    let rs = fs.createReadStream(filePath, { encoding: 'utf8' })
    let chunkRemainder = ''
    let wordBins = {}

    rs.on('readable', () => {
      let chunk = rs.read()

      if (chunk === null) {
        return
      }

      let lastNewLine = chunk.lastIndexOf('\n')
      let parseableLines = chunkRemainder + chunk.slice(0, lastNewLine)

      chunkRemainder = chunk.slice(lastNewLine + 1)

      parseableLines
        .split('\n')
        .map(extractWord)
        .filter(isWordLengthMoreThan(1))
        .filter(isWordLengthLessThan(30))
        .reduce(binWordsByLength, wordBins)
    })

    rs.on('error', reject)

    rs.on('end', () => {
      resolve(convertToElmFriendlyBins(wordBins))
    })
  })
}

function extractWord(line) {
  return line.slice(0, line.indexOf(' ')).replace(/_/g, ' ')
}

function isWordLengthMoreThan(length) {
  return (word) => word.length > length
}

function isWordLengthLessThan(length) {
  return (word) => word.length < length
}

function isWordMoreThanOneLetter(word) {
  return word.length > 1
}

function binWordsByLength(accum, word) {
  let length = word.length

  if (!accum.hasOwnProperty(length)) {
    accum[length] = []
  }

  accum[length].push(word)

  return accum
}

function convertToElmFriendlyBins(binnedWords) {
  return Object.keys(binnedWords)
    .map((key) => {
      return {
        length: parseInt(key),
        words: binnedWords[key]
      }
    })
}

function saveAsJson(filePath, data) {
  fs.writeFileSync(filePath, JSON.stringify(data), { encoding: 'utf8' })
  return
}
