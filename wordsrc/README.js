You can use `wordpack.js` in the project root to extract words in
three files in this directory: `adj.txt`, `noun.txt`, and `verb.txt`.
The caveat is that these files need to be formatted in a particular
way.  In making this project, I used raw dumps from the WordNet
project so, in order for `wordpack.js` to work without modifications
the files must have one word on each line terminated by a space
followed by a newline, like so:

    abacus<SPC>\n
    abba<SPC>\n
    zaxxon<SPC>\n
