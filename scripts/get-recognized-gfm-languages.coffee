###

linguist project: https://github.com/github/linguist

this script downloads the yaml file with all recognized languages in github
markdown from linguist, parses it, and prints it into a sexp on stdout.

to run this script, you need to install node, npm, and coffeescript. check out
https://nodejs.org for node and npm. once that's done, run:
npm install -g coffee

run this script with:
coffee get-recognized-gfm-languages.coffee

###

https = require 'https'

linguistUrl = 'https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml'

parseYamlKeys = (fileText) -> fileText.match(/^[^#\s]+(?=:)/gm) or []

makeSexp = (keys) -> "'(\"#{keys.join '"\n  "'}\")\n"

https.get(linguistUrl, (resp) ->
  str = ''
  resp.on('data', (chunk) -> str += chunk).on 'end', ->
    process.stdout.write makeSexp parseYamlKeys str
  ).on 'error', (err) -> console.error err
