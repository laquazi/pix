# npx elm-watch hot

# https://github.com/benblamey/when_changed
# when_changed 'src/**.elm' build.ps1

elm-live src/Main.elm --pushstate --hot -- -- --output=elm.js
