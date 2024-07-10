run:
	npx elm-live src/Main.elm

build:
	elm make src/Main.elm --output public/index.html