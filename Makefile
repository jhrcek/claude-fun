.PHONY: build
build:
	elm make src/Main.elm --optimize --output=docs/js/elm.js
	uglifyjs docs/js/elm.js \
	    --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output docs/js/elm.js

.PHONY: live
live:
	elm-live --open --dir=docs -- src/Main.elm --output=docs/js/elm.js --debug

.PHONY: format
format:
	elm-format --yes src

.PHONY: test
test:
	elm-test

.PHONY: review
review:
	elm-review
