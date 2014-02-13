SRC=*/*.ls
LSC=lsc
BUNDLE=browserify -x prelude-ls

all:
	$(LSC) -c $(SRC)
	$(BUNDLE) src/main.js -o www/js/main.js

watch:
	pywatch "make" */*.ls
