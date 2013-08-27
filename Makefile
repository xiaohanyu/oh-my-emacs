# Name of your emacs binary
EMACS=emacs

BATCH=$(EMACS) -batch -Q --eval '(require (quote org))' --eval '(setq ome-dir default-directory)'

FILES = ome.org

FILESO = $(FILES:.org=.el)

all: el
	$(BATCH) --eval '(mapc (lambda (x) (byte-compile-file (symbol-name x))) (quote ($(FILESO))))'

el: $(FILES)
	$(BATCH) --eval '(mapc (lambda (x) (org-babel-load-file (symbol-name x))) (quote ($(FILES))))'

%.el: %.org
	$(BATCH) --eval '(org-babel-load-file "$<")'

doc: doc/index.html

doc/index.html:
	mkdir -p doc
	$(EMACS) --batch -Q --eval '(org-babel-load-file "ome-publish.org")'
	rm ome-publish.el
	cp doc/ome.html doc/index.html
	echo "Documentation published to doc/"

clean:
	rm -f *.elc *.aux *.tex *.pdf ome*.el ome*.html doc/*html *~ .ome*.part.org
