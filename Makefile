# Name of your emacs binary
EMACS=emacs

BATCH=$(EMACS) --batch -Q --eval '(require (quote org))' --eval '(setq ome-dir default-directory)'

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
	$(BATCH) --batch -Q --eval '(org-babel-load-file "ome-publish.org")'
	rm ome-publish.el
	cp doc/README.html doc/index.html
	echo "Documentation published to doc/"

tarball:
	tar czf oh-my-emacs.tar.gz \
		--exclude-backups \
		core el-get modules ome-el-get-recipes \
		CHANGELOG.org CONTRIBUTING.org custom.el init.el \
		LICENSE.txt Makefile ome.org ome-publish.org README.org

clean:
	rm -f *.elc *.aux *.tex *.pdf ome*.el ome*.html doc/*html *~ .ome*.part.org
