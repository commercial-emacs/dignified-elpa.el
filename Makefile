export VERSION := $(shell git describe --tags --abbrev=0 2>/dev/null || echo 0.0.1)
SHELL := /bin/bash
EMACS ?= emacs
ELSRC := dignified-elpa.el
VERSION 
.PHONY: compile
compile: deps/archives/gnu/archive-contents
	$(EMACS) -batch \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) && exit $$ret)

deps/archives/gnu/archive-contents: $(ELSRC)
	$(call install-recipe,$(CURDIR)/deps)
	rm -rf deps/dignified-elpa*

.PHONY: dist-clean
dist-clean:
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l dignified-elpa-package --eval "(princ (dignified-elpa-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L . -l dignified-elpa-package -f dignified-elpa-package-inception
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l dignified-elpa-package --eval "(princ (dignified-elpa-package-name))"`; \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	$(MAKE) dist
	( \
	set -e; \
	INSTALL_PATH=$(1); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	PKG_NAME=`$(EMACS) -batch -L . -l dignified-elpa-package --eval "(princ (dignified-elpa-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote dignified-elpa) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	)
	$(MAKE) dist-clean
endef

.PHONY: retag
retag:
	2>/dev/null git tag -d $(VERSION) || true
	2>/dev/null git push --delete origin $(VERSION) || true
	git tag $(VERSION)
	git push origin $(VERSION)

