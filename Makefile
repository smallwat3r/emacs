EMACS ?= emacs
INIT := ~/.emacs.d/init.el

.PHONY: help link install update freeze thaw rebuild clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

link: ## Symlink this directory to ~/.emacs.d
	@if [ -e ~/.emacs.d ]; then \
		echo "~/.emacs.d already exists, remove it first"; exit 1; \
	fi
	ln -s $(CURDIR) ~/.emacs.d
	@echo "Linked $(CURDIR) -> ~/.emacs.d"

install: ## Install packages (first run or from lockfile)
	$(EMACS) --batch -l $(INIT)

update: ## Pull latest versions and freeze
	$(EMACS) --batch -l $(INIT) --eval "(straight-pull-all)"
	$(EMACS) --batch -l $(INIT) --eval "(straight-freeze-versions)"

freeze: ## Save current package versions to lockfile
	$(EMACS) --batch -l $(INIT) --eval "(straight-freeze-versions)"

thaw: ## Restore packages to lockfile versions
	$(EMACS) --batch -l $(INIT) --eval "(straight-thaw-versions)"

rebuild: ## Rebuild all packages
	$(EMACS) --batch -l $(INIT) --eval "(straight-rebuild-all)"

clean: ## Remove all installed packages
	rm -rf straight/build straight/repos
