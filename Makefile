EMACS ?= emacs

.PHONY: help link clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

link: ## Symlink this directory to ~/.emacs.d
	@if [ -e ~/.emacs.d ]; then \
		echo "~/.emacs.d already exists, remove it first"; exit 1; \
	fi
	ln -s $(CURDIR) ~/.emacs.d
	@echo "Linked $(CURDIR) -> ~/.emacs.d"

clean: ## Remove all installed packages
	rm -rf elpaca/
