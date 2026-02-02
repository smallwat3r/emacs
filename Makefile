EMACS ?= emacs
INIT := ~/.emacs.d/init.el

.PHONY: help link install update lock clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

link: ## Symlink this directory to ~/.emacs.d
	@if [ -e ~/.emacs.d ]; then \
		echo "~/.emacs.d already exists, remove it first"; exit 1; \
	fi
	ln -s $(CURDIR) ~/.emacs.d
	@echo "Linked $(CURDIR) -> ~/.emacs.d"

install: ## Install packages (first run)
	$(EMACS) --batch -l $(INIT)

update: ## Update all packages and save lockfile
	$(EMACS) --batch -l $(INIT) \
		--eval "(elpaca-update-all t)" \
		--eval "(elpaca-wait)" \
		--eval "(elpaca-write-lockfile elpaca-lockfile)"

lock: ## Save current package versions to lockfile
	$(EMACS) --batch -l $(INIT) \
		--eval "(elpaca-wait)" \
		--eval "(elpaca-write-lockfile elpaca-lockfile)"

clean: ## Remove all installed packages
	rm -rf elpaca/
