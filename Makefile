EMACS ?= emacs

.PHONY: help link check clean

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-10s %s\n", $$1, $$2}'

link: ## Symlink this directory to ~/.emacs.d
	@if [ -e ~/.emacs.d ]; then \
		echo "~/.emacs.d already exists, remove it first"; exit 1; \
	fi
	ln -s $(CURDIR) ~/.emacs.d
	@echo "Linked $(CURDIR) -> ~/.emacs.d"

check: ## Smoke test: load config and activate deferred modes in batch
	$(EMACS) --batch --init-directory=$(CURDIR) \
		--eval '(setq debug-on-error t)' \
		-l $(CURDIR)/early-init.el -l $(CURDIR)/init.el \
		--eval '(elpaca-wait)' \
		--eval '(setq recentf-save-file (make-temp-name "/tmp/check-recentf"))' \
		--eval '(setq savehist-file (make-temp-name "/tmp/check-savehist"))' \
		--eval '(setq save-place-file (make-temp-name "/tmp/check-places"))' \
		--eval "(run-hooks 'sw-first-input-hook 'sw-first-file-hook 'sw-first-buffer-hook)" \
		--eval '(message "Config loaded OK")'

clean: ## Remove all installed packages
	rm -rf elpaca/ elpa/ eln-cache/
