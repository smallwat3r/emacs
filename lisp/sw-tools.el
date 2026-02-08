;;; sw-tools.el --- Miscellaneous tools -*- lexical-binding: t -*-

;;; Commentary:
;; Various utilities and tools.

;;; Code:

;;; Browser configuration

(when sw-is-linux
  (setq browse-url-browser-function 'browse-url-xdg-open))

;;; Goto address - highlight URLs and email addresses

(use-package goto-addr
  :ensure nil
  :hook ((prog-mode text-mode) . goto-address-mode)
  :custom
  (goto-address-url-face 'link)
  (goto-address-mail-face 'link))

;;; Restart Emacs

(use-package restart-emacs
  :commands restart-emacs)

;;; Sudo edit

(use-package sudo-edit
  :commands (sudo-edit sudo-edit-find-file))

;;; PDF viewing

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  :config
  (pdf-tools-install :no-query))

;;; Password management

(use-package pass
  :commands pass)

(use-package password-store
  :commands (password-store-copy
             password-store-get))

;; Auth source for pass
(use-package auth-source-pass
  :ensure nil
  :defer 2
  :config
  (auth-source-pass-enable))

;;; Rest client

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;;; Imenu list

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-size 0.2)
  :commands imenu-list-smart-toggle)

;;; Pinentry for GPG

(use-package pinentry
  :defer 2
  :config
  (setenv "GPG_AGENT_INFO" nil)
  (pinentry-start))

;;; Helpful - better help buffers

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point))

;;; Whitespace cleanup (only on edited lines)

(use-package ws-butler
  :hook ((prog-mode text-mode) . ws-butler-mode))

(provide 'sw-tools)
;;; sw-tools.el ends here
