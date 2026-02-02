;;; sw-filetypes.el --- Additional file type support -*- lexical-binding: t -*-

;;; Commentary:
;; Mode associations and configurations for various file types.

;;; Code:

;;; SQL

(use-package sql
  :ensure nil
  :mode (("\\.sql\\'" . sql-mode)
         ("\\.mysql\\'" . sql-mode)
         ("\\.pgsql\\'" . sql-mode))
  :custom
  (sql-mysql-options '("--ssl-mode=DISABLED"))
  (sql-mysql-login-params '((user :default "root")
                            password database
                            (server :default "127.0.0.1")
                            (port :default 3306)))
  (sql-postgres-login-params '((user :default "postgres")
                               password database
                               (server :default "127.0.0.1")
                               (port :default 5432))))

;;; Makefile

(use-package make-mode
  :ensure nil
  :mode ("Makefile.*" . makefile-mode))

;;; Additional file type associations

;; Poetry lock files are TOML
(add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . conf-toml-mode))

;; ROS (Robot Operating System) launch files are XML
(add-to-list 'auto-mode-alist '("\\.launch\\'" . xml-mode))

;; Conky config files are Lua
(add-to-list 'auto-mode-alist '("conky\\.conf\\'" . lua-mode))

;; Djot (use markdown-mode as syntax is similar)
(add-to-list 'auto-mode-alist '("\\.dj\\'" . markdown-mode))

(provide 'sw-filetypes)
;;; sw-filetypes.el ends here
