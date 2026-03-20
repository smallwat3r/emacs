((aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "MELPA" :protocol https :inherit t :depth
                treeless :ref "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "MELPA" :protocol https
                     :inherit t :depth treeless :ref "011e7b999552f3c0730035183a4a6dfb60c10182"))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo "rmuslimov/browse-at-remote" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless :ref
                             "38e5ffd77493c17c821fd88f938dbf42705a5158"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "a326a0575fe5ca574f6607557dbb8bd6ce83dbbd"))
 (cargo :source "elpaca-menu-lock-file" :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "7f8466063381eed05d4e222ce822b1dd44e3bf17"))
 (claude-code :source "elpaca-menu-lock-file" :recipe
              (:package "claude-code" :fetcher github :repo "stevemolitor/claude-code.el" :files
                        (:defaults (:exclude "install-deps.el")) :source "MELPA" :protocol https
                        :inherit t :depth treeless :host github :wait t :ref
                        "4a9914bd4161eb43f489820f9174c62390e5adc8"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
           (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "f8c2ef57e83af3d45e345e5c14089f2f9973682b"))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
                (:package "consult-eglot" :fetcher github :repo "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/corfu-*.el") :fetcher
                  github :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "d2a995c5c732d0fc439efe09440870a9de779a74"))
 (creamy-theme :source "elpaca-menu-lock-file" :recipe
               (:package "creamy-theme" :fetcher github :repo "smallwat3r/emacs-creamy-theme" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :wait t :ref
                         "d16902a91c28d616d01bb7bcfd7c6248415f5860"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (deft :source "elpaca-menu-lock-file" :recipe
       (:package "deft" :repo "jrblevin/deft" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "b369d7225d86551882568788a23c5497b232509c"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "bb9af85441b0cbb3281268d30256d50f0595ebfe"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo "Fuco1/dired-hacks" :files
                              ("dired-hacks-utils.el") :source "MELPA" :protocol https :inherit t
                              :depth treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
               (:package "dired-narrow" :fetcher github :repo "Fuco1/dired-hacks" :files
                         ("dired-narrow.el") :source "MELPA" :protocol https :inherit t :depth
                         treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
                (:package "dired-subtree" :fetcher github :repo "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "MELPA" :protocol https :inherit t :depth
                          treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (diredfl :source "elpaca-menu-lock-file" :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "fe72d2e42ee18bf6228bba9d7086de4098f18a70"))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "916686b86e83a3bd2281fbc5e6f98962aa747429"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*" (:exclude ".git")) :source "NonGNU ELPA" :protocol https :inherit t :depth
                treeless :ref "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo "editorconfig/editorconfig-emacs"
                         :old-names (editorconfig-core editorconfig-fnmatch) :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :ref
                         "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (eglot-booster :source "elpaca-menu-lock-file" :recipe
                (:source nil :protocol https :inherit t :depth treeless :host github :repo
                         "jdtsmith/eglot-booster" :package "eglot-booster" :ref
                         "cab7803c4f0adc7fff9da6680f90110674bb7a22"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "MELPA" :protocol https
                       :inherit t :depth treeless :ref "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "8ed83477f74bcae5019f0542fc5f774d8e20c041" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca"))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca--compile-info)
                               :source "Elpaca extensions" :protocol https :inherit t :depth
                               treeless :ref "8ed83477f74bcae5019f0542fc5f774d8e20c041"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "MELPA" :protocol https
                   :inherit t :depth treeless :ref "e0238889b1c946514fd967d21d70599af9c4e887"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
                           ("embark-consult.el") :source "MELPA" :protocol https :inherit t :depth
                           treeless :ref "e0238889b1c946514fd967d21d70599af9c4e887"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el")) :source
                 "MELPA" :protocol https :inherit t :depth treeless :wait t :ref
                 "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection"
                            :files (:defaults "modes") :source "MELPA" :protocol https :inherit t
                            :depth treeless :wait t :ref "0c63456ae73520839b66c3985de9bfabd38b835f"))
 (evil-goggles :source "elpaca-menu-lock-file" :recipe
               (:package "evil-goggles" :repo "edkolev/evil-goggles" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "MELPA" :protocol https :inherit t :depth treeless :ref
                         "34ca276a85f615d2b45e714c9f8b5875bcb676f3"))
 (evil-lion :source "elpaca-menu-lock-file" :recipe
            (:package "evil-lion" :fetcher github :repo "edkolev/evil-lion" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "5a0bca151466960e090d1803c4c5ded88875f90a"))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                                "redguardtoo/evil-nerd-commenter" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                                "ae52c5070a48793e2c24474c9c8dbf20175d18a0"))
 (evil-snipe :source "elpaca-menu-lock-file" :recipe
             (:package "evil-snipe" :repo "hlissner/evil-snipe" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "16317d7e54313490a0fe8642ed9a1a72498e7ad2"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (evil-textobj-tree-sitter :source "elpaca-menu-lock-file" :recipe
                           (:package "evil-textobj-tree-sitter" :fetcher github :repo
                                     "meain/evil-textobj-tree-sitter" :files
                                     (:defaults "queries" "treesit-queries") :old-names
                                     (evil-textobj-treesitter) :source "MELPA" :protocol https
                                     :inherit t :depth treeless :ref
                                     "7f58008a82c70eb1c6c5761db499f0be0db9d6cb"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode gitignore-mode) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (gotest :source "elpaca-menu-lock-file" :recipe
         (:package "gotest" :fetcher github :repo "nlamirault/gotest.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth treeless :ref
                   "490189e68d743a851bfb42d0017428a7550e8615"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-numbers :source "elpaca-menu-lock-file" :recipe
                    (:package "highlight-numbers" :fetcher github :repo "Fanael/highlight-numbers"
                              :old-names (number-font-lock-mode) :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth treeless :ref
                              "8b4744c7f46c72b1d3d599d4fb75ef8183dee307"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (imenu-list :source "elpaca-menu-lock-file" :recipe
             (:package "imenu-list" :repo "bmag/imenu-list" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "76f2335ee6f2f066d87fe4e4729219d70c9bc70d"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :host github :wait
                       t :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files ("llama.el" ".dir-locals.el")
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults (:exclude "init-tryout.el")) :source "MELPA" :protocol https
                     :inherit t :depth treeless :ref "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   ".dir-locals.el" ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "b9f19bae4d5e5c485d2d8d7bf52364eeb7d22a6b"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "d28a5e5c1a2e5f3e6669b0197f38da84e08f94a0"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "107a368a6deffa943544c220b5a6b1304ffc9945"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source "MELPA" :protocol https :inherit t :depth treeless
                       :ref "9a7f44db9a53567f04603bc88d05402cad49c64c"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth treeless :ref
                             "929b62f01b93d30a3f42cc507fc45c84a2457b3f"))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "c4ac5de975d65c84893a130a470af32a48b0b66c"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :wait t :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (org-journal :source "elpaca-menu-lock-file" :recipe
              (:package "org-journal" :fetcher github :repo "bastibe/org-journal" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :ref
                        "831ecfd50a29057c239b9fa55ebc02d402a6d4a7"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth treeless :ref
                       "f514a2570da0f7a8ff0d72641458dbcf96ccf702"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el")) :source "MELPA" :protocol https
                         :inherit t :depth treeless :ref "1c37329703a507fa357302cf6fc29d4f2fe631a8"))
 (parent-mode :source "elpaca-menu-lock-file" :recipe
              (:package "parent-mode" :fetcher github :repo "Fanael/parent-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :ref
                        "fbd49857ab2e4cd0c5611c0cc83f93711657b298"))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth treeless :ref
                 "143456809fd2dbece9f241f4361085e1de0b0e75"))
 (password-store :source "elpaca-menu-lock-file" :recipe
                 (:package "password-store" :fetcher github :repo "zx2c4/password-store" :files
                           ("contrib/emacs/*.el") :source "MELPA" :protocol https :inherit t :depth
                           treeless :ref "3ca13cd8882cae4083c1c478858adbf2e82dd037"))
 (password-store-otp :source "elpaca-menu-lock-file" :recipe
                     (:package "password-store-otp" :repo "volrath/password-store-otp.el" :fetcher
                               github :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth treeless :ref
                               "be3a00a981921ed1b2f78012944dc25eb5a0beca"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile") ("build" "server")) :source "MELPA"
                      :protocol https :inherit t :depth treeless :ref
                      "365f88238f46f9b1425685562105881800f10386"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :protocol https :inherit t :depth treeless :ref
                "222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158"))
 (pinentry :source "elpaca-menu-lock-file" :recipe
           (:package "pinentry" :repo ("https://github.com/ueno/pinentry-el.git" . "pinentry")
                     :files ("*" (:exclude ".git")) :source "GNU ELPA" :protocol https :inherit t
                     :depth treeless :ref "0079964a1dde954ccb2ce8a28613d8020c549a36"))
 (python-pytest :source "elpaca-menu-lock-file" :recipe
                (:package "python-pytest" :fetcher github :repo "wbolster/emacs-python-pytest"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "78b5ea1d19c7e365ac00649d13c733954b11f822"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "MELPA" :protocol https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (restart-emacs :source "elpaca-menu-lock-file" :recipe
                (:package "restart-emacs" :fetcher github :repo "iqbalansari/restart-emacs" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth treeless :ref
                          "1607da2bc657fe05ae01f7fdf26f716eafead02c"))
 (ros-mode :source "elpaca-menu-lock-file" :recipe
           (:source nil :protocol https :inherit t :depth treeless :host github :repo
                    "smallwat3r/emacs-ros-mode" :package "ros-mode" :ref
                    "02968f75ccc7451de1a5bfd4e335cf156dda4823"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo "emacsorphanage/restclient" :files
                       ("restclient.el") :source "MELPA" :protocol https :inherit t :depth treeless
                       :ref "1800a4e367c250051617d0b8c16a7cbd7f47da69"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "74eb1e6986461baed9a9269566ff838530b4379b"))
 (symbol-overlay :source "elpaca-menu-lock-file" :recipe
                 (:package "symbol-overlay" :fetcher github :repo "wolray/symbol-overlay" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth treeless :ref
                           "6151f4279bd94b5960149596b202cdcb45cacec2"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode" :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "MELPA" :protocol https :inherit t :depth treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (toc-org :source "elpaca-menu-lock-file" :recipe
          (:package "toc-org" :fetcher github :repo "snosov1/toc-org" :old-names (org-toc) :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :wait t :ref
                      "7131bec61e558e022ce75e2d2d5e55c748fcf8e0"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files (:defaults "extensions/vertico-*.el")
                    :fetcher github :source "MELPA" :protocol https :inherit t :depth treeless :ref
                    "0b96e8f169653cba6530da1ab0a1c28ffa44b180"))
 (warm-mode :source "elpaca-menu-lock-file" :recipe
            (:source nil :protocol https :inherit t :depth treeless :host github :repo
                     "smallwat3r/emacs-warm-mode" :package "warm-mode" :ref
                     "27362826e970ed0e902bee3512d97dc02f196a7b"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth treeless :ref
                     "1e7694aee87722f9e51b6e39c35d175d83a1fb2c"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "MELPA" :protocol https :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth treeless :ref
                        "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git" :branch
                      "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth treeless :ref
                      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA" :protocol https :inherit t :depth
                      treeless :ref "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo "AndreaCrotti/yasnippet-snippets" :fetcher
                               github :files ("*.el" "snippets" ".nosearch") :source "MELPA"
                               :protocol https :inherit t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86")))
