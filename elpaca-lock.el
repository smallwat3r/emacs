((aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "elpaca-menu-lock-file" :id aio :type git
                :protocol https :inherit t :depth treeless :ref
                "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id annalist :type git :protocol https :inherit
                     t :depth treeless :ref "f83019d61b82a64e621d8dfd63a527a578ff0a11"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "elpaca-menu-lock-file"
                     :id apheleia :type git :protocol https :inherit t :depth treeless :ref
                     "14a0bb4454fb2cc3b5b377619288b742ce117da5"))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo "rmuslimov/browse-at-remote" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id browse-at-remote :type git
                             :protocol https :inherit t :depth treeless :ref
                             "38e5ffd77493c17c821fd88f938dbf42705a5158"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id cape :type git :protocol https :inherit t
                 :depth treeless :ref "73e0023e7fd9f224babfe8795eb9334dba8eec47"))
 (cargo :source "elpaca-menu-lock-file" :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id cargo :type git :protocol https :inherit t
                  :depth treeless :ref "7f8466063381eed05d4e222ce822b1dd44e3bf17"))
 (claude-code :source "elpaca-menu-lock-file" :recipe
              (:package "claude-code" :fetcher github :repo "stevemolitor/claude-code.el" :files
                        (:defaults (:exclude "install-deps.el")) :source "elpaca-menu-lock-file" :id
                        claude-code :host github :wait t :type git :protocol https :inherit t :depth
                        treeless :ref "03199df8b3a1e9cd4857f0851f7a912ba524aff3"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo ("https://github.com/emacs-compat/compat" . "compat") :files
                   ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id compat :wait t :type
                   git :protocol https :inherit t :depth treeless :ref
                   "a0d646554730471579de8b33b0194077fd05abe1"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
           (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id cond-let :type git :protocol https :inherit
                     t :depth treeless :ref "21b9e9835756ff5cd1acb971cf9eb56fff671c8b"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id consult :type git :protocol https :inherit t
                    :depth treeless :ref "5849e82baeaff378b5e2d88c5d81cf7d314d43aa"))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
                (:package "consult-eglot" :fetcher github :repo "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id consult-eglot :type git :protocol
                          https :inherit t :depth treeless :ref
                          "3e4d9a40911b897c0a2c5d20199d0f7c30bfc1c2"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/corfu-*.el") :fetcher
                  github :source "elpaca-menu-lock-file" :id corfu :type git :protocol https
                  :inherit t :depth treeless :ref "c588350d8f9fc60ed0fcf4b78c37671835be01a9"))
 (creamy-theme :source "elpaca-menu-lock-file" :recipe
               (:package "creamy-theme" :fetcher github :repo "smallwat3r/emacs-creamy-theme" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id creamy-theme :wait t :type git
                         :protocol https :inherit t :depth treeless :ref
                         "d16902a91c28d616d01bb7bcfd7c6248415f5860"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source "elpaca-menu-lock-file" :id dash :type git :protocol https :inherit t
                 :depth treeless :ref "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (deft :source "elpaca-menu-lock-file" :recipe
       (:package "deft" :repo "jrblevin/deft" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id deft :type git :protocol https :inherit t
                 :depth treeless :ref "b369d7225d86551882568788a23c5497b232509c"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id diff-hl :type git :protocol https :inherit t
                    :depth treeless :ref "2d7d0714d9637f54af672987c65b6973b31e56a2"))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo "Fuco1/dired-hacks" :files
                              ("dired-hacks-utils.el") :source "elpaca-menu-lock-file" :id
                              dired-hacks-utils :type git :protocol https :inherit t :depth treeless
                              :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
               (:package "dired-narrow" :fetcher github :repo "Fuco1/dired-hacks" :files
                         ("dired-narrow.el") :source "elpaca-menu-lock-file" :id dired-narrow :type
                         git :protocol https :inherit t :depth treeless :ref
                         "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
                (:package "dired-subtree" :fetcher github :repo "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "elpaca-menu-lock-file" :id dired-subtree
                          :type git :protocol https :inherit t :depth treeless :ref
                          "de9336f4b47ef901799fe95315fa080fa6d77b48"))
 (diredfl :source "elpaca-menu-lock-file" :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id diredfl :type git :protocol https :inherit t
                    :depth treeless :ref "fe72d2e42ee18bf6228bba9d7086de4098f18a70"))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id docker :type git :protocol https :inherit t
                   :depth treeless :ref "09132b633ace9928a4838f743dd3bf5aa9829e1a"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id dockerfile-mode :type git :protocol
                            https :inherit t :depth treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat") :tar "0.9.4" :host
                nongnu :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id eat :type
                git :protocol https :inherit t :depth treeless :ref
                "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99"))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo "editorconfig/editorconfig-emacs"
                         :old-names (editorconfig-core editorconfig-fnmatch) :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id editorconfig :type git :protocol https
                         :inherit t :depth treeless :ref "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79"))
 (eglot-booster :source "elpaca-menu-lock-file" :recipe
                (:source "elpaca-menu-lock-file" :package "eglot-booster" :id eglot-booster :host
                         github :repo "jdtsmith/eglot-booster" :type git :protocol https :inherit t
                         :depth treeless :ref "510f579409627c333ef0e9157db713b1004da842"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "elpaca-menu-lock-file"
                       :id elisp-refs :type git :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo "https://github.com/progfolio/elpaca.git" :ref
            "fedf24160150b0195a42999b3bb53748217b779c" :depth 1 :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build (:not elpaca-activate) :type
            git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id elpaca-use-package :type git
                               :protocol https :inherit t :depth treeless :ref
                               "fedf24160150b0195a42999b3bb53748217b779c"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "elpaca-menu-lock-file" :id
                   embark :type git :protocol https :inherit t :depth treeless :ref
                   "350ca86924c5027e80875943fba7b912a71e5791"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
                           ("embark-consult.el") :source "elpaca-menu-lock-file" :id embark-consult
                           :type git :protocol https :inherit t :depth treeless :ref
                           "350ca86924c5027e80875943fba7b912a71e5791"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el")) :source
                 "elpaca-menu-lock-file" :id evil :wait t :type git :protocol https :inherit t
                 :depth treeless :ref "3b678a221ee99cc6a95b01d7a3129ce5efc4c3da"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection"
                            :files (:defaults "modes") :source "elpaca-menu-lock-file" :id
                            evil-collection :wait t :type git :protocol https :inherit t :depth
                            treeless :ref "162183159dde328336bf2c92cef66b4151df26cf"))
 (evil-goggles :source "elpaca-menu-lock-file" :recipe
               (:package "evil-goggles" :repo "edkolev/evil-goggles" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id evil-goggles :type git :protocol https
                         :inherit t :depth treeless :ref "34ca276a85f615d2b45e714c9f8b5875bcb676f3"))
 (evil-lion :source "elpaca-menu-lock-file" :recipe
            (:package "evil-lion" :fetcher github :repo "edkolev/evil-lion" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id evil-lion :type git :protocol https
                      :inherit t :depth treeless :ref "5a0bca151466960e090d1803c4c5ded88875f90a"))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                                "redguardtoo/evil-nerd-commenter" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id evil-nerd-commenter :type git
                                :protocol https :inherit t :depth treeless :ref
                                "db5ee61a6e75db074b7d20e9dcb68e0b94b4edc7"))
 (evil-snipe :source "elpaca-menu-lock-file" :recipe
             (:package "evil-snipe" :repo "hlissner/evil-snipe" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id evil-snipe :type git :protocol https
                       :inherit t :depth treeless :ref "16317d7e54313490a0fe8642ed9a1a72498e7ad2"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id evil-surround :type git :protocol
                          https :inherit t :depth treeless :ref
                          "e6548372e8359ee55e67d73ca418314086011f1a"))
 (evil-textobj-tree-sitter :source "elpaca-menu-lock-file" :recipe
                           (:package "evil-textobj-tree-sitter" :fetcher github :repo
                                     "meain/evil-textobj-tree-sitter" :files
                                     (:defaults "queries" "treesit-queries") :old-names
                                     (evil-textobj-treesitter) :source "elpaca-menu-lock-file" :id
                                     evil-textobj-tree-sitter :type git :protocol https :inherit t
                                     :depth treeless :ref "fecc0e11615df31a6651ce11b012388e53cad4e9"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id exec-path-from-shell :type git
                                 :protocol https :inherit t :depth treeless :ref
                                 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id f :type git :protocol https :inherit t :depth
              treeless :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id gcmh :type git :protocol https :inherit t
                 :depth treeless :ref "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode gitignore-mode) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id git-modes :type git :protocol https
                      :inherit t :depth treeless :ref "f291a4cc4a8b02a25d5cf93b4ab6af29e6f060d9"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id git-timemachine :type git :protocol
                            https :inherit t :depth treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (gotest :source "elpaca-menu-lock-file" :recipe
         (:package "gotest" :fetcher github :repo "nlamirault/gotest.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id gotest :type git :protocol https :inherit t
                   :depth treeless :ref "490189e68d743a851bfb42d0017428a7550e8615"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id goto-chg :type git :protocol https :inherit
                     t :depth treeless :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id hcl-mode :type git :protocol https :inherit
                     t :depth treeless :ref "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id helpful :type git :protocol https :inherit t
                    :depth treeless :ref "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (highlight-numbers :source "elpaca-menu-lock-file" :recipe
                    (:package "highlight-numbers" :fetcher github :repo "Fanael/highlight-numbers"
                              :old-names (number-font-lock-mode) :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id highlight-numbers :type git
                              :protocol https :inherit t :depth treeless :ref
                              "8b4744c7f46c72b1d3d599d4fb75ef8183dee307"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id hl-todo :type git :protocol https :inherit t
                    :depth treeless :ref "527d545b8c2f36243194cbe4a8d0e6ac9d50e6a7"))
 (imenu-list :source "elpaca-menu-lock-file" :recipe
             (:package "imenu-list" :repo "bmag/imenu-list" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id imenu-list :type git :protocol https
                       :inherit t :depth treeless :ref "76f2335ee6f2f066d87fe4e4729219d70c9bc70d"))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id inheritenv :host github :wait t :type git
                       :protocol https :inherit t :depth treeless :ref
                       "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files ("llama.el" ".dir-locals.el")
                  :source "elpaca-menu-lock-file" :id llama :type git :protocol https :inherit t
                  :depth treeless :ref "4d4024048053b898a01521046e0f063ee47615b0"))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults (:exclude "init-tryout.el")) :source "elpaca-menu-lock-file" :id
                     lua-mode :type git :protocol https :inherit t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   ".dir-locals.el" ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git :protocol https :inherit t
                  :depth treeless :ref "55bbdde3fec0fcd486cc42a48c634e91b571ef45"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :id magit-section :type git :protocol
                          https :inherit t :depth treeless :ref
                          "55bbdde3fec0fcd486cc42a48c634e91b571ef45"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id marginalia :type git :protocol https
                       :inherit t :depth treeless :ref "7604eb0463994b0528b92aaf6e594f64089b8b95"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id markdown-mode :type git :protocol
                          https :inherit t :depth treeless :ref
                          "1f72cefa6a4b759f90e335e4908725a721b17ad9"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source "elpaca-menu-lock-file" :id nerd-icons :type git
                       :protocol https :inherit t :depth treeless :ref
                       "d7742c5e8fba5d601633dd46f4cd7b34928f1185"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id nerd-icons-dired :type git
                             :protocol https :inherit t :depth treeless :ref
                             "104acd8879528b8115589f35f1bbcbe231ad732f"))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id nginx-mode :type git :protocol https
                       :inherit t :depth treeless :ref "c4ac5de975d65c84893a130a470af32a48b0b66c"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id orderless :wait t :type git :protocol
                      https :inherit t :depth treeless :ref
                      "0ffd9d6903714c1f6d8fcbb6a20941fb33dd7ae5"))
 (org-journal :source "elpaca-menu-lock-file" :recipe
              (:package "org-journal" :fetcher github :repo "bastibe/org-journal" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id org-journal :type git :protocol https
                        :inherit t :depth treeless :ref "6460f6f2b0835b4b8aa87d5fdf40cac7deb319f5"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id org-modern :type git :protocol https
                       :inherit t :depth treeless :ref "2b1e8fcd0743e3d2a69c0aeb8fc6f728904fcd91"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el")) :source "elpaca-menu-lock-file"
                         :id package-lint :type git :protocol https :inherit t :depth treeless :ref
                         "35996f478d81e51dae4fa30d051f741895d07399"))
 (parent-mode :source "elpaca-menu-lock-file" :recipe
              (:package "parent-mode" :fetcher github :repo "Fanael/parent-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id parent-mode :type git :protocol https
                        :inherit t :depth treeless :ref "fbd49857ab2e4cd0c5611c0cc83f93711657b298"))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id pass :type git :protocol https :inherit t
                 :depth treeless :ref "143456809fd2dbece9f241f4361085e1de0b0e75"))
 (password-store :source "elpaca-menu-lock-file" :recipe
                 (:package "password-store" :fetcher github :repo "zx2c4/password-store" :files
                           ("contrib/emacs/*.el") :source "elpaca-menu-lock-file" :id password-store
                           :type git :protocol https :inherit t :depth treeless :ref
                           "3ca13cd8882cae4083c1c478858adbf2e82dd037"))
 (password-store-otp :source "elpaca-menu-lock-file" :recipe
                     (:package "password-store-otp" :repo "volrath/password-store-otp.el" :fetcher
                               github :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id password-store-otp :type git
                               :protocol https :inherit t :depth treeless :ref
                               "be3a00a981921ed1b2f78012944dc25eb5a0beca"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile") ("build" "server")) :source
                      "elpaca-menu-lock-file" :id pdf-tools :type git :protocol https :inherit t
                      :depth treeless :ref "365f88238f46f9b1425685562105881800f10386"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id pet :type git :protocol https :inherit t :depth
                treeless :ref "222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158"))
 (pinentry :source "elpaca-menu-lock-file" :recipe
           (:package "pinentry" :repo ("https://github.com/ueno/pinentry-el.git" . "pinentry")
                     :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :id pinentry
                     :type git :protocol https :inherit t :depth treeless :ref
                     "0079964a1dde954ccb2ce8a28613d8020c549a36"))
 (python-pytest :source "elpaca-menu-lock-file" :recipe
                (:package "python-pytest" :fetcher github :repo "wbolster/emacs-python-pytest"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id python-pytest :type git :protocol
                          https :inherit t :depth treeless :ref
                          "8c0e048a771f355903a8c2ce78fa0974a077214e"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id rainbow-delimiters :type git
                               :protocol https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo "emacsorphanage/restclient" :files
                       ("restclient.el") :source "elpaca-menu-lock-file" :id restclient :type git
                       :protocol https :inherit t :depth treeless :ref
                       "d280632df39a175dac06037038105e32945624be"))
 (ros-face :source "elpaca-menu-lock-file" :recipe
           (:source "elpaca-menu-lock-file" :package "ros-face" :id ros-face :host github :repo
                    "smallwat3r/emacs-ros-face" :type git :protocol https :inherit t :depth treeless
                    :ref "7f21d5b9f50a0ae7847b9e3b3a574b4359bab28e"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git :protocol https :inherit t :depth
              treeless :ref "7393fa6fa305403e628058c0ec78c35d610fab05"))
 (scad-mode :source "elpaca-menu-lock-file" :recipe
            (:package "scad-mode" :repo "openscad/emacs-scad-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id scad-mode :type git :protocol https
                      :inherit t :depth treeless :ref "b5b795cf54b416bcd3245e88835662e4ec7c3dc3"))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id sudo-edit :type git :protocol https
                      :inherit t :depth treeless :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (symbol-overlay :source "elpaca-menu-lock-file" :recipe
                 (:package "symbol-overlay" :fetcher github :repo "wolray/symbol-overlay" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :id symbol-overlay :type git :protocol
                           https :inherit t :depth treeless :ref
                           "253b957f5082603708b469d02ae8c31c58292823"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id tablist :type git :protocol https :inherit t
                    :depth treeless :ref "01f065e387ffe6b7a41f180f257cd12551c7a9c2"))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode" :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :id terraform-mode :type git :protocol
                           https :inherit t :depth treeless :ref
                           "01635df3625c0cec2bb4613a6f920b8569d41009"))
 (toc-org :source "elpaca-menu-lock-file" :recipe
          (:package "toc-org" :fetcher github :repo "snosov1/toc-org" :old-names (org-toc) :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id toc-org :type git :protocol https :inherit t
                    :depth treeless :ref "69165018974a82b397d0869f0c7f0bf7d3b4ee07"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id transient :wait t :type git :protocol
                      https :inherit t :depth treeless :ref
                      "9d103f338fb9fd4506a0f1651c483209ab838f49"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files (:defaults "extensions/vertico-*.el")
                    :fetcher github :source "elpaca-menu-lock-file" :id vertico :type git :protocol
                    https :inherit t :depth treeless :ref "97a781560ff7cb77ed6e7cf09c24e0e1f2e2d95e"))
 (warm-mode :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :package "warm-mode" :id warm-mode :host github :repo
                     "smallwat3r/emacs-warm-mode" :type git :protocol https :inherit t :depth
                     treeless :ref "45a74fe5012f44297a1c563d0e94435d53935ff5"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id web-mode :type git :protocol https :inherit
                     t :depth treeless :ref "aeee2d4c82a791ff69657c1413873bf9265544df"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "elpaca-menu-lock-file" :id wgrep :type git :protocol https :inherit t
                  :depth treeless :ref "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id with-editor :type git :protocol https
                        :inherit t :depth treeless :ref "36c34610b6b700b4d1f39ccabd2b8b8c9642292d"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git" :branch
                      "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id ws-butler :type git :protocol https
                      :inherit t :depth treeless :ref "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "elpaca-menu-lock-file" :id yasnippet
                      :type git :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo "AndreaCrotti/yasnippet-snippets" :fetcher
                               github :files ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :id yasnippet-snippets :type git :protocol
                               https :inherit t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86")))
