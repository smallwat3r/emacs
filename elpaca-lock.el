((aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "elpaca-menu-lock-file" :protocol https
                :inherit t :depth treeless :ref "0e94a06bb035953cbbb4242568b38ca15443ad4c" :id aio
                :type git))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d" :id annalist :type git))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters")) :source "elpaca-menu-lock-file"
                     :protocol https :inherit t :depth treeless :ref
                     "14316996958ec8962d9395bc4d986411f77971de" :id apheleia :type git))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo "rmuslimov/browse-at-remote" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                             treeless :ref "38e5ffd77493c17c821fd88f938dbf42705a5158" :id
                             browse-at-remote :type git))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                 "7a6a752bc694e81853d915281a73a9c3acc69757" :id cape :type git))
 (cargo :source "elpaca-menu-lock-file" :recipe
        (:package "cargo" :repo "kwrooijen/cargo.el" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                  "7f8466063381eed05d4e222ce822b1dd44e3bf17" :id cargo :type git))
 (claude-code :source "elpaca-menu-lock-file" :recipe
              (:package "claude-code" :fetcher github :repo "stevemolitor/claude-code.el" :files
                        (:defaults (:exclude "install-deps.el")) :source "elpaca-menu-lock-file"
                        :protocol https :inherit t :depth treeless :host github :wait t :ref
                        "4a9914bd4161eb43f489820f9174c62390e5adc8" :id claude-code :type git))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
             "8bf87d45e169ebc091103b2aae325aece3aa804d" :id cond-let :type git))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "20476c690ce3ecd45460011ce6b03fd58a642181" :id consult :type git))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
                (:package "consult-eglot" :fetcher github :repo "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                          :ref "d8b444aac39edfc6473ffbd228df3e9119451b51" :id consult-eglot :type
                          git))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files (:defaults "extensions/corfu-*.el") :fetcher
                  github :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                  :ref "20009d4fcc31770200b63a1440f15320ee009def" :id corfu :type git))
 (creamy-theme :source "elpaca-menu-lock-file" :recipe
               (:package "creamy-theme" :fetcher github :repo "smallwat3r/emacs-creamy-theme" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                         :wait t :ref "d16902a91c28d616d01bb7bcfd7c6248415f5860" :id creamy-theme
                         :type git))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                 "d3a84021dbe48dba63b52ef7665651e0cf02e915" :id dash :type git))
 (deft :source "elpaca-menu-lock-file" :recipe
       (:package "deft" :repo "jrblevin/deft" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                 "b369d7225d86551882568788a23c5497b232509c" :id deft :type git))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "b965e19e6e7f9933199e421849a49229207c1c9f" :id diff-hl :type git))
 (dired-hacks-utils :source "elpaca-menu-lock-file" :recipe
                    (:package "dired-hacks-utils" :fetcher github :repo "Fuco1/dired-hacks" :files
                              ("dired-hacks-utils.el") :source "elpaca-menu-lock-file" :protocol
                              https :inherit t :depth treeless :ref
                              "de9336f4b47ef901799fe95315fa080fa6d77b48" :id dired-hacks-utils :type
                              git))
 (dired-narrow :source "elpaca-menu-lock-file" :recipe
               (:package "dired-narrow" :fetcher github :repo "Fuco1/dired-hacks" :files
                         ("dired-narrow.el") :source "elpaca-menu-lock-file" :protocol https
                         :inherit t :depth treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"
                         :id dired-narrow :type git))
 (dired-subtree :source "elpaca-menu-lock-file" :recipe
                (:package "dired-subtree" :fetcher github :repo "Fuco1/dired-hacks" :files
                          ("dired-subtree.el") :source "elpaca-menu-lock-file" :protocol https
                          :inherit t :depth treeless :ref "de9336f4b47ef901799fe95315fa080fa6d77b48"
                          :id dired-subtree :type git))
 (diredfl :source "elpaca-menu-lock-file" :recipe
          (:package "diredfl" :fetcher github :repo "purcell/diredfl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "fe72d2e42ee18bf6228bba9d7086de4098f18a70" :id diredfl :type git))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                   "916686b86e83a3bd2281fbc5e6f98962aa747429" :id docker :type git))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                            treeless :ref "97733ce074b1252c1270fd5e8a53d178b66668ed" :id
                            dockerfile-mode :type git))
 (eat :source "elpaca-menu-lock-file" :recipe
      (:package "eat" :repo ("https://codeberg.org/akib/emacs-eat" . "eat") :files
                ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :protocol https :inherit t
                :depth treeless :ref "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99" :id eat :type git))
 (editorconfig :source "elpaca-menu-lock-file" :recipe
               (:package "editorconfig" :fetcher github :repo "editorconfig/editorconfig-emacs"
                         :old-names (editorconfig-core editorconfig-fnmatch) :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                         :ref "b18fcf7fdea1ce84b7fdc60360ad8016b5c00d79" :id editorconfig :type git))
 (eglot-booster :source "elpaca-menu-lock-file" :recipe
                (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host
                         github :repo "jdtsmith/eglot-booster" :package "eglot-booster" :ref
                         "cab7803c4f0adc7fff9da6680f90110674bb7a22" :id eglot-booster :type git))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source "elpaca-menu-lock-file"
                       :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d" :id elisp-refs :type git))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo "https://github.com/progfolio/elpaca.git" :ref
            "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4" :depth 1 :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build (:not elpaca-activate) :type
            git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca--compile-info)
                               :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                               treeless :ref "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4" :id
                               elpaca-use-package :type git))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github :files
                   ("embark.el" "embark-org.el" "embark.texi") :source "elpaca-menu-lock-file"
                   :protocol https :inherit t :depth treeless :ref
                   "27de48004242e98586b9c9661fdb6912f26fe70f" :id embark :type git))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark" :fetcher github :files
                           ("embark-consult.el") :source "elpaca-menu-lock-file" :protocol https
                           :inherit t :depth treeless :ref
                           "27de48004242e98586b9c9661fdb6912f26fe70f" :id embark-consult :type git))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el")) :source
                 "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :wait t :ref
                 "729d9a58b387704011a115c9200614e32da3cefc" :id evil :type git))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection"
                            :files (:defaults "modes") :source "elpaca-menu-lock-file" :protocol
                            https :inherit t :depth treeless :wait t :ref
                            "12a4f9797a92df48fd4e009d91915372af7437e2" :id evil-collection :type git))
 (evil-goggles :source "elpaca-menu-lock-file" :recipe
               (:package "evil-goggles" :repo "edkolev/evil-goggles" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                         :ref "34ca276a85f615d2b45e714c9f8b5875bcb676f3" :id evil-goggles :type git))
 (evil-lion :source "elpaca-menu-lock-file" :recipe
            (:package "evil-lion" :fetcher github :repo "edkolev/evil-lion" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :ref "5a0bca151466960e090d1803c4c5ded88875f90a" :id evil-lion :type git))
 (evil-nerd-commenter :source "elpaca-menu-lock-file" :recipe
                      (:package "evil-nerd-commenter" :fetcher github :repo
                                "redguardtoo/evil-nerd-commenter" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                                treeless :ref "ae52c5070a48793e2c24474c9c8dbf20175d18a0" :id
                                evil-nerd-commenter :type git))
 (evil-snipe :source "elpaca-menu-lock-file" :recipe
             (:package "evil-snipe" :repo "hlissner/evil-snipe" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :ref "16317d7e54313490a0fe8642ed9a1a72498e7ad2" :id evil-snipe :type git))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                          :ref "da05c60b0621cf33161bb4335153f75ff5c29d91" :id evil-surround :type
                          git))
 (evil-textobj-tree-sitter :source "elpaca-menu-lock-file" :recipe
                           (:package "evil-textobj-tree-sitter" :fetcher github :repo
                                     "meain/evil-textobj-tree-sitter" :files
                                     (:defaults "queries" "treesit-queries") :old-names
                                     (evil-textobj-treesitter) :source "elpaca-menu-lock-file"
                                     :protocol https :inherit t :depth treeless :ref
                                     "7f58008a82c70eb1c6c5761db499f0be0db9d6cb" :id
                                     evil-textobj-tree-sitter :type git))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                  "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                                 treeless :ref "7552abf032a383ff761e7d90e6b5cbb4658a728a" :id
                                 exec-path-from-shell :type git))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52" :id f :type git))
 (gcmh :source "elpaca-menu-lock-file" :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                 "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9" :id gcmh :type git))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode gitignore-mode) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :ref "c3faeeea1982786f78d8c38397dec0f078eaec84" :id git-modes :type git))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                             "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                             "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                            treeless :ref "d1346a76122595aeeb7ebb292765841c6cfd417b" :id
                            git-timemachine :type git))
 (gotest :source "elpaca-menu-lock-file" :recipe
         (:package "gotest" :fetcher github :repo "nlamirault/gotest.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                   "490189e68d743a851bfb42d0017428a7550e8615" :id gotest :type git))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7" :id goto-chg :type git))
 (hcl-mode :source "elpaca-menu-lock-file" :recipe
           (:package "hcl-mode" :repo "hcl-emacs/hcl-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                     "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed" :id hcl-mode :type git))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39" :id helpful :type git))
 (highlight-numbers :source "elpaca-menu-lock-file" :recipe
                    (:package "highlight-numbers" :fetcher github :repo "Fanael/highlight-numbers"
                              :old-names (number-font-lock-mode) :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                              treeless :ref "8b4744c7f46c72b1d3d599d4fb75ef8183dee307" :id
                              highlight-numbers :type git))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d" :id hl-todo :type git))
 (imenu-list :source "elpaca-menu-lock-file" :recipe
             (:package "imenu-list" :repo "bmag/imenu-list" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :ref "76f2335ee6f2f066d87fe4e4729219d70c9bc70d" :id imenu-list :type git))
 (inheritenv :source "elpaca-menu-lock-file" :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :host github :wait t :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f" :id
                       inheritenv :type git))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files ("llama.el" ".dir-locals.el")
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                  "d430d48e0b5afd2a34b5531f103dcb110c3539c4" :id llama :type git))
 (lua-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults (:exclude "init-tryout.el")) :source "elpaca-menu-lock-file"
                     :protocol https :inherit t :depth treeless :ref
                     "2f6b8d7a6317e42c953c5119b0119ddb337e0a5f" :id lua-mode :type git))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   ".dir-locals.el" ("git-hooks" "git-hooks/*") (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                  "83d899b66f2aa4744498baa6866f3bdec2d238be" :id magit :type git))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                          :ref "83d899b66f2aa4744498baa6866f3bdec2d238be" :id magit-section :type
                          git))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :ref "51a79bb82355d0ce0ee677151f041a3aba8cbfca" :id marginalia :type git))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                          :ref "182640f79c3ed66f82f0419f130dffc173ee9464" :id markdown-mode :type
                          git))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source "elpaca-menu-lock-file" :protocol https :inherit t
                       :depth treeless :ref "1db0b0b9203cf293b38ac278273efcfc3581a05f" :id
                       nerd-icons :type git))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo "rainstormstudio/nerd-icons-dired" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                             treeless :ref "104acd8879528b8115589f35f1bbcbe231ad732f" :id
                             nerd-icons-dired :type git))
 (nginx-mode :source "elpaca-menu-lock-file" :recipe
             (:package "nginx-mode" :fetcher github :repo "ajc/nginx-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :ref "c4ac5de975d65c84893a130a470af32a48b0b66c" :id nginx-mode :type git))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :wait t :ref "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7" :id orderless :type
                      git))
 (org-journal :source "elpaca-menu-lock-file" :recipe
              (:package "org-journal" :fetcher github :repo "bastibe/org-journal" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                        :ref "6460f6f2b0835b4b8aa87d5fdf40cac7deb319f5" :id org-journal :type git))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                       :ref "713beb72aed4db43f8a10feed72136e931eb674a" :id org-modern :type git))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el")) :source "elpaca-menu-lock-file"
                         :protocol https :inherit t :depth treeless :ref
                         "1c37329703a507fa357302cf6fc29d4f2fe631a8" :id package-lint :type git))
 (parent-mode :source "elpaca-menu-lock-file" :recipe
              (:package "parent-mode" :fetcher github :repo "Fanael/parent-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                        :ref "fbd49857ab2e4cd0c5611c0cc83f93711657b298" :id parent-mode :type git))
 (pass :source "elpaca-menu-lock-file" :recipe
       (:package "pass" :fetcher github :repo "NicolasPetton/pass" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                 "143456809fd2dbece9f241f4361085e1de0b0e75" :id pass :type git))
 (password-store :source "elpaca-menu-lock-file" :recipe
                 (:package "password-store" :fetcher github :repo "zx2c4/password-store" :files
                           ("contrib/emacs/*.el") :source "elpaca-menu-lock-file" :protocol https
                           :inherit t :depth treeless :ref
                           "3ca13cd8882cae4083c1c478858adbf2e82dd037" :id password-store :type git))
 (password-store-otp :source "elpaca-menu-lock-file" :recipe
                     (:package "password-store-otp" :repo "volrath/password-store-otp.el" :fetcher
                               github :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                               treeless :ref "be3a00a981921ed1b2f78012944dc25eb5a0beca" :id
                               password-store-otp :type git))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile") ("build" "server")) :source
                      "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                      "365f88238f46f9b1425685562105881800f10386" :id pdf-tools :type git))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                "222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158" :id pet :type git))
 (pinentry :source "elpaca-menu-lock-file" :recipe
           (:package "pinentry" :repo ("https://github.com/ueno/pinentry-el.git" . "pinentry")
                     :files ("*" (:exclude ".git")) :source "elpaca-menu-lock-file" :protocol https
                     :inherit t :depth treeless :ref "0079964a1dde954ccb2ce8a28613d8020c549a36" :id
                     pinentry :type git))
 (python-pytest :source "elpaca-menu-lock-file" :recipe
                (:package "python-pytest" :fetcher github :repo "wbolster/emacs-python-pytest"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                          :ref "78b5ea1d19c7e365ac00649d13c733954b11f822" :id python-pytest :type
                          git))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                               treeless :ref "f40ece58df8b2f0fb6c8576b527755a552a5e763" :id
                               rainbow-delimiters :type git))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo "emacsorphanage/restclient" :files
                       ("restclient.el") :source "elpaca-menu-lock-file" :protocol https :inherit t
                       :depth treeless :ref "1800a4e367c250051617d0b8c16a7cbd7f47da69" :id
                       restclient :type git))
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
              :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d" :id s :type git))
 (sudo-edit :source "elpaca-menu-lock-file" :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :ref "74eb1e6986461baed9a9269566ff838530b4379b" :id sudo-edit :type git))
 (symbol-overlay :source "elpaca-menu-lock-file" :recipe
                 (:package "symbol-overlay" :fetcher github :repo "wolray/symbol-overlay" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                           treeless :ref "6151f4279bd94b5960149596b202cdcb45cacec2" :id
                           symbol-overlay :type git))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f" :id tablist :type git))
 (terraform-mode :source "elpaca-menu-lock-file" :recipe
                 (:package "terraform-mode" :repo "hcl-emacs/terraform-mode" :fetcher github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                            "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                            "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                      "LICENSE" "README*" "*-pkg.el"))
                           :source "elpaca-menu-lock-file" :protocol https :inherit t :depth
                           treeless :ref "01635df3625c0cec2bb4613a6f920b8569d41009" :id
                           terraform-mode :type git))
 (toc-org :source "elpaca-menu-lock-file" :recipe
          (:package "toc-org" :fetcher github :repo "snosov1/toc-org" :old-names (org-toc) :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                    "6d3ae0fc47ce79b1ea06cabe21a3c596395409cd" :id toc-org :type git))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :wait t :ref "9a9776c010a50169aa8f3aac459556c94b616758" :id transient :type
                      git))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files (:defaults "extensions/vertico-*.el")
                    :fetcher github :source "elpaca-menu-lock-file" :protocol https :inherit t
                    :depth treeless :ref "f3c2033ba63880d6265cf1e1eb9e987792042fc4" :id vertico
                    :type git))
 (warm-mode :source "elpaca-menu-lock-file" :recipe
            (:source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :host github
                     :repo "smallwat3r/emacs-warm-mode" :package "warm-mode" :ref
                     "45a74fe5012f44297a1c563d0e94435d53935ff5" :id warm-mode :type git))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                     "e93b3fb89fd6345a5ff59795bed712abd486200a" :id web-mode :type git))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f" :id wgrep :type git))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
             "64211dcb815f2533ac3d2a7e56ff36ae804d8338" :id with-editor :type git))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git" :branch
                      "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                      :ref "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a" :id ws-butler :type git))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets") :source "elpaca-menu-lock-file" :protocol https
                      :inherit t :depth treeless :ref "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37" :id
                      yasnippet :type git))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo "AndreaCrotti/yasnippet-snippets" :fetcher
                               github :files ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless
                               :ref "606ee926df6839243098de6d71332a697518cb86" :id
                               yasnippet-snippets :type git)))
