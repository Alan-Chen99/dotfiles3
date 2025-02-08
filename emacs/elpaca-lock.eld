((annalist :recipe
           (:package "annalist" :fetcher github :repo
                     "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :ref "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (anzu :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu"
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :ref "21cb5ab2295614372cb9f1a21429381e49a6255f"))
 (auctex :recipe
         (:package "auctex" :repo
                   ("https://git.savannah.gnu.org/git/auctex.git"
                    . "auctex")
                   :branch "main" :files ("*" (:exclude ".git")) :ref
                   "efc1355ca3cea1b48c08bda2759bfb6030146d73"))
 (better-jumper :recipe
                (:package "better-jumper" :repo
                          "gilbertw1/better-jumper" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :ref
                          "b1bf7a3c8cb820d942a0305e0e6412ef369f819c"))
 (boogie-friends :recipe
                 (:package "boogie-friends" :fetcher github :repo
                           "boogie-org/boogie-friends" :files
                           ("emacs/*.el" "emacs/etc") :ref
                           "e4ffb45641fcefd0a0c0a89071060b842a656369"))
 (clipetty :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :ref "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (cmake-mode :recipe
             (:package "cmake-mode" :fetcher git :url
                       "https://gitlab.kitware.com/cmake/cmake.git"
                       :files ("Auxiliary/*.el") :ref
                       "eacc7287297267203fd65be1ca1daaaf62ebca37"))
 (codeium :recipe (:ref "3c3e7de8810e46f4c9a934fceeb79d8b6777a711"))
 (company :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :ref "9a81d0cca268d35d5e18d7d4e2277a9e57887dd8"))
 (company-quickhelp :recipe
                    (:package "company-quickhelp" :fetcher github
                              :repo "company-mode/company-quickhelp"
                              :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :ref
                              "5bda859577582cc42d16fc0eaf5f7c8bedfd9e69"))
 (consult :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "5b0c682d7092e02a0e8e0f047370be8c61ad3b4e"))
 (csharp-mode :recipe
              (:package "csharp-mode" :fetcher github :repo
                        "emacs-csharp/csharp-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :ref
                        "02c61c219b2c22491eff9b7315fed661fab423d4"))
 (dash :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :ref
                 "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))
 (dockerfile-mode :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :ref
                            "4d893bd2da15833ce056332e6c972d5d93e78f04"))
 (doom-themes :recipe
              (:package "doom-themes" :fetcher github :repo
                        "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el"
                                   "extensions/*.el")
                        :ref
                        "e506a8724156da3b1e62cb8136265e9705549d04"))
 (ef-themes :recipe
            (:package "ef-themes" :repo
                      ("https://github.com/protesilaos/ef-themes"
                       . "ef-themes")
                      :files
                      ("*"
                       (:exclude ".git" "COPYING" "doclicense.texi"
                                 "contrast-ratios.org"))
                      :ref "03017b99c673b47957fe813b3934bcd2ae3f9557"))
 (elpaca :recipe (:ref "1b7343b335eee4968f4c2ea189fc6876c8d07d55"))
 (embark :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :ref "d5df0eff182b014ab49328a4dbb1d69eb7faafbd"))
 (evil :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :ref "c222ce1ea6fbefaed08308c061371ebdf01b078f"))
 (evil-anzu :recipe
            (:package "evil-anzu" :fetcher github :repo
                      "emacsorphanage/evil-anzu" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :ref "d1e98ee6976437164627542909a25c6946497899"))
 (evil-collection :recipe
                  (:package "evil-collection" :fetcher github :repo
                            "emacs-evil/evil-collection" :files
                            (:defaults "modes") :ref
                            "7ab9179591bc2f9474f480f52ec8dc44cf0a05f1"))
 (evil-surround :recipe
                (:package "evil-surround" :repo
                          "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (evil-terminal-cursor-changer :recipe
                               (:package
                                "evil-terminal-cursor-changer"
                                :fetcher github :repo
                                "7696122/evil-terminal-cursor-changer"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :ref
                                "2358f3e27d89128361cf80fcfa092fdfe5b52fd8"))
 (evil-textobj-tree-sitter :recipe
                           (:package "evil-textobj-tree-sitter"
                                     :fetcher github :repo
                                     "meain/evil-textobj-tree-sitter"
                                     :files
                                     (:defaults "queries"
                                                "treesit-queries")
                                     :old-names
                                     (evil-textobj-treesitter) :ref
                                     "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"))
 (evil-visualstar :recipe
                  (:package "evil-visualstar" :repo
                            "bling/evil-visualstar" :fetcher github
                            :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :ref
                            "06c053d8f7381f91c53311b1234872ca96ced752"))
 (f :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (flycheck :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :ref "af1e8ca7257298dd6c51ed4424c09283b4fedb7a"))
 (format-all :recipe
             (:package "format-all" :fetcher github :repo
                       "lassik/emacs-format-all-the-code" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :ref "75471c9255834fcbe4ebda296afeb5e984f8ec3f"))
 (general :recipe
          (:package "general" :fetcher github :repo
                    "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f"))
 (goto-chg :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :ref "6090687150e4781e93c1d736e2b8d1e6be60054f"))
 (hotfuzz :recipe
          (:package "hotfuzz" :repo "axelf4/hotfuzz" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "ff72f544e03dd2afb358f28014b15529104c1d89"))
 (hotfuzz-rs :recipe (:ref "e3250ed4ca7da3380cf00981562f4227aa449edc"))
 (ht :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :ref
                  "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (iflipb :recipe
         (:package "iflipb" :repo "jrosdahl/iflipb" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :ref "9ec1888335107bd314e8f40b3e113d525fed8083"))
 (image-roll :recipe (:ref "abd08072354508a4c3115beeadad2391e38ca2a1"))
 (inheritenv :recipe
             (:package "inheritenv" :fetcher github :repo
                       "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f"))
 (language-id :recipe
              (:package "language-id" :fetcher github :repo
                        "lassik/emacs-language-id" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :ref
                        "dbfbc4903ffb042552b458fac76ee9f67a022036"))
 (llama :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :ref "9802c215a3eea748d9d7f81a1465850388006897"))
 (lsp-mode :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.el") :ref
                     "e1857fdb668917eed1a55ba37e3d76ef29326d84"))
 (lsp-pyright :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :ref
                        "dd54b3ae7c22d34faaced7b1a89739063c552b1f"))
 (lsp-scheme :recipe
             (:package "lsp-scheme" :fetcher codeberg :repo
                       "rgherdt/emacs-lsp-scheme" :files
                       (:defaults "scripts") :ref
                       "99251252005650d6f39cead8b2b9698c83251f01"))
 (lua-mode :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher
                     github :files
                     (:defaults (:exclude "init-tryout.el")) :ref
                     "d074e4134b1beae9ed4c9b512af741ca0d852ba3"))
 (lv :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :ref
               "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (magit :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" "magit-pkg.el"
                   (:exclude "lisp/magit-section.el"))
                  :ref "c8e8ae9abfa0de8c6b52b2a806cc02548ef8bbaf"))
 (magit-section :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :ref
                          "c8e8ae9abfa0de8c6b52b2a806cc02548ef8bbaf"))
 (marginalia :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :ref "5282d5755ad1098c1f06113bf9180fd26450b92a"))
 (markdown-mode :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :ref
                          "7659bc470d096e7a53285fa246cbabcb07d66a33"))
 (nix-ts-mode :recipe
              (:package "nix-ts-mode" :fetcher github :repo
                        "nix-community/nix-ts-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :ref
                        "cea932fd683bfab84234205f5f1d161e29a5adf6"))
 (orderless :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :ref "c7cb04499d94ee1c17affb29b1cfcd2a45116c97"))
 (page-break-lines :recipe
                   (:package "page-break-lines" :fetcher github :repo
                             "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :ref
                             "247854f6a82b5c7ab5ae493eb5390c5079cd032d"))
 (pdf-tools :recipe
            (:package "pdf-tools" :fetcher github :repo
                      "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile")
                                 ("build" "server"))
                      :ref "ddc0e1e4a8b845a34a81273ee72823b39c894221"))
 (php-mode :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :ref "0f756a8c0782ebdc00557addc68763305c91ca51"))
 (popup :recipe
        (:package "popup" :fetcher github :repo
                  "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :ref "265e6ba20f049c0f9f7e098282e087461fc3ca4e"))
 (pos-tip :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (rainbow-mode :recipe
               (:ref "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (s :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (seq :recipe
      (:package "seq" :repo
                ("https://git.savannah.gnu.org/git/emacs/elpa.git"
                 . "seq")
                :branch "externals/seq" :files ("*" (:exclude ".git"))
                :ref "27a90793a13f149121180e864fa53d68b9eac0b3"))
 (spinner :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :files ("*" (:exclude ".git")) :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (tablist :recipe
          (:package "tablist" :fetcher github :repo
                    "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (transient :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :ref "e5cb1fd7e8d35e264313436f47972acae0819764"))
 (tree-sitter :recipe
              (:package "tree-sitter" :fetcher github :repo
                        "emacs-tree-sitter/elisp-tree-sitter" :branch
                        "release" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :ref
                        "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (tree-sitter-langs :recipe
                    (:package "tree-sitter-langs" :repo
                              "emacs-tree-sitter/tree-sitter-langs"
                              :fetcher github :branch "release" :files
                              (:defaults "queries") :ref
                              "aa29077c5463ea67249a71608fa1a8eadec921c2"))
 (ts-movement :recipe
              (:ref "7083bdeb135565c058c9cab15e5338875d8e398e"))
 (tsc :recipe
      (:package "tsc" :fetcher github :repo
                "emacs-tree-sitter/elisp-tree-sitter" :branch
                "release" :files
                ("core/*.el" "core/Cargo.toml" "core/Cargo.lock"
                 "core/src")
                :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (undo-fu :recipe
          (:package "undo-fu" :fetcher codeberg :repo
                    "ideasman42/emacs-undo-fu" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :ref "399cc12f907f81a709f9014b6fad0205700d5772"))
 (undo-fu-session :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo
                            "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :ref
                            "d90d42ddba8fa42ef5dc109196545caeabb42b75"))
 (vertico :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :ref
                    "5679dfd6aa05fb38773cf5551aef1b657b73ac78"))
 (visual-basic-mode :recipe
                    (:ref "79689e97d9dc0f90388c4111c5409d544a173631"))
 (vterm :recipe
        (:package "vterm" :fetcher github :repo
                  "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h"
                   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
                   "vterm-module.c" "vterm-module.h")
                  :ref "57e953c79ff4da2cc70913c6f93693c9e354f107"))
 (vundo :recipe
        (:package "vundo" :repo
                  ("https://github.com/casouri/vundo" . "vundo")
                  :files ("*" (:exclude ".git" "test")) :ref
                  "288892ee93214ddcaf65fdd9f3134c1bd10100ee"))
 (with-editor :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :ref "ca902ae02972bdd6919a902be2593d8cb6bd991b"))
 (yasnippet :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :ref
                      "5b315f1753480ebe669b157b8242448b5eafcfea")))
