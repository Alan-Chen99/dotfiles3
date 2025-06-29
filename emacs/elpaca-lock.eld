((annalist :source "MELPA" :recipe
           (:ref "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d" :package
                 "annalist" :fetcher github :repo
                 "noctuid/annalist.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el")))
           :description
           "Record and display information such as keybindings" :date
           (26161 48704) :url "https://github.com/noctuid/annalist.el")
 (anzu :source "MELPA" :recipe
       (:ref "21cb5ab2295614372cb9f1a21429381e49a6255f" :package
             "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el")))
       :description
       "Show number of matches in mode-line while searching" :date
       (26360 53440) :url "https://github.com/emacsorphanage/anzu")
 (auctex :source "GNU ELPA" :url
         "https://elpa.gnu.org/packages/auctex.html" :description
         "Integrated environment for *TeX*" :recipe
         (:ref "d85316fdb0a95fdd11ea863c8defa2260110c945" :package
               "auctex" :repo
               ("https://git.savannah.gnu.org/git/auctex.git"
                . "auctex")
               :branch "main" :files ("*" (:exclude ".git"))))
 (better-jumper :source "MELPA" :recipe
                (:ref "b1bf7a3c8cb820d942a0305e0e6412ef369f819c"
                      :package "better-jumper" :repo
                      "gilbertw1/better-jumper" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el")))
                :description "Configurable jump list" :date
                (26373 65472) :url
                "https://github.com/gilbertw1/better-jumper")
 (boogie-friends :source "MELPA" :recipe
                 (:ref "bd12c6cf162fa4d90aa2a2b8dcd4b45049830bfd"
                       :package "boogie-friends" :fetcher github :repo
                       "boogie-org/boogie-friends" :files
                       ("emacs/*.el" "emacs/etc"))
                 :description
                 "A collection of programming modes for Boogie, Dafny, and Z3 (SMTLIB v2)"
                 :date (25459 7376) :url
                 "https://github.com/boogie-org/boogie-friends/")
 (clipetty :source "MELPA" :recipe
           (:ref "01b39044b9b65fa4ea7d3166f8b1ffab6f740362" :package
                 "clipetty" :repo "spudlyo/clipetty" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el")))
           :description
           "Send every kill from a TTY frame to the system clipboard"
           :date (24189 31296) :url
           "https://github.com/spudlyo/clipetty")
 (cmake-mode :source "MELPA" :recipe
             (:ref "eacc7287297267203fd65be1ca1daaaf62ebca37" :package
                   "cmake-mode" :fetcher git :url
                   "https://gitlab.kitware.com/cmake/cmake.git" :files
                   ("Auxiliary/*.el"))
             :description "Major-mode for editing CMake sources" :date
             (26501 61264) :url nil)
 (codeium :recipe
          (:ref "3c3e7de8810e46f4c9a934fceeb79d8b6777a711" :package
                "codeium")
          :source "Init file")
 (company :source "MELPA" :recipe
          (:ref "9a81d0cca268d35d5e18d7d4e2277a9e57887dd8" :package
                "company" :fetcher github :repo
                "company-mode/company-mode" :files
                (:defaults "icons"
                           ("images/small" "doc/images/small/*.png")))
          :description "Modular text completion framework" :date
          (26501 61264) :url "http://company-mode.github.io/")
 (company-quickhelp :source "MELPA" :recipe
                    (:ref "5bda859577582cc42d16fc0eaf5f7c8bedfd9e69"
                          :package "company-quickhelp" :fetcher github
                          :repo "company-mode/company-quickhelp"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el")))
                    :description
                    "Popup documentation for completion candidates"
                    :date (25913 58432) :url
                    "https://www.github.com/expez/company-quickhelp")
 (compat :source "GNU ELPA" :url
         "https://elpa.gnu.org/packages/compat.html" :description
         "Emacs Lisp Compatibility Library" :recipe
         (:ref "bfc0a46098be2dcaad0518ec200ff5a2be5cc9bb" :package
               "compat" :repo
               ("https://github.com/emacs-compat/compat" . "compat")
               :files ("*" (:exclude ".git"))))
 (consult :source "MELPA" :recipe
          (:ref "efad758b85b9b8af6d92e8c8d896eaba0623b8c3" :package
                "consult" :repo "minad/consult" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Consulting completing-read" :date
          (26533 37712) :url "https://github.com/minad/consult")
 (csharp-mode :source "MELPA" :recipe
              (:ref "02c61c219b2c22491eff9b7315fed661fab423d4"
                    :package "csharp-mode" :fetcher github :repo
                    "emacs-csharp/csharp-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el")))
              :description "C# mode derived mode" :date (25473 40272)
              :url "https://github.com/emacs-csharp/csharp-mode")
 (dash :source "MELPA" :recipe
       (:ref "1de9dcb83eacfb162b6d9a118a4770b1281bcd84" :package
             "dash" :fetcher github :repo "magnars/dash.el" :files
             ("dash.el" "dash.texi"))
       :description "A modern list library for Emacs" :date
       (26173 39872) :url "https://github.com/magnars/dash.el")
 (dockerfile-mode :source "MELPA" :recipe
                  (:ref "4d893bd2da15833ce056332e6c972d5d93e78f04"
                        :package "dockerfile-mode" :fetcher github
                        :repo "spotify/dockerfile-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el")))
                  :description
                  "Major mode for editing Docker's Dockerfiles" :date
                  (26341 2624) :url
                  "https://github.com/spotify/dockerfile-mode")
 (doom-themes :source "MELPA" :recipe
              (:ref "e506a8724156da3b1e62cb8136265e9705549d04"
                    :package "doom-themes" :fetcher github :repo
                    "doomemacs/themes" :files
                    (:defaults "themes/*.el" "themes/*/*.el"
                               "extensions/*.el"))
              :description
              "An opinionated pack of modern color-themes" :date
              (26499 19536) :url "https://github.com/doomemacs/themes")
 (ef-themes :source "GNU ELPA" :url
            "https://elpa.gnu.org/packages/ef-themes.html"
            :description "Colorful and legible themes" :recipe
            (:ref "03017b99c673b47957fe813b3934bcd2ae3f9557" :package
                  "ef-themes" :repo
                  ("https://github.com/protesilaos/ef-themes"
                   . "ef-themes")
                  :files
                  ("*"
                   (:exclude ".git" "COPYING" "doclicense.texi"
                             "contrast-ratios.org"))))
 (elpaca :recipe
   (:ref "0b9a81409e615c92c35bc55cc616f8d3b37a7256" :package "elpaca")
   :source "Init file")
 (embark :source "MELPA" :recipe
         (:ref "d5df0eff182b014ab49328a4dbb1d69eb7faafbd" :package
               "embark" :repo "oantolin/embark" :fetcher github :files
               ("embark.el" "embark-org.el" "embark.texi"))
         :description "Conveniently act on minibuffer completions"
         :date (26519 4816) :url "https://github.com/oantolin/embark")
 (evil :source "MELPA" :recipe
       (:ref "c222ce1ea6fbefaed08308c061371ebdf01b078f" :package
             "evil" :repo "emacs-evil/evil" :fetcher github :files
             (:defaults "doc/build/texinfo/evil.texi"
                        (:exclude "evil-test-helpers.el")))
       :description "Extensible vi layer" :date (26511 10704) :url
       "https://github.com/emacs-evil/evil")
 (evil-anzu :source "MELPA" :recipe
            (:ref "d1e98ee6976437164627542909a25c6946497899" :package
                  "evil-anzu" :fetcher github :repo
                  "emacsorphanage/evil-anzu" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el")))
            :description "Anzu for evil-mode" :date (25373 23872) :url
            "https://github.com/syohex/emacs-evil-anzu")
 (evil-collection :source "MELPA" :recipe
                  (:ref "7ab9179591bc2f9474f480f52ec8dc44cf0a05f1"
                        :package "evil-collection" :fetcher github
                        :repo "emacs-evil/evil-collection" :files
                        (:defaults "modes"))
                  :description "A set of keybindings for Evil mode"
                  :date (26515 7760) :url
                  "https://github.com/emacs-evil/evil-collection")
 (evil-surround :source "MELPA" :recipe
                (:ref "da05c60b0621cf33161bb4335153f75ff5c29d91"
                      :package "evil-surround" :repo
                      "emacs-evil/evil-surround" :fetcher github
                      :old-names (surround) :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el")))
                :description "Emulate surround.vim from Vim" :date
                (26112 63168) :url
                "https://github.com/emacs-evil/evil-surround")
 (evil-terminal-cursor-changer :source "MELPA" :recipe
                               (:ref
                                "2358f3e27d89128361cf80fcfa092fdfe5b52fd8"
                                :package
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
                                           "README*" "*-pkg.el")))
                               :description
                               "Change cursor shape and color by evil state in terminal"
                               :date (25920 31680) :url
                               "https://github.com/7696122/evil-terminal-cursor-changer")
 (evil-textobj-tree-sitter :source "MELPA" :recipe
                           (:ref
                            "bce236e5d2cc2fa4eae7d284ffd19ad18d46349a"
                            :package "evil-textobj-tree-sitter"
                            :fetcher github :repo
                            "meain/evil-textobj-tree-sitter" :files
                            (:defaults "queries" "treesit-queries")
                            :old-names (evil-textobj-treesitter))
                           :description
                           "Provides evil textobjects using tree-sitter"
                           :date (26426 51664) :url
                           "https://github.com/meain/evil-textobj-tree-sitter")
 (evil-visualstar :source "MELPA" :recipe
                  (:ref "06c053d8f7381f91c53311b1234872ca96ced752"
                        :package "evil-visualstar" :repo
                        "bling/evil-visualstar" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el")))
                  :description
                  "Starts a * or # search from the visual selection"
                  :date (22219 59216) :url
                  "https://github.com/bling/evil-visualstar")
 (f :source "MELPA" :recipe
    (:ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52" :package "f"
          :fetcher github :repo "rejeep/f.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
           "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el")))
    :description "Modern API for working with files and directories"
    :date (26366 5824) :url "http://github.com/rejeep/f.el")
 (flycheck :source "MELPA" :recipe
           (:ref "af1e8ca7257298dd6c51ed4424c09283b4fedb7a" :package
                 "flycheck" :repo "flycheck/flycheck" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el")))
           :description "On-the-fly syntax checking" :date
           (26525 43600) :url "https://www.flycheck.org")
 (format-all :source "MELPA" :recipe
             (:ref "75471c9255834fcbe4ebda296afeb5e984f8ec3f" :package
                   "format-all" :fetcher github :repo
                   "lassik/emacs-format-all-the-code" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el")))
             :description
             "Auto-format C, C++, JS, Python, Ruby and 50 other languages"
             :date (26437 21968) :url
             "https://github.com/lassik/emacs-format-all-the-code")
 (general :source "MELPA" :recipe
          (:ref "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f" :package
                "general" :fetcher github :repo "noctuid/general.el"
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Convenience wrappers for keybindings" :date
          (26134 3776) :url "https://github.com/noctuid/general.el")
 (goto-chg :source "MELPA" :recipe
           (:ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7" :package
                 "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
                 github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el")))
           :description "Go to last change" :date (26130 6720) :url
           "https://github.com/emacs-evil/goto-chg")
 (gptel :source "MELPA" :recipe
        (:ref "5fad6f18a608818c70ae7f9332e006c4cfaa1838" :package
              "gptel" :repo "karthink/gptel" :fetcher github :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el")))
        :description "Interact with ChatGPT or other LLMs" :date
        (26534 58576) :url "https://github.com/karthink/gptel")
 (hotfuzz :source "MELPA" :recipe
          (:ref "ff72f544e03dd2afb358f28014b15529104c1d89" :package
                "hotfuzz" :repo "axelf4/hotfuzz" :fetcher github
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Fuzzy completion style" :date (26436 1104)
          :url "https://github.com/axelf4/hotfuzz")
 (hotfuzz-rs :recipe
             (:ref "e3250ed4ca7da3380cf00981562f4227aa449edc" :package
                   "hotfuzz-rs")
             :source "Init file")
 (ht :source "MELPA" :recipe
     (:ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef" :package "ht"
           :fetcher github :repo "Wilfred/ht.el" :files
           ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
            "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
            "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
            "docs/*.texinfo"
            (:exclude ".dir-locals.el" "test.el" "tests.el"
                      "*-test.el" "*-tests.el" "LICENSE" "README*"
                      "*-pkg.el")))
     :description "The missing hash table library for Emacs" :date
     (25762 18368) :url "https://github.com/Wilfred/ht.el")
 (hydra :source "MELPA" :recipe
        (:ref "317e1de33086637579a7aeb60f77ed0405bf359b" :package
              "hydra" :repo "abo-abo/hydra" :fetcher github :files
              (:defaults (:exclude "lv.el")))
        :description "Make bindings that stick around" :date
        (25372 3008) :url "https://github.com/abo-abo/hydra")
 (iflipb :source "MELPA" :recipe
         (:ref "9ec1888335107bd314e8f40b3e113d525fed8083" :package
               "iflipb" :repo "jrosdahl/iflipb" :fetcher github :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el")))
         :description
         "Interactively flip between recently visited buffers" :date
         (25253 25792) :url "https://github.com/jrosdahl/iflipb")
 (image-roll :recipe
             (:ref "abd08072354508a4c3115beeadad2391e38ca2a1" :package
                   "image-roll")
             :source "Init file")
 (inheritenv :source "MELPA" :recipe
             (:ref "b9e67cc20c069539698a9ac54d0e6cc11e616c6f" :package
                   "inheritenv" :fetcher github :repo
                   "purcell/inheritenv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el")))
             :description
             "Make temp buffers inherit buffer-local environment variables"
             :date (26428 6992) :url
             "https://github.com/purcell/inheritenv")
 (language-id :source "MELPA" :recipe
              (:ref "dbfbc4903ffb042552b458fac76ee9f67a022036"
                    :package "language-id" :fetcher github :repo
                    "lassik/emacs-language-id" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el")))
              :description
              "Library to work with programming language identifiers"
              :date (26393 50752) :url
              "https://github.com/lassik/emacs-language-id")
 (llama :source "MELPA" :recipe
        (:ref "9802c215a3eea748d9d7f81a1465850388006897" :package
              "llama" :fetcher github :repo "tarsius/llama" :files
              ("llama.el" ".dir-locals.el"))
        :description "Compact syntax for short lambda" :date
        (26525 43600) :url "https://github.com/tarsius/llama")
 (lsp-mode :source "MELPA" :recipe
           (:ref "e1857fdb668917eed1a55ba37e3d76ef29326d84" :package
                 "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github
                 :files (:defaults "clients/*.el"))
           :description "LSP mode" :date (26528 19792) :url
           "https://github.com/emacs-lsp/lsp-mode")
 (lsp-pyright :source "MELPA" :recipe
              (:ref "dd54b3ae7c22d34faaced7b1a89739063c552b1f"
                    :package "lsp-pyright" :repo
                    "emacs-lsp/lsp-pyright" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el")))
              :description "Python LSP client using Pyright" :date
              (26405 41920) :url
              "https://github.com/emacs-lsp/lsp-pyright")
 (lsp-scheme :source "MELPA" :recipe
             (:ref "99251252005650d6f39cead8b2b9698c83251f01" :package
                   "lsp-scheme" :fetcher codeberg :repo
                   "rgherdt/emacs-lsp-scheme" :files
                   (:defaults "scripts"))
             :description "Scheme support for lsp-mode" :date
             (25726 44864) :url
             "https://codeberg.org/rgherdt/emacs-lsp-scheme")
 (lua-mode :source "MELPA" :recipe
           (:ref "d074e4134b1beae9ed4c9b512af741ca0d852ba3" :package
                 "lua-mode" :repo "immerrr/lua-mode" :fetcher github
                 :files (:defaults (:exclude "init-tryout.el")))
           :description "A major-mode for editing Lua scripts" :date
           (25909 61376) :url "https://immerrr.github.io/lua-mode")
 (lv :source "MELPA" :recipe
     (:ref "317e1de33086637579a7aeb60f77ed0405bf359b" :package "lv"
           :repo "abo-abo/hydra" :fetcher github :files ("lv.el"))
     :description "Other echo area" :date (24243 34752) :url
     "https://github.com/abo-abo/hydra")
 (magit :source "MELPA" :recipe
        (:ref "085baa2ac96df90bdb2a1bedd3598df19919d731" :package
              "magit" :fetcher github :repo "magit/magit" :files
              ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
               "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
               (:exclude "lisp/magit-section.el")))
        :description "A Git porcelain inside Emacs" :date
        (26534 58576) :url "https://github.com/magit/magit")
 (magit-section :source "MELPA" :recipe
                (:ref "085baa2ac96df90bdb2a1bedd3598df19919d731"
                      :package "magit-section" :fetcher github :repo
                      "magit/magit" :files
                      ("lisp/magit-section.el"
                       "docs/magit-section.texi"
                       "magit-section-pkg.el"))
                :description "Sections for read-only buffers" :date
                (26532 16848) :url "https://github.com/magit/magit")
 (marginalia :source "MELPA" :recipe
             (:ref "5282d5755ad1098c1f06113bf9180fd26450b92a" :package
                   "marginalia" :repo "minad/marginalia" :fetcher
                   github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el")))
             :description
             "Enrich existing commands with completion annotations"
             :date (26528 19792) :url
             "https://github.com/minad/marginalia")
 (markdown-mode :source "MELPA" :recipe
                (:ref "7659bc470d096e7a53285fa246cbabcb07d66a33"
                      :package "markdown-mode" :fetcher github :repo
                      "jrblevin/markdown-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el")))
                :description "Major mode for Markdown-formatted text"
                :date (26503 16592) :url
                "https://jblevins.org/projects/markdown-mode/")
 (modus-themes :source "MELPA" :recipe
               (:ref "ab56e2888b2fc77af2824afcc7b77d5dd854fd05"
                     :package "modus-themes" :fetcher github :repo
                     "protesilaos/modus-themes" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el")))
               :description
               "Elegant, highly legible and customizable themes" :date
               (26534 58576) :url
               "https://github.com/protesilaos/modus-themes")
 (nix-ts-mode :source "MELPA" :recipe
              (:ref "cea932fd683bfab84234205f5f1d161e29a5adf6"
                    :package "nix-ts-mode" :fetcher github :repo
                    "nix-community/nix-ts-mode" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el")))
              :description
              "Major mode for Nix expressions, powered by tree-sitter"
              :date (26362 8768) :url
              "https://github.com/nix-community/nix-ts-mode")
 (orderless :source "MELPA" :recipe
            (:ref "c7cb04499d94ee1c17affb29b1cfcd2a45116c97" :package
                  "orderless" :repo "oantolin/orderless" :fetcher
                  github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el")))
            :description
            "Completion style for matching regexps in any order" :date
            (26525 43600) :url "https://github.com/oantolin/orderless")
 (page-break-lines :source "MELPA" :recipe
                   (:ref "247854f6a82b5c7ab5ae493eb5390c5079cd032d"
                         :package "page-break-lines" :fetcher github
                         :repo "purcell/page-break-lines" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el")))
                   :description
                   "Display ^L page breaks as tidy horizontal lines"
                   :date (26337 5568) :url
                   "https://github.com/purcell/page-break-lines")
 (pdf-tools :source "MELPA" :recipe
            (:ref "ddc0e1e4a8b845a34a81273ee72823b39c894221" :package
                  "pdf-tools" :fetcher github :repo "vedang/pdf-tools"
                  :files
                  (:defaults "README" ("build" "Makefile")
                             ("build" "server")))
            :description "Support library for PDF documents" :date
            (26159 6976) :url "http://github.com/vedang/pdf-tools/")
 (php-mode :source "MELPA" :recipe
           (:ref "0f756a8c0782ebdc00557addc68763305c91ca51" :package
                 "php-mode" :repo "emacs-php/php-mode" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el")))
           :description "Major mode for editing PHP code" :date
           (26495 22480) :url "https://github.com/emacs-php/php-mode")
 (popup :source "MELPA" :recipe
        (:ref "265e6ba20f049c0f9f7e098282e087461fc3ca4e" :package
              "popup" :fetcher github :repo "auto-complete/popup-el"
              :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el")))
        :description "Visual Popup User Interface" :date (26484 52176)
        :url "https://github.com/auto-complete/popup-el")
 (pos-tip :source "MELPA" :recipe
          (:ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde" :package
                "pos-tip" :repo "pitkali/pos-tip" :fetcher github
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Show tooltip at point" :date (26053 45392)
          :url "https://github.com/pitkali/pos-tip")
 (rainbow-mode :recipe
               (:ref "f7db3b5919f70420a91eb199f8663468de3033f3"
                     :package "rainbow-mode")
               :source "Init file")
 (s :source "MELPA" :recipe
    (:ref "dda84d38fffdaf0c9b12837b504b402af910d01d" :package "s"
          :fetcher github :repo "magnars/s.el" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
           "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
           "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
           "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el")))
    :description "The long lost Emacs string manipulation library"
    :date (25361 32704) :url "https://github.com/magnars/s.el")
 (sclang :recipe
         (:ref "d8296a9af54d145703726328e22b9e89a08476af" :package
               "sclang")
         :source "Init file")
 (seq :source "GNU ELPA" :url "https://elpa.gnu.org/packages/seq.html"
      :description "Sequence manipulation functions" :recipe
      (:ref "27a90793a13f149121180e864fa53d68b9eac0b3" :package "seq"
            :repo
            ("https://github.com/emacsmirror/gnu_elpa.git" . "seq")
            :branch "externals/seq" :files ("*" (:exclude ".git"))))
 (spinner :source "GNU ELPA" :url
          "https://elpa.gnu.org/packages/spinner.html" :description
          "Add spinners and progress-bars to the mode-line for ongoing operations"
          :recipe
          (:ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21" :package
                "spinner" :repo
                ("https://github.com/Malabarba/spinner.el" . "spinner")
                :files ("*" (:exclude ".git"))))
 (tablist :source "MELPA" :recipe
          (:ref "fcd37147121fabdf003a70279cf86fbe08cfac6f" :package
                "tablist" :fetcher github :repo
                "emacsorphanage/tablist" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Extended tabulated-list-mode" :date
          (25904 43456) :url
          "https://github.com/emacsorphanage/tablist")
 (transient :source "MELPA" :recipe
            (:ref "e5cb1fd7e8d35e264313436f47972acae0819764" :package
                  "transient" :fetcher github :repo "magit/transient"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el")))
            :description "Transient commands" :date (26530 61520) :url
            "https://github.com/magit/transient")
 (tree-sitter :source "MELPA" :recipe
              (:ref "3cfab8a0e945db9b3df84437f27945746a43cc71"
                    :package "tree-sitter" :fetcher github :repo
                    "emacs-tree-sitter/elisp-tree-sitter" :branch
                    "release" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el")))
              :description "Incremental parsing system" :date
              (25095 16080) :url
              "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
 (tree-sitter-langs :source "MELPA" :recipe
                    (:ref "aa29077c5463ea67249a71608fa1a8eadec921c2"
                          :package "tree-sitter-langs" :repo
                          "emacs-tree-sitter/tree-sitter-langs"
                          :fetcher github :branch "release" :files
                          (:defaults "queries"))
                    :description "Grammar bundle for tree-sitter"
                    :date (26532 16848) :url
                    "https://github.com/emacs-tree-sitter/tree-sitter-langs")
 (ts-movement :recipe
              (:ref "7083bdeb135565c058c9cab15e5338875d8e398e"
                    :package "ts-movement")
              :source "Init file")
 (tsc :source "MELPA" :recipe
      (:ref "3cfab8a0e945db9b3df84437f27945746a43cc71" :package "tsc"
            :fetcher github :repo
            "emacs-tree-sitter/elisp-tree-sitter" :branch "release"
            :files
            ("core/*.el" "core/Cargo.toml" "core/Cargo.lock"
             "core/src"))
      :description "Core Tree-sitter APIs" :date (25095 16080) :url
      "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
 (undo-fu :source "MELPA" :recipe
          (:ref "399cc12f907f81a709f9014b6fad0205700d5772" :package
                "undo-fu" :fetcher codeberg :repo
                "ideasman42/emacs-undo-fu" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el")))
          :description "Undo helper with redo" :date (26450 34000)
          :url "https://codeberg.org/ideasman42/emacs-undo-fu")
 (undo-fu-session :source "MELPA" :recipe
                  (:ref "d90d42ddba8fa42ef5dc109196545caeabb42b75"
                        :package "undo-fu-session" :fetcher codeberg
                        :repo "ideasman42/emacs-undo-fu-session"
                        :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el")))
                  :description
                  "Persistent undo, available between sessions" :date
                  (26458 28112) :url
                  "https://codeberg.org/ideasman42/emacs-undo-fu-session")
 (vertico :source "MELPA" :recipe
          (:ref "5679dfd6aa05fb38773cf5551aef1b657b73ac78" :package
                "vertico" :repo "minad/vertico" :files
                (:defaults "extensions/vertico-*.el") :fetcher github)
          :description "VERTical Interactive COmpletion" :date
          (26528 19792) :url "https://github.com/minad/vertico")
 (visual-basic-mode :recipe
                    (:ref "79689e97d9dc0f90388c4111c5409d544a173631"
                          :package "visual-basic-mode")
                    :source "Init file")
 (vterm :source "MELPA" :recipe
        (:ref "57e953c79ff4da2cc70913c6f93693c9e354f107" :package
              "vterm" :fetcher github :repo "akermu/emacs-libvterm"
              :files
              ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h"
               "etc" "utf8.c" "utf8.h" "vterm.el" "vterm-module.c"
               "vterm-module.h"))
        :description "Fully-featured terminal emulator" :date
        (26466 22224) :url "https://github.com/akermu/emacs-libvterm")
 (vundo :source "GNU ELPA" :url
        "https://elpa.gnu.org/packages/vundo.html" :description
        "Visual undo tree" :recipe
        (:ref "288892ee93214ddcaf65fdd9f3134c1bd10100ee" :package
              "vundo" :repo
              ("https://github.com/casouri/vundo" . "vundo") :files
              ("*" (:exclude ".git" "test"))))
 (with-editor :source "MELPA"
   :recipe
   (:ref "ca902ae02972bdd6919a902be2593d8cb6bd991b" :package
         "with-editor" :fetcher github :repo "magit/with-editor"
         :files
         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
          "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
          "docs/*.texinfo"
          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                    "*-tests.el" "LICENSE" "README*" "*-pkg.el")))
   :description "Use the Emacsclient as $EDITOR" :date (26443 60752)
   :url "https://github.com/magit/with-editor")
 (xr :source "GNU ELPA" :url "https://elpa.gnu.org/packages/xr.html"
     :description "Convert string regexp to rx notation" :recipe
     (:ref "31cd397f54591e3692bf88d6e1995236928e3561" :package "xr"
           :repo ("https://github.com/mattiase/xr" . "xr") :files
           ("*" (:exclude ".git" "Makefile" ".github" "*-test.el*"))))
 (yasnippet :source "MELPA" :recipe
            (:ref "5b315f1753480ebe669b157b8242448b5eafcfea" :package
                  "yasnippet" :repo "joaotavora/yasnippet" :fetcher
                  github :files ("yasnippet.el" "snippets"))
            :description "Yet another snippet extension for Emacs"
            :date (26499 19536) :url
            "http://github.com/joaotavora/yasnippet"))
