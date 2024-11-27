((elpaca :source "lockfile" :date
         (26368 34962 258455 601000)
         :recipe
         (:source nil :protocol https :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "86675183608ec290b6ff24fab4a4f4419ff7dc26" :files
                  (:defaults "elpaca-test.el"
                             (:exclude "extensions"))
                  :build
                  (:not elpaca--activate-package)
                  :package "elpaca"))
 (evil :source "lockfile" :date
       (26368 34962 257446 1000)
       :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "ea552efeeb809898932f55d1690da9cbe8ef5fa1"))
 (general :source "lockfile" :date
          (26368 34962 256499 1000)
          :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f"))
 (dash :source "lockfile" :date
       (26368 34962 255489 1000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi")
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))
 (evil-visualstar :source "lockfile" :date
                  (26368 34962 254516 600000)
                  :recipe
                  (:package "evil-visualstar" :repo "bling/evil-visualstar" :fetcher github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1 :ref "06c053d8f7381f91c53311b1234872ca96ced752"))
 (undo-fu :source "lockfile" :date
          (26368 34962 253565 600000)
          :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "dbb3e4b699dd488497ef9b32a04b8e928a6bc8ef"))
 (undo-fu-session :source "lockfile" :date
                  (26368 34962 252534 400000)
                  :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1 :ref "beb0e285d074145eaf481a959c903b77c19ae91e"))
 (format-all :source "lockfile" :date
             (26368 34962 251216 400000)
             :recipe
             (:package "format-all" :fetcher github :repo "lassik/emacs-format-all-the-code" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "9ae47456dad2925e4d41f58bd2c864b87f82aa8b"))
 (hydra :source "lockfile" :date
        (26368 34962 249371 900000)
        :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults
                   (:exclude "lv.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (seq :source "lockfile" :date
      (26368 34962 248435 100000)
      :recipe
      (:package "seq" :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "seq" :branch "externals/seq" :files
                ("*"
                 (:exclude ".git"))
                :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :build
                (:not elpaca--activate-package)
                :ref "27a90793a13f149121180e864fa53d68b9eac0b3"))
 (goto-chg :source "lockfile" :date
           (26368 34962 247165 100000)
           :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (inheritenv :source "lockfile" :date
             (26368 34962 246113 999000)
             :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "e8b012933cf5cc8deee8d961ca6c6c0d92745218"))
 (language-id :source "lockfile" :date
              (26368 34962 245080 799000)
              :recipe
              (:package "language-id" :fetcher github :repo "lassik/emacs-language-id" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "44452e4f7962aca41cc2539fce1d27799d6e656c"))
 (lv :source "lockfile" :date
     (26368 34962 244038 599000)
     :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el")
               :source "MELPA" :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (ef-themes :source "lockfile" :date
            (26368 34962 242762 899000)
            :recipe
            (:package "ef-themes" :repo "https://github.com/protesilaos/ef-themes" :local-repo "ef-themes" :files
                      ("*"
                       (:exclude ".git" "COPYING" "doclicense.texi" "contrast-ratios.org"))
                      :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (add-to-list 'theme-short-list 'ef-day)
                        (add-to-list 'theme-short-list 'ef-night)
                        (add-to-list 'theme-short-list 'ef-winter)
                        (add-to-list 'theme-short-list 'ef-frost)
                        (startup-switch-theme 'ef-winter))
                      :ref "dfeddbded6df72ecc5d58081c7ed123ee2ec5a89"))
 (doom-themes :source "lockfile" :date
              (26368 34962 241847 599000)
              :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el")
                        :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                        (closure
                            (ef-themes-faces-overwrites t)
                            nil
                          (add-to-list 'theme-short-list 'doom-peacock)
                          (add-to-list 'theme-short-list 'doom-dracula))
                        :ref "1cac71a4b2434036496a49b4440fdba3d0b5b387"))
 (evil-collection :source "lockfile" :date
                  (26368 34962 240951 599000)
                  :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection" :files
                            (:defaults "modes")
                            :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                            (closure
                                (t)
                                nil
                              (startup-queue-package 'evil-collection 80))
                            :ref "6365e7c8ae728f7a26294db261b6778d089a6263"))
 (evil-terminal-cursor-changer :source "lockfile" :date
                               (26368 34962 239976 699000)
                               :recipe
                               (:package "evil-terminal-cursor-changer" :fetcher github :repo "7696122/evil-terminal-cursor-changer" :files
                                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                         :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                                         (closure
                                             (t)
                                             nil
                                           (if
                                               (display-graphic-p)
                                               nil
                                             (startup-queue-package 'evil-terminal-cursor-changer 100)))
                                         :ref "2358f3e27d89128361cf80fcfa092fdfe5b52fd8"))
 (clipetty :source "lockfile" :date
           (26368 34962 239011 398000)
           :recipe
           (:package "clipetty" :repo "spudlyo/clipetty" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                     (closure
                         (t)
                         nil
                       (if
                           (display-graphic-p)
                           nil
                         (startup-queue-package 'clipetty 70)))
                     :ref "01b39044b9b65fa4ea7d3166f8b1ffab6f740362"))
 (rainbow-mode :source "lockfile" :date
               (26368 34962 238043 798000)
               :recipe
               (:source nil :inherit nil :host github :repo "emacsmirror/rainbow-mode" :alan-init-fn
                        (closure
                            (t)
                            nil
                          (startup-queue-package 'rainbow-mode -10))
                        :package "rainbow-mode" :ref "f7db3b5919f70420a91eb199f8663468de3033f3"))
 (iflipb :source "lockfile" :date
         (26368 34962 236911 198000)
         :recipe
         (:package "iflipb" :repo "jrosdahl/iflipb" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                   (closure
                       (t)
                       nil
                     (startup-queue-package 'iflipb 50))
                   :ref "9ec1888335107bd314e8f40b3e113d525fed8083"))
 (vundo :source "lockfile" :date
        (26368 34962 235842 998000)
        :recipe
        (:package "vundo" :repo "https://github.com/casouri/vundo" :local-repo "vundo" :files
                  ("*"
                   (:exclude ".git" "test"))
                  :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                  (closure
                      (t)
                      nil
                    (general-define-key :states 'motion "g t" #'vundo))
                  :ref "ca590c571546eb1d38c855216db11d28135892f2"))
 (page-break-lines :source "lockfile" :date
                   (26368 34962 234858 298000)
                   :recipe
                   (:package "page-break-lines" :fetcher github :repo "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                             (closure
                                 (t)
                                 nil
                               (startup-queue-package 'page-break-lines 50))
                             :ref "4a20d4a28a1228c561b151f868611ad75de90e5e"))
 (evil-anzu :source "lockfile" :date
            (26368 34962 233831 398000)
            :recipe
            (:package "evil-anzu" :fetcher github :repo "emacsorphanage/evil-anzu" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (startup-queue-package 'evil-anzu 75))
                      :ref "d1e98ee6976437164627542909a25c6946497899"))
 (evil-surround :source "lockfile" :date
                (26368 34962 232812 298000)
                :recipe
                (:package "evil-surround" :repo "emacs-evil/evil-surround" :fetcher github :old-names
                          (surround)
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                          (closure
                              (t)
                              nil
                            (startup-queue-package 'evil-surround 70))
                          :ref "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (better-jumper :source "lockfile" :date
                (26368 34962 231677 398000)
                :recipe
                (:package "better-jumper" :repo "gilbertw1/better-jumper" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                          (closure
                              (t)
                              nil
                            (startup-queue-package 'better-jumper 70))
                          :ref "47622213783ece37d5337dc28d33b530540fc319"))
 (vertico :source "lockfile" :date
          (26368 34962 230543 497000)
          :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/*")
                    :fetcher github :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'vertico 80))
                    :ref "e826dfcb14af5e2cfd88ed110d0208ddc2d37788"))
 (consult :source "lockfile" :date
          (26368 34962 229438 897000)
          :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'consult 50))
                    :ref "07ea5421fbcb65b2207a43a0941124a95a47abc7"))
 (orderless :source "lockfile" :date
            (26368 34962 228343 97000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (startup-queue-package 'orderless 75))
                      :ref "96b74d2450ab4ab1a175d0e86c62f6695c4709b5"))
 (hotfuzz :source "lockfile" :date
          (26368 34962 226865 897000)
          :recipe
          (:package "hotfuzz" :repo "axelf4/hotfuzz" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'hotfuzz 76))
                    :ref "17c0413af30ed249650535369d4d4f7abef70a8c"))
 (hotfuzz-rs :source "lockfile" :date
             (26368 34962 225914 997000)
             :recipe
             (:source nil :protocol ssh :inherit t :depth 1 :host github :repo "Alan-Chen99/hotfuzz-rs" :pre-build
                      (let
                          ((target-file
                            (if
                                (eq system-type 'windows-nt)
                                "hotfuzz-rs-module.dll" "hotfuzz-rs-module.so"))
                           (cargo-res
                            (if
                                (eq system-type 'windows-nt)
                                "./target/release/hotfuzz_rs_module.dll" "./target/release/libhotfuzz_rs_module.so")))
                        (elpaca-process-poll "cargo" "clean")
                        (elpaca-process-poll "cargo" "build" "--release")
                        (delete-file target-file)
                        (rename-file cargo-res target-file))
                      :files
                      (:defaults "hotfuzz-rs-module.so" "hotfuzz-rs-module.dll")
                      :alan-init-fn
                      (closure
                          (t)
                          nil
                        (startup-queue-package 'hotfuzz-rs 76))
                      :package "hotfuzz-rs" :ref "e3250ed4ca7da3380cf00981562f4227aa449edc"))
 (company :source "lockfile" :date
          (26368 34962 225004 297000)
          :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small" "doc/images/small/*.png"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'company 80)
                      (startup-queue-package 'company-capf 80)
                      (startup-queue-package 'company-files 80))
                    :ref "9c273fc7c1a9dd69ccf508589211c4f8bd0e0765"))
 (company-quickhelp :source "lockfile" :date
                    (26368 34962 224124 397000)
                    :recipe
                    (:package "company-quickhelp" :fetcher github :repo "company-mode/company-quickhelp" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                              (closure
                                  (t)
                                  nil
                                (alan-eval-after-load 'company
                                  #'(lambda nil
                                      (startup-queue-package 'company-quickhelp 80))))
                              :ref "5bda859577582cc42d16fc0eaf5f7c8bedfd9e69"))
 (marginalia :source "lockfile" :date
             (26368 34962 223169 996000)
             :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                       (closure
                           (t)
                           nil
                         (startup-queue-package 'marginalia 50))
                       :ref "be2e57efff640880251c082ac93bd365b7202e6a"))
 (flycheck :source "lockfile" :date
           (26368 34962 222283 396000)
           :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                     (closure
                         (t)
                         nil
                       (startup-queue-package 'flycheck 0))
                     :ref "7a6398ea3538a898eba0276f0f89b2f878325a89"))
 (lsp-mode :source "lockfile" :date
           (26368 34962 221417 296000)
           :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.el")
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "3a952ca135bd508e7dee4650a195b7ec5886a0ea"))
 (tsc :source "lockfile" :date
      (26368 34962 220501 296000)
      :recipe
      (:package "tsc" :fetcher github :repo "emacs-tree-sitter/elisp-tree-sitter" :branch "release" :files
                ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src" "core/tsc-dyn.so")
                :source "MELPA" :protocol https :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (tree-sitter :source "lockfile" :date
              (26368 34962 219666 196000)
              :recipe
              (:package "tree-sitter" :repo "emacs-tree-sitter/elisp-tree-sitter" :fetcher github :branch "release" :files
                        (:defaults
                         (:exclude "lisp/tree-sitter-tests.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (tree-sitter-langs :source "lockfile" :date
                    (26368 34962 218768 296000)
                    :recipe
                    (:package "tree-sitter-langs" :repo "emacs-tree-sitter/tree-sitter-langs" :fetcher github :branch "release" :files
                              (:defaults "queries")
                              :source "MELPA" :protocol https :inherit t :depth nil :remotes
                              ("alan" :repo "Alan-Chen99/tree-sitter-langs" :branch "master")
                              :alan-init-fn
                              (closure
                                  (t)
                                  nil
                                (startup-queue-package 'tree-sitter-langs 0))
                              :ref "aa29077c5463ea67249a71608fa1a8eadec921c2"))
 (evil-textobj-tree-sitter :source "lockfile" :date
                           (26368 34962 217716 696000)
                           :recipe
                           (:package "evil-textobj-tree-sitter" :fetcher github :repo "meain/evil-textobj-tree-sitter" :files
                                     (:defaults "queries" "treesit-queries")
                                     :old-names
                                     (evil-textobj-treesitter)
                                     :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                                     (closure
                                         (t)
                                         nil
                                       (startup-queue-package 'evil-textobj-tree-sitter 0))
                                     :ref "5056ebc231492827c159c878be30b55a42ae68be"))
 (ts-movement :source "lockfile" :date
              (26368 34962 216799 96000)
              :recipe
              (:source nil :protocol https :inherit t :depth 1 :host github :repo "haritkapadia/ts-movement" :alan-init-fn
                       (closure
                           (t)
                           nil
                         (startup-queue-package 'ts-movement 0))
                       :package "ts-movement" :ref "7083bdeb135565c058c9cab15e5338875d8e398e"))
 (transient :source "lockfile" :date
            (26368 34962 215912 295000)
            :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref "fc03c0b75826aa771b682137aa3f4e24130a9e3c"))
 (with-editor :source "lockfile" :date
   (26368 34962 214923 395000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth 1 :ref "77cb2403158cfea9d8bfb8adad81b84d1d6d7c6a"))
 (magit :source "lockfile" :date
        (26368 34962 213967 595000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "magit-pkg.el"
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                  (closure
                      (t)
                      nil
                    (startup-queue-package 'magit -50))
                  :ref "2b6516e04431c339a41887b27ce6a1193df6e9c3"))
 (gptel :source "lockfile" :date
        (26368 34962 212889 395000)
        :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :host github :ref "88f066d829f11f53fdcb39d26ac34b0492f02687"))
 (vterm :source "lockfile" :date
        (26368 34962 211798 695000)
        :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
                  (:defaults "vterm-module.so" "etc")
                  :source "MELPA" :protocol https :inherit t :depth 1 :pre-build
                  (progn
                    (setq vterm-always-compile-module t)
                    (require 'vterm)
                    (vterm-module-compile)
                    (require 'vterm-module))
                  :ref "ae4ae1aef2fcae98a37aad83f2a6aeeaeacedd4f"))
 (codeium :source "lockfile" :date
          (26368 34962 210733 995000)
          :recipe
          (:source nil :protocol https :inherit t :depth 1 :host github :repo "Exafunction/codeium.el" :package "codeium" :ref "08d5ecfa74d960cf18af46c2d7fa0449d789d73b"))
 (rust-mode :source "lockfile" :date
            (26368 34962 209758 695000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref "3bd0863f28414e629ca15ad2852a429a126226c5"))
 (lsp-pyright :source "lockfile" :date
              (26368 34962 208802 295000)
              :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "bf27f1959460661648ecaaee4b5701bc003e3003"))
 (image-roll :source "lockfile" :date
             (26368 34962 207776 994000)
             :recipe
             (:source nil :protocol https :inherit t :depth 1 :host github :repo "dalanicolai/image-roll.el" :package "image-roll" :ref "abd08072354508a4c3115beeadad2391e38ca2a1"))
 (pdf-tools :source "lockfile" :date
            (26368 34962 206903 794000)
            :recipe
            (:package "pdf-tools" :fetcher github :repo "dalanicolai/pdf-tools" :files
                      ("lisp/*.el" "README"
                       ("build" "Makefile")
                       ("build" "server")
                       (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :host github :branch "pdf-roll" :alan-extra-deps
                      ((image-roll "0"))
                      :alan-init-fn
                      (closure
                          (t)
                          nil
                        (add-to-list 'auto-mode-alist
                                     (cons "\\.pdf\\'" #'pdf-view-mode)))
                      :ref "ddc0e1e4a8b845a34a81273ee72823b39c894221"))
 (nix-ts-mode :source "lockfile" :date
              (26368 34962 205987 394000)
              :recipe
              (:package "nix-ts-mode" :fetcher github :repo "nix-community/nix-ts-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                        (closure
                            (t)
                            nil
                          (add-to-list 'auto-mode-alist
                                       '("\\.nix\\'" . nix-ts-mode)))
                        :ref "a1189df50d8f4fa88f3b939cb9727bb5997f9b82"))
 (markdown-mode :source "lockfile" :date
                (26368 34962 205109 894000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1 :ref "6102ac5b7301b4c4fc0262d9c6516693d5a33f2b"))
 (lsp-scheme :source "lockfile" :date
             (26368 34962 204193 894000)
             :recipe
             (:package "lsp-scheme" :fetcher codeberg :repo "rgherdt/emacs-lsp-scheme" :files
                       (:defaults "scripts")
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "99251252005650d6f39cead8b2b9698c83251f01"))
 (auctex :source "lockfile" :date
         (26368 34962 203278 994000)
         :recipe
         (:package "auctex" :repo "https://git.savannah.gnu.org/git/auctex.git" :local-repo "auctex" :files
                   ("*"
                    (:exclude ".git"))
                   :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref "58d72b1799d25fa5b6aaa3b7404f4fc49718ae31"))
 (dockerfile-mode :source "lockfile" :date
                  (26368 34962 202252 294000)
                  :recipe
                  (:package "dockerfile-mode" :fetcher github :repo "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1 :ref "4d893bd2da15833ce056332e6c972d5d93e78f04"))
 (lua-mode :source "lockfile" :date
           (26368 34962 201304 894000)
           :recipe
           (:package "lua-mode" :repo "immerrr/lua-mode" :fetcher github :files
                     (:defaults
                      (:exclude "init-tryout.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "d074e4134b1beae9ed4c9b512af741ca0d852ba3"))
 (php-mode :source "lockfile" :date
           (26368 34962 200336 793000)
           :recipe
           (:package "php-mode" :repo "emacs-php/php-mode" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "e3087db57e6b1682eb80af2916f106ac19472ce3"))
 (visual-basic-mode :source "lockfile" :date
                    (26368 34962 199451 93000)
                    :recipe
                    (:source nil :protocol https :inherit t :depth 1 :host github :repo "emacsmirror/visual-basic-mode" :alan-init-fn
                             (closure
                                 (t)
                                 nil
                               (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
                               (setq auto-mode-alist
                                     (cons
                                      '("\\.\\(?:frm\\|\\(?:ba\\|vb\\)s\\)\\'" . visual-basic-mode)
                                      auto-mode-alist))
                               (progn
                                 (set-default 'visual-basic-mode-indent 4))
                               (general-define-key :keymaps 'visual-basic-mode-map
                                                   [remap evil-indent]
                                                   #'evil-indent))
                             :package "visual-basic-mode" :ref "79689e97d9dc0f90388c4111c5409d544a173631"))
 (annalist :source "lockfile" :date
           (26368 34962 198535 893000)
           :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (anzu :source "lockfile" :date
       (26368 34962 197299 193000)
       :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "21cb5ab2295614372cb9f1a21429381e49a6255f"))
 (compat :source "lockfile" :date
         (26368 34962 196161 493000)
         :recipe
         (:package "compat" :repo "https://github.com/emacs-compat/compat" :local-repo "compat" :files
                   ("*"
                    (:exclude ".git"))
                   :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref "9a234d0d28cccd33f64faea6074fa2865a17c164"))
 (pos-tip :source "lockfile" :date
          (26368 34962 195302 993000)
          :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (f :source "lockfile" :date
    (26368 34962 194427 693000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (ht :source "lockfile" :date
     (26368 34962 193514 193000)
     :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (spinner :source "lockfile" :date
          (26368 34962 192620 692000)
          :recipe
          (:package "spinner" :repo "https://github.com/Malabarba/spinner.el" :local-repo "spinner" :files
                    ("*"
                     (:exclude ".git"))
                    :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (s :source "lockfile" :date
    (26368 34962 191715 692000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (magit-section :source "lockfile" :date
                (26368 34962 190786 492000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source "MELPA" :protocol https :inherit t :depth 1 :ref "2b6516e04431c339a41887b27ce6a1193df6e9c3"))
 (tablist :source "lockfile" :date
          (26368 34962 189879 492000)
          :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "fcd37147121fabdf003a70279cf86fbe08cfac6f")))
