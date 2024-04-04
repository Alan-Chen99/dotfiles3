((elpaca :source "lockfile" :date
         (26120 31575 631353 122000)
         :recipe
         (:source nil :protocol https :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "86675183608ec290b6ff24fab4a4f4419ff7dc26" :files
                  (:defaults "elpaca-test.el"
                             (:exclude "extensions"))
                  :build
                  (:not elpaca--activate-package)
                  :package "elpaca"))
 (evil :source "lockfile" :date
       (26120 31575 630504 322000)
       :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "a7ffa73bbdc523c0e473d79c0ded7c6457bcb65c"))
 (general :source "lockfile" :date
          (26120 31575 629513 522000)
          :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "ced143c30de8e20f5a3761a465e684a1dc48471e"))
 (dash :source "lockfile" :date
       (26120 31575 628669 422000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi")
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "5df7605da5a080df769d4f260034fb0e5e86a7a4"))
 (evil-visualstar :source "lockfile" :date
                  (26120 31575 627789 122000)
                  :recipe
                  (:package "evil-visualstar" :repo "bling/evil-visualstar" :fetcher github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1 :ref "06c053d8f7381f91c53311b1234872ca96ced752"))
 (undo-fu :source "lockfile" :date
          (26120 31575 626909 622000)
          :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "04961ba775142627c5fa4bb94c3e507afedaecd1"))
 (undo-fu-session :source "lockfile" :date
                  (26120 31575 626038 122000)
                  :recipe
                  (:package "undo-fu-session" :fetcher codeberg :repo "ideasman42/emacs-undo-fu-session" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :source "MELPA" :protocol https :inherit t :depth 1 :ref "2b355c9d39b2688f859a762f2289f23fd16fadc4"))
 (format-all :source "lockfile" :date
             (26120 31575 625132 322000)
             :recipe
             (:package "format-all" :fetcher github :repo "lassik/emacs-format-all-the-code" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "1f4a69811b4b6a00c74fa2566ef731b17b9a2ed1"))
 (hydra :source "lockfile" :date
        (26120 31575 624221 922000)
        :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults
                   (:exclude "lv.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (seq :source "lockfile" :date
      (26120 31575 623300 322000)
      :recipe
      (:package "seq" :repo "git://git.sv.gnu.org/emacs/elpa" :local-repo "seq" :branch "externals/seq" :files
                ("*"
                 (:exclude ".git"))
                :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :build
                (:not elpaca--activate-package)
                :ref "27a90793a13f149121180e864fa53d68b9eac0b3"))
 (goto-chg :source "lockfile" :date
           (26120 31575 622403 222000)
           :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "278cd3e6d5107693aa2bb33189ca503f22f227d0"))
 (inheritenv :source "lockfile" :date
             (26120 31575 621231 122000)
             :recipe
             (:package "inheritenv" :fetcher github :repo "purcell/inheritenv" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "bac62ca6324828623cf8ce5a3d6aee0fcb65d620"))
 (language-id :source "lockfile" :date
              (26120 31575 620156 822000)
              :recipe
              (:package "language-id" :fetcher github :repo "lassik/emacs-language-id" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "1ad782d7e448c1e8d8652861f01f4a58315826c3"))
 (lv :source "lockfile" :date
     (26120 31575 619274 22000)
     :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el")
               :source "MELPA" :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (ef-themes :source "lockfile" :date
            (26120 31575 618379 222000)
            :recipe
            (:package "ef-themes" :repo "https://github.com/protesilaos/ef-themes" :local-repo "ef-themes" :files
                      ("*"
                       (:exclude ".git" "COPYING" "doclicense.texi" "contrast-ratios.org"))
                      :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (require 'ef-themes))
                      :ref "965053727ace036c4feb6bf2dc80d1b40e8a7965"))
 (iflipb :source "lockfile" :date
         (26120 31575 617524 22000)
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
        (26120 31575 616466 222000)
        :recipe
        (:package "vundo" :repo "https://github.com/casouri/vundo" :local-repo "vundo" :files
                  ("*"
                   (:exclude ".git" "test"))
                  :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                  (closure
                      (t)
                      nil
                    (general-define-key :states 'motion "g t" #'vundo))
                  :ref "cf440667b4ece07a74e68dc84f029779f1ddfc24"))
 (page-break-lines :source "lockfile" :date
                   (26120 31575 615615 222000)
                   :recipe
                   (:package "page-break-lines" :fetcher github :repo "purcell/page-break-lines" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                             (closure
                                 (t)
                                 nil
                               (startup-queue-package 'page-break-lines 50))
                             :ref "e33426ae7f10c60253afe4850450902919fc87fd"))
 (evil-anzu :source "lockfile" :date
            (26120 31575 614766 122000)
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
                (26120 31575 613907 922000)
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
 (vertico :source "lockfile" :date
          (26120 31575 613039 622000)
          :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/*")
                    :fetcher github :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'vertico 80))
                    :ref "68cbd47589446e9674921bae0b98ff8fbe28be23"))
 (consult :source "lockfile" :date
          (26120 31575 612163 622000)
          :recipe
          (:package "consult" :repo "minad/consult" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                    (closure
                        (t)
                        nil
                      (startup-queue-package 'consult 50))
                    :ref "b48ff6bf0527baeb6bfd07c6da9d303ff0b79c3d"))
 (orderless :source "lockfile" :date
            (26120 31575 611318 322000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (startup-queue-package 'orderless 75))
                      :ref "dc7a781acf2e58ac7d20d1b522be0cde5213e057"))
 (hotfuzz :source "lockfile" :date
          (26120 31575 610425 522000)
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
             (26120 31575 609501 922000)
             :recipe
             (:source nil :protocol ssh :inherit t :depth 1 :host github :repo "Alan-Chen99/hotfuzz-rs" :pre-build
                      (("cargo" "build" "--release")
                       ("rm" "hotfuzz-rs-module.so")
                       ("ln" "-s" "./target/release/libhotfuzz_rs_module.so" "hotfuzz-rs-module.so"))
                      :files
                      (:defaults "hotfuzz-rs-module.so")
                      :alan-init-fn
                      (closure
                          (t)
                          nil
                        (startup-queue-package 'hotfuzz-rs 76))
                      :package "hotfuzz-rs" :ref "89ebf7f74b768d209716ec533c592a64207457f8"))
 (company :source "lockfile" :date
          (26120 31575 608644 522000)
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
                    :ref "b0a522ac5bf8ba3d2f4f22e3aa846a4f82978a16"))
 (company-quickhelp :source "lockfile" :date
                    (26120 31575 607764 222000)
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
             (26120 31575 606662 222000)
             :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                       (closure
                           (t)
                           nil
                         (startup-queue-package 'marginalia 50))
                       :ref "f6fe86b989a177355ab3ff7e97a384e10a7b0bb1"))
 (flycheck :source "lockfile" :date
           (26120 31575 605524 922000)
           :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                     (closure
                         (t)
                         nil
                       (startup-queue-package 'flycheck 0))
                     :ref "79d20bf37ad90086c362dfcd8254bc94f66ed243"))
 (lsp-mode :source "lockfile" :date
           (26120 31575 604667 622000)
           :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.el")
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "782e1dc15a590ae9aa2c9b7e39c4cc8a65952e13"))
 (tree-sitter :source "lockfile" :date
              (26120 31575 603800 222000)
              :recipe
              (:package "tree-sitter" :repo "emacs-tree-sitter/elisp-tree-sitter" :fetcher github :branch "release" :files
                        (:defaults
                         (:exclude "lisp/tree-sitter-tests.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (tree-sitter-langs :source "lockfile" :date
                    (26120 31575 602819 622000)
                    :recipe
                    (:package "tree-sitter-langs" :repo "emacs-tree-sitter/tree-sitter-langs" :fetcher github :branch "release" :files
                              (:defaults "queries")
                              :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                              (closure
                                  (t)
                                  nil
                                (startup-queue-package 'tree-sitter-langs 0))
                              :ref "6aa9469a7561095515e6b4590e21f78b81b415c0"))
 (evil-textobj-tree-sitter :source "lockfile" :date
                           (26120 31575 601834 222000)
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
                                     :ref "a19ab9d89a00f4a04420f9b5d61b66f04fea5261"))
 (ts-movement :source "lockfile" :date
              (26120 31575 600891 822000)
              :recipe
              (:source nil :protocol https :inherit t :depth 1 :host github :repo "haritkapadia/ts-movement" :alan-init-fn
                       (closure
                           (t)
                           nil
                         (startup-queue-package 'ts-movement 0))
                       :package "ts-movement" :ref "7083bdeb135565c058c9cab15e5338875d8e398e"))
 (transient :source "lockfile" :date
            (26120 31575 599936 22000)
            :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref "3e30f5bff633a1d0d720305f6c8b5758b8ff1997"))
 (with-editor :source "lockfile" :date
   (26120 31575 599085 222000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :source "MELPA" :protocol https :inherit t :depth 1 :ref "5db5f0eb2202f52d44f529fe00654c866bb64eb1"))
 (magit :source "lockfile" :date
        (26120 31575 598234 522000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
                   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
                  :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                  (closure
                      (t)
                      nil
                    (startup-queue-package 'magit -50))
                  :ref "0963697f24cfbe80f92312044bd9ab28b914b053"))
 (gptel :source "lockfile" :date
        (26120 31575 597396 322000)
        :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :source "MELPA" :protocol ssh :inherit t :depth 1 :host github :alan-init-fn
                  (closure
                      (t)
                      nil
                    (defalias 'gptn
                      #'(lambda nil
                          (interactive)
                          (setq current-prefix-arg
                                '(1))
                          (call-interactively #'gptel))))
                  :ref "af66de97ae18b3d493c0cb0b4fb6c78e788d496e"))
 (rust-mode :source "lockfile" :date
            (26120 31575 596505 522000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :ref "87bf4ea711456c858445ec4c22b3552fd796708d"))
 (lsp-pyright :source "lockfile" :date
              (26120 31575 595689 722000)
              :recipe
              (:package "lsp-pyright" :repo "emacs-lsp/lsp-pyright" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :protocol https :inherit t :depth 1 :ref "2f2631ae242d5770dbe6cb924e44c1ee5671789d"))
 (pdf-tools :source "lockfile" :date
            (26120 31575 594889 722000)
            :recipe
            (:package "pdf-tools" :fetcher github :repo "vedang/pdf-tools" :files
                      (:defaults "README"
                                 ("build" "Makefile")
                                 ("build" "server"))
                      :source "MELPA" :protocol https :inherit t :depth 1 :alan-init-fn
                      (closure
                          (t)
                          nil
                        (add-to-list 'auto-mode-alist
                                     (cons "\\.pdf\\'" #'pdf-view-mode)))
                      :ref "93e74924517d39483b432d6c3c9b8f8b8f0eb50c"))
 (nix-mode :source "lockfile" :date
           (26120 31575 594037 222000)
           :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :source "MELPA" :protocol https :inherit t :depth 1 :ref "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (markdown-mode :source "lockfile" :date
                (26120 31575 593114 122000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :source "MELPA" :protocol https :inherit t :depth 1 :ref "b7f7da9b2dbab9a5e3de7a674495c0b1627b456c"))
 (better-jumper :source "lockfile" :date
                (26120 31575 591910 22000)
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
 (anzu :source "lockfile" :date
       (26120 31575 590906 622000)
       :recipe
       (:package "anzu" :fetcher github :repo "emacsorphanage/anzu" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :source "MELPA" :protocol https :inherit t :depth 1 :ref "26fb50b429ee968eb944b0615dd0aed1dd66172c"))
 (compat :source "lockfile" :date
         (26120 31575 589809 922000)
         :recipe
         (:package "compat" :repo "https://github.com/emacs-compat/compat" :local-repo "compat" :files
                   ("*"
                    (:exclude ".git"))
                   :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref "e398e1a8f098359c768b2a8e2e99d6e1d31e3c57"))
 (pos-tip :source "lockfile" :date
          (26120 31575 588746 122000)
          :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (f :source "lockfile" :date
    (26120 31575 587920 722000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref "1e7020dc0d4c52d3da9bd610d431cab13aa02d8c"))
 (ht :source "lockfile" :date
     (26120 31575 587087 522000)
     :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :source "MELPA" :protocol https :inherit t :depth 1 :ref "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (spinner :source "lockfile" :date
          (26120 31575 586237 22000)
          :recipe
          (:package "spinner" :repo "https://github.com/Malabarba/spinner.el" :local-repo "spinner" :files
                    ("*"
                     (:exclude ".git"))
                    :source "GNU-devel ELPA" :protocol https :inherit t :depth 1 :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (s :source "lockfile" :date
    (26120 31575 585318 422000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :source "MELPA" :protocol https :inherit t :depth 1 :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (tsc :source "lockfile" :date
      (26120 31575 584373 322000)
      :recipe
      (:package "tsc" :fetcher github :repo "emacs-tree-sitter/elisp-tree-sitter" :branch "release" :files
                ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src")
                :source "MELPA" :protocol https :inherit t :depth 1 :ref "3cfab8a0e945db9b3df84437f27945746a43cc71"))
 (git-commit :source "lockfile" :date
             (26120 31575 583482 22000)
             :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
                       ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
                       :old-names
                       (git-commit-mode)
                       :source "MELPA" :protocol https :inherit t :depth 1 :ref "0963697f24cfbe80f92312044bd9ab28b914b053"))
 (magit-section :source "lockfile" :date
                (26120 31575 582513 422000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi")
                          :source "MELPA" :protocol https :inherit t :depth 1 :ref "0963697f24cfbe80f92312044bd9ab28b914b053"))
 (tablist :source "lockfile" :date
          (26120 31575 581457 922000)
          :recipe
          (:package "tablist" :fetcher github :repo "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :source "MELPA" :protocol https :inherit t :depth 1 :ref "fcd37147121fabdf003a70279cf86fbe08cfac6f")))
