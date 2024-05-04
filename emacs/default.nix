{
  self,
  emacs-base,
  legacypkgs,
  mcc-env,
  mcc-hook,
  tree,
  std,
  dbg,
}: rec {
  # export.emacs = emacs-base;
  export.emacs =
    (emacs-base.override (prev: {
      withCsrc = false;
      stdenv = mcc-env;
    }))
    .overrideAttrs (final: prev: {
      MCC_BUILD_DIR = "$out/${final.pname}-${final.version}-src";

      MCC_KEEP_REGEXP = ''
        \.c$|
        \.h$|
        \.m$|
        ^TAGS$
      '';
    });

  export.emacs-test =
    (legacypkgs.hello.override (prev: {
      stdenv = mcc-env;
    }))
    .overrideAttrs (final: prev: {
      MCC_BUILD_DIR = "$out/${final.pname}-${final.version}-src";
    });

  pdf-tools = (legacypkgs.emacsPackagesFor emacs-base).pdf-tools;

  export.pdf-tools-epdfinfo =
    std.runCommandLocal "pdf-tools-epdfinfo" {}
    ''
      mkdir -p $out/bin
      cp ${pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-${pdf-tools.version}/epdfinfo \
       $out/bin/epdfinfo
    '';
}
