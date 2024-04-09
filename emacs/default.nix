{
  self,
  emacs,
  legacypkgs,
  std,
}: rec {
  pdf-tools = (legacypkgs.emacsPackagesFor emacs).pdf-tools;

  export.pdf-tools-epdfinfo =
    std.runCommandLocal "pdf-tools-epdfinfo" {}
    ''
      mkdir -p $out/bin
      cp ${pdf-tools}/share/emacs/site-lisp/elpa/pdf-tools-${pdf-tools.version}/epdfinfo \
       $out/bin/epdfinfo
    '';
}
