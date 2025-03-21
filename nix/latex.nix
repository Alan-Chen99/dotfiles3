{
  self,
  clangtools,
  legacypkgs,
  llvmpkgs,
  std,
  texlive,
}: rec {
  export.texlive-with-pkgs = texlive.withPackages (ps: [
    ps.algorithm2e
    ps.algorithmicx
    ps.algpseudocodex
    ps.asymptote
    ps.biber
    ps.biblatex
    ps.braket
    ps.cleveref
    ps.derivative
    ps.doublestroke
    ps.ellipsis
    ps.enumitem
    ps.epigraph
    ps.ifoddpage
    ps.isodate
    ps.mdframed
    ps.minted
    ps.multirow
    ps.needspace
    ps.nextpage
    ps.relsize
    ps.rsfs
    ps.siunitx
    ps.standalone
    ps.subfiles
    ps.substr
    ps.thmtools
    ps.tikz-cd
    ps.todonotes
    ps.wrapfig
    ps.xstring
    ps.yhmath
    ps.zref
  ]);

  export.latexenv = std.buildEnv {
    name = "latex-env";
    paths = [
      self.texlive-with-pkgs
      legacypkgs.perlPackages.LatexIndent
      legacypkgs.texlab
    ];
  };
}
