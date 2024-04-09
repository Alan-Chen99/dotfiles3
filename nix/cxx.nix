{
  self,
  legacypkgs,
  llvmpkgs,
  clangtools,
  std,
}: rec {
  # llvm = llvmpkgs.llvm;

  export.cxxtools = std.buildEnv {
    name = "cxxtools";
    paths = [
      clangtools
    ];
  };
}
