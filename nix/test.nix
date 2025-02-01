{
  self,
  lib,
  src,
  std,
  tree,
  which,
  system,
  coreutils,
  nix,
  llvmpkgs,
}: {
  export.test = derivation {
    name = "test-drv";
    builder = std.stdenv.shell;
    system = system;
    args = [
      "-c"
      ''
        ${coreutils}/bin/printenv
        ${tree}/bin/tree -a /build
        # ${coreutils}/bin/ls -la /nix/store

        echo "hello" >> $out
        echo "${std.stdenv.shell}" >> $dev
      ''
    ];
    outputs = ["out" "dev"];
    # requiredSystemFeatures = ["recursive-nix"];
    # NIX_REMOTE_RECURSIVE_PROTOCOL_VERSION = "0x101";
  };

  # test2 = std.mkDerivation {
  #   name = "test-drv";
  #   buildInputs = [nix_2_16];
  #   phases = ["buildPhase"];
  #   buildPhase = ''
  #     printenv
  #     exit 1
  #   '';
  # };

  export.test2 = let
    # stdenv = nix.stdenv;
    stdenv = llvmpkgs.stdenv;
    cc = stdenv.cc;
    # CXXFLAGS = builtins.concatStringsSep " " [
    #   (lib.removeSuffix "\n" (builtins.readFile "${cc}/nix-support/cc-cflags"))
    #   (lib.removeSuffix "\n" (builtins.readFile "${cc}/nix-support/libc-cflags"))
    #   (lib.removeSuffix "\n" (builtins.readFile "${cc}/nix-support/libcxx-cxxflags"))
    # ];
  in
    std.runCommandCC "nix-get-cflags" {
      stdenv = stdenv;
      # buildInputs = [nix] ++ nix.buildInputs;
      buildInputs = [nix] ++ nix.buildInputs;
    } ''
      echo "c compiler: ${cc}/bin/c++"
      mkdir $out
      ln -s ${nix} $out/nix
      ln -s ${cc} $out/cc
      ln -s $(cat ${cc}/nix-support/orig-cc) $out/unwrapped
      (
        echo $NIX_CFLAGS_COMPILE
        echo " -isystem ${nix.dev}/include/nix "
        cat ${cc}/nix-support/cc-cflags
        cat ${cc}/nix-support/libc-cflags
        cat ${cc}/nix-support/libcxx-cxxflags
      ) >> $out/cxx_flags
      (
        echo $NIX_LDFLAGS
        cat ${cc}/nix-support/cc-ldflags
        cat ${cc}/nix-support/libc-ldflags
        cat ${cc}/nix-support/libcxx-ldflags
      ) >> $out/ld_flags
    '';
}
