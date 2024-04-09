{
  self,
  alejandra,
  flamegraph,
  git,
  nix,
  nix-stable,
  nix-tree,
  nixd,
  python,
  std,
  nixconf-file,
}: rec {
  # https://discourse.nixos.org/t/nix-flamegraph-or-profiling-tool/33333
  nix-instantiate-flamegraph = std.writeShellScriptBin "nix-instantiate-flamegraph" ''
    WORKDIR=$(mktemp -d /tmp/nix-fun-calls-XXXXX)
    ${nix}/bin/nix-instantiate --trace-function-calls "$1" -A "$2" 2> $WORKDIR/nix-function-calls.trace 1>/dev/null
    ${python}/bin/python ${nix.src}/contrib/stack-collapse.py $WORKDIR/nix-function-calls.trace > $WORKDIR/nix-function-calls.folded
    ${flamegraph}/bin/flamegraph.pl $WORKDIR/nix-function-calls.folded > $WORKDIR/nix-function-calls.svg
    echo $WORKDIR/nix-function-calls.svg
  '';

  export.nix-tree-with-stable =
    std.runCommandLocal "nix-tree" {
      nativeBuildInputs = [std.makeWrapper];
    } ''
      mkdir $out
      makeWrapper ${nix-tree}/bin/nix-tree $out/bin/nix-tree \
        --set PATH ${nix-stable}/bin:${git}/bin \
        --set NIX_USER_CONF_FILES ${nixconf-file}
    '';

  export.nixtools = std.buildEnv {
    name = "nix tools";
    paths = [
      alejandra
      nix-instantiate-flamegraph
      self.nix-tree-with-stable
      nixd
    ];
    pathsToLink = ["/bin"];
  };
}
