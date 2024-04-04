{
  self,
  bashInteractive,
  cacert,
  cachix,
  coreutils,
  nix-stable,
  std,
  tree,
  which,
}: rec {
  export.profile-root = std.buildEnv {
    name = "profile-root";
    checkCollisionContents = false;
    paths = [
      bashInteractive
      cacert
      cachix
      coreutils
      nix-stable
      tree
      which
    ];
  };
}
