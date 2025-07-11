{
  #
  system ? builtins.currentSystem,
}: let
  flake-compat =
    import
    (
      let
        lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      in
        fetchTarball {
          url = "https://github.com/inclyc/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
          sha256 = lock.nodes.flake-compat.locked.narHash;
        }
    );
  flake =
    (
      flake-compat
      {
        src = ./.;
        inherit system;
      }
    )
    .defaultNix;
in
  flake.legacyPackages."${system}" // {__flake = flake;}
