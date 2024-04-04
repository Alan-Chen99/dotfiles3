let
  dotfiles = import ./local.nix;
  default = dotfiles.default;
  dbg = default.dbg;
in
  builtins
  // dbg.__clean
  // {
    builtins = builtins.removeAttrs builtins ["builtins"];
    df = dotfiles;
    df-inputs = dotfiles.export.legacyPackages.inputs;
    inherit (default) dbg lib deps std;
    pkgs = default.legacypkgs;
  }
