let
  dotfiles = import ./local.nix {system = builtins.currentSystem;};
  default = dotfiles.default;
  dbg = default.dbg;
in
  builtins
  // dbg.__clean
  // {
    builtins = builtins.removeAttrs builtins ["builtins"];
    df = dotfiles;
    mods = dotfiles.__mods;
    df-inputs = dotfiles.inputs;
    inherit (default) dbg lib deps std;
    pkgs = default.legacypkgs;
  }
