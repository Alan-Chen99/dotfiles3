let
  dotfiles = import ./local.nix;
  dbg = dotfiles.dbg;
in
  builtins
  // dbg.__clean
  // {
    builtins = builtins.removeAttrs builtins ["builtins"];
    df = dotfiles;
    mods = dotfiles.mods;
    df-inputs = dotfiles.inputs;
    inherit (dotfiles) dbg lib deps std;
    pkgs = dotfiles.legacypkgs;
  }
