{
  self,
  system,
  python-nopkgs,
  flakes,
  lib,
  nix,
  nixpkgs-flakes,
  source-ver,
  std,
  dbg,
}: rec {
  export.less-download-flakes = false;

  flakelock = let
    flakelock = builtins.fromJSON (builtins.readFile ../flake.lock);
  in
    assert flakelock.version == 7; flakelock;

  # lock file entries for direct inputs, looked up by name to avoid
  # touching live flake inputs (which would force downloads)
  flakes-with-source = let
    root-inputs = flakelock.nodes.${flakelock.root}.inputs;
    resolve = ref:
      if builtins.isList ref
      then let
        node = flakelock.nodes.${builtins.head ref};
      in
        resolve node.inputs.${builtins.elemAt ref 1}
      else ref;
  in
    builtins.mapAttrs (
      name: _:
        flakelock.nodes.${resolve root-inputs.${name}}.locked
    ) (removeAttrs flakes ["self"]);

  # extra things added to flake inputs
  _flake-registry = {
    n = flakes-with-source.nixpkgs;
    nixpkgs = flakes-with-source.nixpkgs;
    #
    df = "${source-ver}";
    dotfiles = "${source-ver}";
    p = "${source-ver}";
  };

  export.flake-registry =
    (removeAttrs flakes-with-source ["nixpkgs-lib"]) // _flake-registry;

  flake-registry-list = {
    version = 2;
    flakes =
      lib.attrsets.mapAttrsToList (name: val: {
        from = {
          id = name;
          type = "indirect";
        };
        to =
          if lib.isString val
          then {
            path = builtins.toString val;
            type = "path";
          }
          else val;
      })
      self.flake-registry;
  };

  export.flake-registry-file = std.runCommandLocal "flake-registry" {} ''
    mkdir $out
    echo ${lib.strings.escapeShellArg (builtins.toJSON flake-registry-list)} >> $out/registry.json
    # run some command to validate
    ${nix}/bin/nix --extra-experimental-features "nix-command flakes" --store dummy:// --offline \
      registry remove --registry $out/registry.json dummy-nonexistent
  '';

  _nix-path = {
    df = "${source-ver}";
    dotfiles = "${source-ver}";
    n = "${nixpkgs-flakes}";
    nixpkgs = "${nixpkgs-flakes}";
    p = "${source-ver}";
    prelude = "${source-ver}/nix/prelude.nix";
  };

  export.nix-path =
    if self.less-download-flakes
    then _nix-path
    else (removeAttrs flakes ["self"]) // _nix-path;

  export.nixconf = {
    # see also https://jackson.dev/post/nix-reasonable-defaults/
    nix-path =
      lib.concatStringsSep " " (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name}=${value}") self.nix-path));
    flake-registry = "${self.flake-registry-file}/registry.json";
    allow-import-from-derivation = true;
    extra-experimental-features = "nix-command flakes recursive-nix";

    # for binary cache
    connect-timeout = 5;
    fallback = true;

    auto-optimise-store = true;

    keep-outputs = true;

    narinfo-cache-negative-ttl = 0;

    cores = "2";
  };

  export.nixconf-file =
    std.writeText "nixconf"
    (lib.concatStringsSep "\n" (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name} = ${builtins.toString value}") self.nixconf)));

  export.nixwrapper =
    derivation {
      name = "nixwrapper";
      builder = "${python-nopkgs}/bin/python";
      system = system;
      args = [
        "${./make_nix_wrapper.py}"
        "${std.stdenv.shell}"
        "${nix}"
        "${self.nixconf-file}"
      ];
    }
    // {
      meta.mainProgram = "nix";
    };
}
