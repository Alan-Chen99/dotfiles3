{
  self,
  system,
  python,
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

  # here we put the same stuff in the lock file into flake registery
  # we only do this for flakes we input directly
  # ~we lookup their rev (previously narHash) in the lock file and get info there
  # ~note: new lock file seem to not contain narHash anymore
  # above seem to be a bug in nix/lazy-trees
  # TODO: is this behavior what i actually want?
  hash-to-source-mapping = builtins.listToAttrs (
    lib.attrsets.mapAttrsToList (name: value: {
      name = "${value.locked.narHash}---${value.locked.dir or ""}";
      value = value.locked;
    })
    (removeAttrs flakelock.nodes [flakelock.root])
  );

  flakes-with-source =
    builtins.mapAttrs (name: val: hash-to-source-mapping."${val.narHash}---${val.dir or ""}")
    (removeAttrs flakes ["self"]);

  _flake-registry = {
    p = "${source-ver}";
    n = "${nixpkgs-flakes}";
    dotfiles = "${source-ver}";
  };

  export.flake-registry =
    if self.less-download-flakes
    then _flake-registry
    else (removeAttrs flakes-with-source ["nixpkgs-lib"]) // _flake-registry;

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
    n = "${nixpkgs-flakes}";
    nixpkgs = "${nixpkgs-flakes}";
    df = "${source-ver}/nix/local.nix";
    prelude = "${source-ver}/nix/prelude.nix";
  };

  export.nix-path =
    if self.less-download-flakes
    then _nix-path
    else (removeAttrs flakes ["self"]) // _nix-path;

  export.nixconf = {
    nix-path =
      lib.concatStringsSep " " (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name}=${value}") self.nix-path));
    flake-registry = "${self.flake-registry-file}/registry.json";
    allow-import-from-derivation = true;
    extra-experimental-features = "nix-command flakes recursive-nix";
    cores = "2";
  };

  export.nixconf-file =
    std.writeText "nixconf"
    (lib.concatStringsSep "\n" (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name} = ${builtins.toString value}") self.nixconf)));

  export.nixwrapper =
    derivation {
      name = "nixwrapper";
      builder = "${python}/bin/python";
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
