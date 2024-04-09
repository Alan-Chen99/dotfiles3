{
  self,
  flakes,
  lib,
  nix,
  nixpkgs-flakes,
  source-ver,
  std,
  dbg,
}: rec {
  flakelock = let
    flakelock = builtins.fromJSON (builtins.readFile ../flake.lock);
  in
    assert flakelock.version == 7; flakelock;

  hash-to-source-mapping = builtins.listToAttrs (
    lib.attrsets.mapAttrsToList (name: value: {
      name = value.locked.narHash + (value.locked.dir or "");
      value = value.locked;
    })
    (removeAttrs flakelock.nodes [flakelock.root])
  );

  flakes-with-source =
    builtins.mapAttrs (name: val: hash-to-source-mapping."${val.narHash}${val.dir or ""}")
    (removeAttrs flakes ["self"]);

  export.flake-registry =
    (removeAttrs flakes-with-source ["nixpkgs-lib"])
    // {
      p = source-ver;
      n = flakes-with-source.nixpkgs;
      dotfiles = source-ver;
    };

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

  export.nix-path = {
    n = "${nixpkgs-flakes}";
    nixpkgs = "${nixpkgs-flakes}";
    prelude = "${source-ver}/nix/prelude.nix";
  };

  export.nixconf = {
    nix-path =
      lib.concatStringsSep " " (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name}=${value}") self.nix-path));
    flake-registry = "${self.flake-registry-file}/registry.json";
    allow-import-from-derivation = true;
    extra-experimental-features = "nix-command flakes recursive-nix";
  };

  export.nixconf-file =
    std.writeText "nixconf"
    (lib.concatStringsSep "\n" (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name} = ${builtins.toString value}") self.nixconf)));

  export.nixwrapper = std.mkDerivation rec {
    name = "nixwrapper";
    preferLocalBuild = true;

    nativeBuildInputs = [std.makeWrapper];
    phases = ["buildPhase"];
    buildPhase = ''
      mkdir -p $out/bin
      for exe in $(ls ${nix}/bin); do
        ln -s ${nix}/bin/$exe $out/bin/$exe-unwrapped
        makeWrapper ${nix}/bin/$exe $out/bin/$exe \
          --set NIX_USER_CONF_FILES ${self.nixconf-file}
      done

      rm $out/bin/nix
      makeWrapper ${nix}/bin/nix $out/bin/nix \
        --add-flags --print-build-logs \
        --set NIX_USER_CONF_FILES ${self.nixconf-file}
    '';
  };
}
