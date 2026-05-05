{
  self,
  callpackage,
  lib,
  nix,
  std,
  nixconf,
  super,
}: rec {
  # does not propagate less-download-flakes, etc correctly
  # env = callpackage ./env.nix {} (final: prev: {
  #   nixconf =
  #     prev.nixconf
  #     // {
  #       allow-unsafe-native-code-during-evaluation = true;
  #     };
  # });

  nixconf-modified =
    nixconf
    // {
      # TODO: nix-doc doesnt build
      # plugin-files = "${nix-doc}/lib/libnix_doc_plugin.so";
      allow-unsafe-native-code-during-evaluation = true;
    };

  nixconf-file =
    std.writeText "nixconf"
    (lib.concatStringsSep "\n" (builtins.attrValues
        (builtins.mapAttrs (name: value: "${name} = ${builtins.toString value}") nixconf-modified)));

  export.nixrepl-wrapper = std.writeScriptBin "nix-repl" ''
    #! ${std.stdenv.shell}
    export NIX_USER_CONF_FILES=${nixconf-file}
    exec ${nix}/bin/nix repl --print-build-logs "$@" --expr "import <prelude>"
    # exec ${nix}/bin/nix repl --print-build-logs "$@"
  '';
}
