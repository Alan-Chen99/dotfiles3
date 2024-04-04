{
  self,
  callpackage,
  nix-doc,
  nix-plugins,
  std,
  nix,
}: rec {
  env = callpackage ./env.nix {} (final: prev: {
    nixconf =
      prev.nixconf
      // {
        plugin-files = "${nix-doc}/lib/libnix_doc_plugin.so";
        allow-unsafe-native-code-during-evaluation = true;
      };
  });

  export.nixrepl-wrapper = std.writeScriptBin "nix-repl" ''
    #! ${std.stdenv.shell}
    export NIX_USER_CONF_FILES=${env.nixconf-file}
    # exec ${nix}/bin/nix repl "$@" "<prelude>"
    exec ${nix}/bin/nix repl --print-build-logs "$@"
  '';
}
