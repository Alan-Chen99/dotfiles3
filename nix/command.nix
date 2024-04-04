{
  self,
  std,
  system,
}: {
  export.run-cmd = name: cmd:
    builtins.derivation {
      name = name;
      builder = std.stdenv.shell;
      system = system;
      args = [
        "-c"
        cmd
      ];
    };
}
