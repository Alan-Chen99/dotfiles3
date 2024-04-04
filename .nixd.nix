# https://github.com/nix-community/nixd/blob/main/docs/user-guide.md
# nix eval --json --file .nixd.nix > .nixd.json
{
  eval = {
    target = {
      # nix eval -f nixd-entry.nix nixd-eval
      args = ["-f" "./nixd-entry.nix"];
      installable = "nixd-eval";
    };
    depth = 0;
    workers = 1;
  };
  options = {
    enable = false;
  };
}
