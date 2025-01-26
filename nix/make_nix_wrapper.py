import os
import sys
from pathlib import Path

bash_path = sys.argv[1]
nix_path = sys.argv[2]
nixconf_file_path = sys.argv[3]

out_path = Path(os.environ["out"]) / "bin"
out_path.mkdir(parents=True)

for x in (Path(nix_path) / "bin").iterdir():
    wrapped = [
        f"#! {bash_path} -e",
        # $HOME/.config/nix/nix.conf allows user and/or magic-nix-cache to inject stuff locally
        f'export NIX_USER_CONF_FILES="$NIX_USER_CONF_FILES:$HOME/.config/nix/nix.conf:{nixconf_file_path}"',
        f'exec -a "$0" {x}{" --print-build-logs" if x.name == "nix" else ""} "$@"',
        f"",
    ]
    out_f = out_path / x.name
    out_f.write_text("\n".join(wrapped))
    out_f.chmod(0o555)
