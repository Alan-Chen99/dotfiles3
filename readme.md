## start on new machine

```
nix flake show --option allow-import-from-derivation true

nix build .#profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#emacs

nix build .#less-build.profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#less-build.profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#less-build.emacs
```
