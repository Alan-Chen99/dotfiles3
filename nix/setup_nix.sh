#!/bin/bash

set -eux

# not tested as a whole
exit 0

cd "$(dirname "$0")"
cd ..

sh <(curl -L https://nixos.org/nix/install) --daemon

nix profile install \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	.#p.cachix

echo "trusted-users = root alan" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon

cachix use alan-chen-public

nix flake show \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	--option allow-import-from-derivation true

# .#less-build.profile

nix build \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	--option allow-import-from-derivation true \
	--print-build-logs \
	.#profile
nix profile install \
	--extra-experimental-features nix-command \
	--extra-experimental-features flakes \
	--option allow-import-from-derivation true \
	--print-build-logs \
	--priority 6 \
	.#profile

nix profile install .#emacs
# nix profile install .#less-build.emacs

mkdir ~/.emacs.d
ln -s $(pwd)/emacs/.emacs ~/.emacs.d/init.el
ln -s $(pwd)/emacs/early-init.el ~/.emacs.d/early-init.el

emacs --batch -l emacs/ci.el --eval "(ci-byte-compile)"

# add to .profile:
if [ -f "${HOME}/.nix-profile/env/.env" ]; then
	. "${HOME}/.nix-profile/env/.env"
fi

home-manager switch --flake .
