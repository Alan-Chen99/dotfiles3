#!/bin/bash

set -eux

cd "$(dirname "$0")"

ln -s /mnt/wslg/runtime-dir/wayland-0* /run/user/1000/ || true

cp -f ./wsl.conf /etc/wsl.conf

# do this to get wayland to work
# https://github.com/microsoft/wslg/issues/1032#issuecomment-2310369848
