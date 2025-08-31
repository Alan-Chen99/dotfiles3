#!/usr/bin/env -S nix shell p#python.inject.cappa --command python
import os
import shlex
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Annotated

import cappa
from cappa import Arg


@dataclass
class Args:
    image: Annotated[str, Arg(short=True)] = "shinsenter/scratch"

    uid: Annotated[int, Arg(short=True)] = 1000

    name: Annotated[str | None, Arg(short=True)] = None

    docker_flags: list[str] = field(default_factory=list)


args = cappa.parse(Args)

# print(args)
# assert False

HOME = Path(os.environ["HOME"]).resolve()
DISPLAY = os.environ["DISPLAY"]
SSL_CERT_FILE = os.environ.get("SSL_CERT_FILE", "")

extra_path1 = HOME / ".nix-profile/bin"
extra_path2 = HOME / ".local/docker_static_bin"

host_bin_path = "/docker_host_prof/bin"

flags: list[str | list[str]] = [
    "docker",
    "run",
    "--interactive",
    "--tty",
    "--rm",
    ["--gpus", "all"],
    # ["--network", "none"],
    # "--privileged",
    # ["--security-opt", "seccomp=unconfined"],
    # "--cap-add=NET_RAW",
    ["--name", args.name] if args.name is not None else [],
    ["-v", f"/nix:/nix"],
    ["-v", f"{extra_path1}:{host_bin_path}:ro"],
    #
    # TODO: this dont work if the base image uses busybox
    # causing stuff to be symlinked to /bin/sh
    # which then dont work
    ["-v", f"{extra_path1}/sh:/bin/sh:ro"],
    #
    # ["-v", f"{extra_path1}/bash:/bin/bash:ro"],
    # ["-v", f"{extra_path1}/ls:/bin/ls:ro"],
    ["-v", f"{extra_path2}:/host_static_bin:ro"],
    ["-v", f"{HOME}:{HOME}"],
    ["--tmpfs", "/tmp:exec,mode=1777"],
    ["--tmpfs", f"/home/dockeruser:exec,mode=1777,uid={args.uid},gid=0"],
    ["-v", f"{HOME}/.cache/nix:/home/dockeruser/.cache/nix"] if args.uid == 0 else [],
    ["--user", f"{args.uid}:0"],
    ["--workdir", os.getcwd()],
    ["-e", f"DISPLAY={DISPLAY}"],
    ["-e", "HOME=/home/dockeruser"],
    ["-e", f"SSL_CERT_FILE={SSL_CERT_FILE}"],
    ["-e", r"PS1=${debian_chroot:+($debian_chroot)}\u@\H:\w\$ "],
    ["-e", f"PATH={host_bin_path}:/host_static_bin"],
    ["-v", "/tmp/.X11-unix:/tmp/.X11-unix"],
    ["--entrypoint", f"{host_bin_path}/bash"],
    *args.docker_flags,
    args.image,
    ["--norc", "--noprofile", "-i"],
    # [
    #     "--norc",
    #     "--noprofile",
    #     "-i",  # bash otherwise drops $PS1
    #     "-c",
    #     """
    #     exec {host_bin_path}/env PATH="{host_bin_path}:/host_static_bin:${PATH}" bash --norc --noprofile -i
    #     """,
    # ],
]


def gen():
    for x in flags:
        if isinstance(x, list):
            yield from x
        else:
            yield x


str_args = list(gen())
# print(" ".join(shlex.quote(x) for x in str_args))

os.execlp("docker", *str_args)
