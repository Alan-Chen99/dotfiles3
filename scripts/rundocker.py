#!/usr/bin/env -S nix shell p#python.inject.simple-parsing --command python

import os
import shlex
import sys
from dataclasses import dataclass
from pathlib import Path

import simple_parsing


@dataclass
class Args:
    image: str | None = simple_parsing.field(positional=True, default=None)


args = simple_parsing.parse(Args)

HOME = Path(os.environ["HOME"]).resolve()
DISPLAY = os.environ["DISPLAY"]
SSL_CERT_FILE = os.environ.get("SSL_CERT_FILE", "")

extra_path1 = HOME / ".nix-profile/bin"
extra_path2 = HOME / ".local/docker_static_bin"

flags: list[str | list[str]] = [
    "docker",
    "run",
    "--interactive",
    "--tty",
    "--rm",
    ["--gpus", "all"],
    ["-v", "/nix:/nix"],
    ["-v", f"{extra_path1}:/host_bin"],
    ["-v", f"{extra_path2}:/host_static_bin"],
    ["-v", f"{HOME}:{HOME}"],
    ["--tmpfs", "/tmp:exec,mode=1777"],
    ["--tmpfs", "/home/dockeruser:exec,mode=1777"],
    ["-v", f"{HOME}/.cache/nix:/home/dockeruser/.cache/nix"],
    ["--user", "1000:0"],
    ["--workdir", os.getcwd()],
    ["-e", f"DISPLAY={DISPLAY}"],
    ["-e", "HOME=/home/dockeruser"],
    ["-e", f"SSL_CERT_FILE={SSL_CERT_FILE}"],
    ["-e", r"PS1=${debian_chroot:+($debian_chroot)}\u@\H:\w\$ "],
    ["-v", "/tmp/.X11-unix:/tmp/.X11-unix"],
    args.image if args.image is not None else "shinsenter/scratch",
    [
        "/host_bin/bash",
        "--norc",
        "--noprofile",
        "-i",  # bash otherwise drops $PS1
        "-c",
        """
        exec /host_bin/env PATH="/host_bin:/host_static_bin:${PATH}" bash --norc --noprofile -i
        """,
    ],
]


def gen():
    for x in flags:
        if isinstance(x, list):
            yield from x
        else:
            yield x


args = list(gen())
# print(" ".join(shlex.quote(x) for x in args))

os.execlp("docker", *args)
