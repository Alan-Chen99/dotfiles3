#!/usr/bin/env -S nix shell p#python.inject.cappa --command python

import os
import shlex
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Annotated, Sequence

import cappa
from cappa import Arg
from rich import print


@dataclass
class Args:
    # image: Annotated[str, Arg(short=True)] = "shinsenter/scratch"
    # image: Annotated[str, Arg(short=True)] = "gcr.io/distroless/static-debian12"
    image: Annotated[str, Arg(short=True)] = "ubuntu"

    uid: Annotated[int, Arg(short=True)] = 0

    name: Annotated[str | None, Arg(short=True)] = None

    docker_flags: list[str] = field(default_factory=list)


args = cappa.parse(Args)

DISPLAY = os.environ["DISPLAY"]
SSL_CERT_FILE = os.environ.get("SSL_CERT_FILE", "")

host_prof = "/home/alan/.nix-profile"


share_paths_ro = [
    "/nix",
    "/home/alan/.local/docker_static_bin",
    "/home/alan/.nix-profile",
    "/home/alan/.local/state/nix",
    "/home/alan/repos/dotfiles",
    #
    "/home/alan/.local/bin/claude",
    "/home/alan/.local/share/claude",
]

share_paths_rw = [
    os.getcwd(),
    #
    "/home/alan/.local/share/uv",
    "/home/alan/.cache/nix",
    "/home/alan/.emacs.d",
    #
    "/home/alan/.config/opencode",
    "/home/alan/.local/share/opencode",
    "/home/alan/.local/state/opencode",
    "/home/alan/.opencode",
    #
    "/home/alan/.claude",
    "/home/alan/.claude.json",
    #
    "/tmp/.X11-unix",
]


PATH = [
    "/home/alan/.local/bin",
    "/home/alan/.local/docker_static_bin",
    "/home/alan/.nix-profile/bin",
    #
    "/home/alan/.opencode/bin",
]

envs = {
    "DISPLAY": DISPLAY,
    "HOME": "/home/alan",
    "SSL_CERT_FILE": SSL_CERT_FILE,
    "PS1": r"${debian_chroot:+($debian_chroot)}\u@\H:\w\$ ",
    "PATH": ":".join(PATH),
}


type FlagsT = str | Sequence[FlagsT]

flags: FlagsT = [
    "docker",
    "run",
    "--interactive",
    "--tty",
    "--rm",
    # ["--gpus", "all"],
    # ["--network", "none"],
    # "--privileged",
    # ["--security-opt", "seccomp=unconfined"],
    # "--cap-add=NET_RAW",
    ["--name", args.name] if args.name is not None else [],
    #
    [["-v", f"{x}:{x}:ro"] for x in share_paths_ro],
    [["-v", f"{x}:{x}:rw"] for x in share_paths_rw],
    ["-v", f"{host_prof}:/docker_host_prof:ro"],
    #
    ["--tmpfs", "/tmp:exec,mode=1777"],
    ["--tmpfs", f"/home/dockeruser:exec,mode=1777,uid={args.uid},gid=0"],
    #
    [["-e", f"{k}={v}"] for k, v in envs.items()],
    #
    ["--user", f"{args.uid}:0"],
    ["--workdir", os.getcwd()],
    ["--entrypoint", "/docker_host_prof/bin/bash"],
    args.docker_flags,
    args.image,
    ["--norc", "--noprofile", "-i"],
]


def resolve(flags: FlagsT):
    if isinstance(flags, str):
        yield flags
    else:
        for x in flags:
            yield from resolve(x)


str_args = list(resolve(flags))

# print(flags)
# print(" ".join(shlex.quote(x) for x in str_args))

os.execlp("docker", *str_args)
