#!/bin/env python3

import os
import shlex
import sys
from pathlib import Path

HOME = Path(os.environ["HOME"]).resolve()
DISPLAY = os.environ["DISPLAY"]

extra_path = HOME / ".nix-profile/bin"

# os.chdir(Path(__file__).parent)

flags: list[str | list[str]] = [
    "docker",
    "run",
    "--interactive",
    "--tty",
    "--rm",
    ["--gpus", "all"],
    ["-v", "/nix:/nix"],
    ["-v", f"{HOME}:{HOME}"],
    ["-v", f"{HOME}/.cache/nix:/home/dockeruser/.cache/nix"],
    ["--user", "1000:1000"],
    ["--workdir", os.getcwd()],
    ["-e", f"DISPLAY={DISPLAY}"],
    ["-v", "/tmp/.X11-unix:/tmp/.X11-unix"],
    "simple",
    "bash",
    "-c",
    f'exec env PATH="{extra_path}:${{PATH}}" bash -l',
]


def gen():
    for x in flags:
        if isinstance(x, list):
            yield from x
        else:
            yield x


args = list(gen())
print(" ".join(shlex.quote(x) for x in args))

os.execlp("docker", *args)
