#!/usr/bin/env -S nix shell p#python.inject.simple-parsing.rich --command python

import os
import shlex
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

import simple_parsing
from rich import print
from simple_parsing import field


@dataclass
class Args:
    nix: list[str] = field(
        default_factory=lambda: [
            "n#nixVersions.nix_2_28",
            ".#deps.nix",
            ".#nixwrapper",
        ],
    )
    drv: list[str] = field(
        default_factory=lambda: [".#profile", "p#profile"],
    )


def run_one(nix: str, drv: str) -> str | None:
    cmd = [
        *["nix", "run"],
        nix,
        *["--", "path-info", "--derivation"],
        drv,
        *["--option", "eval-cache", "false"],
    ]
    print(f"running")
    print(" ".join(shlex.quote(x) for x in cmd))

    res = subprocess.run(cmd, stdout=subprocess.PIPE, text=True)

    if res.returncode != 0:
        print(f"[bold red]failed to instantiate {drv} using {nix}[/bold red]")
        return

    ans = res.stdout.removesuffix("\n")
    print("result:", ans)
    return ans


def main():
    args = simple_parsing.parse(Args)

    did_error = False

    items: dict[str, list[tuple[str, str]]] = {}

    for nix in args.nix:
        for drv in args.drv:
            if (ans := run_one(nix, drv)) is not None:
                items.setdefault(ans, []).append((nix, drv))
            else:
                did_error = True

    if len(items.keys()) > 1:
        did_error = True
        print("[bold red]error: mismatch[/bold red]")
        print(items)
    else:
        print("all outputs are the same")

    sys.exit(1 if did_error else 0)


if __name__ == "__main__":
    main()
