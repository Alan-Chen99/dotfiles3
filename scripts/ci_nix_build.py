#!/bin/env python3
import os
import subprocess
import sys
from pathlib import Path
from typing import Any, Callable, Generic, TypeVar

T = TypeVar("T")


class cast_unchecked(Generic[T]):
    def __init__(self, _: T):
        pass

    def __call__(self, a) -> T:
        return a


_old_print = print


@cast_unchecked(print)
def print(*args, **kwargs):
    kwargs.setdefault("flush", True)
    return _old_print(*args, **kwargs)


@cast_unchecked(subprocess.run)
def run(cmd: str, **kwargs):
    print(f":: {cmd}", flush=True)
    kwargs.setdefault("shell", True)
    kwargs.setdefault("check", True)
    return subprocess.run(cmd, **kwargs)


gh_output = Path(os.environ["GITHUB_OUTPUT"])


def write_gh_out(**envs: str):
    content = "".join(f"{x}={y}\n" for x, y in envs.items())
    gh_output.write_text(content)
    print()
    print("outputs:")
    print(gh_output.read_text())
    print()


step = sys.argv[1]

if step == "instantiate":
    drv_name = sys.argv[2]
    cache_path_root = sys.argv[3]
    drv_path = run(
        f"nix path-info --derivation {drv_name}", stdout=subprocess.PIPE
    ).stdout.decode()
    drv_path = drv_path.removesuffix("\n")
    write_gh_out(
        drv_path=drv_path,
        cache_key=f"nix-{drv_name}-{drv_path}",
        restore_key=f"nix-{drv_name}-",
        cache_path=str(Path(cache_path_root) / drv_name),
    )


elif step == "load_cached":
    cache_path = sys.argv[2]
    cache_key = sys.argv[3]
    try:
        run(f"nix copy --no-check-sigs --all --from {cache_path}")
    except subprocess.CalledProcessError:
        f"::error file={__file__}::failed to load from cache entry {cache_key}"
        raise

elif step == "build":
    drv_name = sys.argv[2]
    drv_path = sys.argv[3]
    cache_path = sys.argv[4]
    cache_hit = sys.argv[5]
    assert cache_hit in ["", "true", "false"], f"invalid value for {cache_hit!r}"

    drv_out = run(
        f"nix build --print-build-logs --no-link --print-out-paths {drv_path}^*",
        stdout=subprocess.PIPE,
    ).stdout.decode()
    drv_out = drv_out.removesuffix("\n")
    drv_out_l = drv_out.split("\n")
    print("got outputs:")
    for x in drv_out_l:
        print(x)

    drv_out_space_sep = " ".join(drv_out_l)

    if cache_hit != "true":
        run(
            f"nix copy --no-check-sigs --to {cache_path} {drv_out_space_sep}",
            stdout=subprocess.PIPE,
        )

    write_gh_out(
        drv_out_space_sep=drv_out_space_sep,
    )

else:
    assert False
