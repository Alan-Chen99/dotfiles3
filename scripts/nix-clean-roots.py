#!/bin/env python3

from pathlib import Path


def get_yes_or_no(prompt: str):
    while True:
        response = input(prompt).lower().strip()
        if response in ["yes", "y"]:
            return True
        elif response in ["no", "n"]:
            return False
        else:
            print("Please enter yes or no.")


paths = [path.readlink() for path in Path("/nix/var/nix/gcroots/auto/").iterdir()]
paths = [path for path in paths if path.parts[1] == "home" and path.exists()]

for path in paths:
    if "profile" not in path.name:
        print(path)

for path in paths:
    if "result" in path.name and "persist" not in path.name:
        if get_yes_or_no(f"remove {str(path)!r} ? "):
            path.unlink()
