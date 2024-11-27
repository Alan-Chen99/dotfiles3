import ctypes
import os
import subprocess
import sys
import winreg
import winreg as registry
from pathlib import Path

# https://stackoverflow.com/questions/38516044/setting-windows-system-path-in-registry-via-python-winreg

# HKEY_CURRENT_USER\
USR_ENV_SUBPATH = r"Environment"


def open_env_registry_key():
    return registry.OpenKey(
        registry.HKEY_CURRENT_USER, USR_ENV_SUBPATH, 0, registry.KEY_ALL_ACCESS
    )


def update_reg_path_value(path_to_add: str):
    env_key = open_env_registry_key()
    current_path: str
    current_path, _ = registry.QueryValueEx(env_key, "Path")

    for x in current_path.split(";"):
        if x == path_to_add:
            return

    new_path = f"{path_to_add};{current_path}"
    registry.SetValueEx(env_key, "Path", 0, registry.REG_SZ, new_path)

    refresh_path()


def refresh_path():
    env_key = open_env_registry_key()
    current_path: str
    current_path, _ = registry.QueryValueEx(env_key, "Path")
    for x in current_path.split(";"):
        if x not in sys.path:
            sys.path.append(x)


def runcmd(args: list[str]):
    ans = subprocess.run(args, check=True)
    refresh_path()
    return ans


def main():
    refresh_path()

    runcmd(["scoop.cmd", "install", "nodejs"])
    runcmd(["npm.cmd", "install", "pyright"])

    scripts_dir = Path(sys.executable).parent / "Scripts"
    scripts_dir = scripts_dir.resolve()
    update_reg_path_value(str(scripts_dir))

    runcmd(["python3", "-m", "pip", "install", "black"])
    runcmd(["python3", "-m", "pip", "install", "isort"])
    runcmd(["python3", "-m", "pip", "install", "ipython"])


if __name__ == "__main__":
    main()
