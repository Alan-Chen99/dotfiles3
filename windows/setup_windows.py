# TODO:
# https://programming-review.com/github/using-github-via-ssh/#adding-ssh-keys-to-the-ssh-agent
# Get-Service -Name ssh-agent | Set-Service -StartupType Manual
# Start-Service ssh-agent

# https://github.com/ryanoasis/nerd-fonts?tab=readme-ov-file#option-3-unofficial-chocolatey-or-scoop-repositories
# scoop bucket add nerd-fonts
# scoop install Hack-NF
# select and install on .ttf files

import ctypes
import os
import subprocess
import sys
import winreg
import winreg as winreg
from pathlib import Path

# https://stackoverflow.com/questions/38516044/setting-windows-system-path-in-registry-via-python-winreg

# HKEY_CURRENT_USER\
USR_ENV_SUBPATH = r"Environment"


def open_env_registry_key():
    return winreg.OpenKey(
        winreg.HKEY_CURRENT_USER, USR_ENV_SUBPATH, 0, winreg.KEY_ALL_ACCESS
    )


def update_reg_path_value(path_to_add: str):
    env_key = open_env_registry_key()
    current_path: str
    current_path, _ = winreg.QueryValueEx(env_key, "Path")

    for x in current_path.split(";"):
        if x == path_to_add:
            return

    new_path = f"{path_to_add};{current_path}"
    winreg.SetValueEx(env_key, "Path", 0, winreg.REG_SZ, new_path)

    refresh_path()


def refresh_path():
    env_key = open_env_registry_key()
    current_path: str
    current_path, _ = winreg.QueryValueEx(env_key, "Path")
    for x in current_path.split(";"):
        if x not in sys.path:
            sys.path.append(x)


def runcmd(args: list[str]):
    ans = subprocess.run(args, check=True)
    refresh_path()
    return ans


def create_key(path: str, subkey: str):
    """Create a registry key if it does not exist."""
    with winreg.OpenKey(winreg.HKEY_CURRENT_USER, path, 0, winreg.KEY_WRITE) as key:
        try:
            winreg.CreateKey(key, subkey)
        except FileExistsError:
            pass


def set_dword_value(path: str, name: str, value: int | str):
    """Set the DWORD value for a registry key."""
    with winreg.OpenKey(winreg.HKEY_CURRENT_USER, path, 0, winreg.KEY_WRITE) as key:
        winreg.SetValueEx(key, name, 0, winreg.REG_DWORD, value)


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

    # TODO: elevate and test
    # https://answers.microsoft.com/en-us/windows/forum/all/how-to-disable-search-the-web-completley-in/ea22410a-3031-487f-b5de-5a0113d656c5
    # create_key(r"Software\Policies\Microsoft\Windows", "Explorer")
    # set_dword_value(
    #     r"Software\Policies\Microsoft\Windows\Explorer",
    #     "DisableSearchBoxSuggestions",
    #     1,
    # )

    # https://answers.microsoft.com/en-us/windows/forum/all/windows-11-right-click-explorer-menu-show-more-as/ba8dafe4-306a-403b-af0d-10a6d1ca0a9a
    with winreg.CreateKey(
        winreg.HKEY_CURRENT_USER,
        r"Software\Classes\CLSID\{86ca1aa0-34aa-4e8b-a509-50c905bae2a2}\InprocServer32",
    ) as registry_key:
        winreg.SetValueEx(registry_key, "", 0, winreg.REG_SZ, "")


if __name__ == "__main__":
    main()
