# TODO:
# https://programming-review.com/github/using-github-via-ssh/#adding-ssh-keys-to-the-ssh-agent
# Get-Service -Name ssh-agent | Set-Service -StartupType Manual
# Start-Service ssh-agent

import ctypes
import os
import shutil
import subprocess
import sys
import urllib.request
import winreg
import winreg as winreg
from pathlib import Path
from typing import Iterable, Sequence

# https://github.com/ryanoasis/nerd-fonts?tab=readme-ov-file#option-3-unofficial-chocolatey-or-scoop-repositories
# scoop bucket add nerd-fonts
# scoop install Hack-NF
# select and install on .ttf files


# https://stackoverflow.com/questions/38516044/setting-windows-system-path-in-registry-via-python-winreg

# HKEY_CURRENT_USER\
USR_ENV_SUBPATH = r"Environment"

HOME = Path(os.environ["USERPROFILE"])
repodir = Path(__file__).parent.parent


def escape_powershell_string(s):
    # Wrap the string with escaped single quotes
    # Replace single quotes with double single quotes (PowerShell escaping)
    s = s.replace("'", "''")
    return f"'{s}'"


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
    path_list = os.environ["PATH"].split(";")
    for x in current_path.split(";"):
        x = os.path.expandvars(x)
        if x not in path_list:
            print(f"adding {x} to PATH", flush=True)
            path_list.append(f";{x}")
    os.environ["PATH"] = ";".join(path_list)


type argspec = str | Path | Sequence[argspec]


def runcmd(*args_: argspec):
    def gen(x: argspec) -> Iterable[str]:
        if isinstance(x, str):
            yield x
        elif isinstance(x, Path):
            yield str(x)
        else:
            for y in x:
                yield from gen(y)

    args = list(gen(args_))

    print("++ runcmd:", " ".join(map(escape_powershell_string, args)), flush=True)
    ans = subprocess.run(args, check=True)
    refresh_path()
    return ans


def run_powershell(args: list[str]):
    cmd = " ".join(map(escape_powershell_string, args))
    print("++/ powershell:", cmd, flush=True)
    return runcmd("powershell", "-Command", cmd)


def symlink(source: Path, target: Path):
    if target.is_symlink():
        runcmd("ln", "-sf", source, target)
    else:
        runcmd("ln", "-s", source, target)


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

    runcmd("scoop.cmd", "bucket", "add", "extras")

    runcmd("scoop.cmd", "install", "nodejs")
    if shutil.which("pyright") is None:
        runcmd("npm.cmd", "install", "-g", "pyright")

    scripts_dir = Path(sys.executable).parent / "Scripts"
    scripts_dir = scripts_dir.resolve()
    update_reg_path_value(str(scripts_dir))

    runcmd("python3", "-m", "pip", "install", "black")
    runcmd("python3", "-m", "pip", "install", "isort")
    runcmd("python3", "-m", "pip", "install", "ipython")

    runcmd("scoop.cmd", "install", "ripgrep")
    runcmd("scoop.cmd", "install", "fd")

    # REQUIRES MANUAL INSTALL AFTER THIS
    runcmd("scoop.cmd", "bucket", "add", "nerd-fonts")
    runcmd("scoop.cmd", "install", "Hack-NF")

    # TODO: set HOME env var

    symlink(repodir / "windows" / ".gitconfig", HOME / ".gitconfig")
    (HOME / ".emacs.d").mkdir(exist_ok=True)
    symlink(repodir / "emacs" / ".emacs", HOME / ".emacs.d" / "init.el")
    symlink(repodir / "emacs" / "early-init.el", HOME / ".emacs.d" / "early-init.el")

    runcmd("scoop.cmd", "install", "rustup")
    # TODO:
    # To get started you need Cargo's bin directory (C:\Users\chenx\scoop\persist
    # \rustup\.cargo\bin) in your PATH
    # environment variable. This has not been done automatically.
    # done.

    # # if shutil.which("link") is None:
    # #     runcmd(["scoop.cmd", "shim", "rm", "link"])

    # neccessary to build rust
    # https://github.com/ScoopInstaller/Extras/issues/1861#issuecomment-1320653215
    # https://learn.microsoft.com/en-us/visualstudio/install/workload-component-id-vs-build-tools?view=vs-2022
    temp_dir = os.environ["TEMP"]
    tmp_buildtools_exe = Path(temp_dir) / "vs_BuildTools.exe"
    urllib.request.urlretrieve(
        "https://aka.ms/vs/17/release/vs_BuildTools.exe", tmp_buildtools_exe
    )
    runcmd(
        tmp_buildtools_exe,
        "--passive",
        "--wait",
        ["--add", "Microsoft.VisualStudio.Workload.VCTools"],
        "--includeRecommended",
        ["--remove", "Microsoft.VisualStudio.Component.VC.CMake.Project"],
    )

    # https://answers.microsoft.com/en-us/windows/forum/all/how-to-disable-search-the-web-completley-in/ea22410a-3031-487f-b5de-5a0113d656c5
    create_key(r"Software\Policies\Microsoft\Windows", "Explorer")
    set_dword_value(
        r"Software\Policies\Microsoft\Windows\Explorer",
        "DisableSearchBoxSuggestions",
        1,
    )

    # requires restart
    # https://answers.microsoft.com/en-us/windows/forum/all/windows-11-right-click-explorer-menu-show-more-as/ba8dafe4-306a-403b-af0d-10a6d1ca0a9a
    with winreg.CreateKey(
        winreg.HKEY_CURRENT_USER,
        r"Software\Classes\CLSID\{86ca1aa0-34aa-4e8b-a509-50c905bae2a2}\InprocServer32",
    ) as registry_key:
        winreg.SetValueEx(registry_key, "", 0, winreg.REG_SZ, "")

    # TODO: `Microsoft Defender Disable.bat`
    # https://github.com/TairikuOokami/Windows.git


if __name__ == "__main__":
    main()
