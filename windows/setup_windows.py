# TODO:
# https://programming-review.com/github/using-github-via-ssh/#adding-ssh-keys-to-the-ssh-agent
# Get-Service -Name ssh-agent | Set-Service -StartupType Manual
# Start-Service ssh-agent

# TODO: `Microsoft Defender Disable.bat`
# https://github.com/TairikuOokami/Windows.git

# TODO
# https://github.com/dechamps/laplock

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

# https://stackoverflow.com/questions/38516044/setting-windows-system-path-in-registry-via-python-winreg

USR_ENV_SUBPATH = r"Environment"

HOME = Path(os.environ["USERPROFILE"])
repodir = Path(__file__).parent.parent

unused = object()


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

    #################### ELEVATED ####################

    # disable "smartscreen", i.e. slience "windows protected your pc"
    # untested; was disabled manually
    with winreg.CreateKey(
        winreg.HKEY_LOCAL_MACHINE,
        r"SOFTWARE\Policies\Microsoft\Windows\System",
    ) as registry_key:
        winreg.SetValueEx(registry_key, "EnableSmartScreen", 0, winreg.REG_DWORD, 0)

    # https://superuser.com/questions/1587025/windows-10-how-to-disable-the-publisher-couldn-t-be-verified-message
    # also need to change Internet Options
    # TODO: do these reg actually do anything?
    with winreg.CreateKey(
        winreg.HKEY_CURRENT_USER,
        r"Software\Microsoft\Windows\CurrentVersion\Policies\Attachments",
    ) as registry_key:
        winreg.SetValueEx(registry_key, "SaveZoneInformation", 0, winreg.REG_DWORD, 1)
    with winreg.CreateKey(
        winreg.HKEY_CURRENT_USER,
        r"Software\Microsoft\Windows\CurrentVersion\Policies\Associations",
    ) as registry_key:
        winreg.SetValueEx(
            registry_key,
            "LowRiskFileTypes",
            0,
            winreg.REG_MULTI_SZ,
            [
                ".avi",
                ".bat",
                ".bmp",
                ".cmd",
                ".com",
                ".exe",
                ".gif",
                ".htm",
                ".html",
                ".jpg",
                ".m3u",
                ".mov",
                ".mp3",
                ".mpeg",
                ".mpg",
                ".msi",
                ".nfo",
                ".rar",
                ".reg",
                ".txt",
                ".wav",
                ".zip",
            ],
        )

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

    # 5/26/2025
    # idk if this actually helps
    # https://superuser.com/questions/1755386/how-to-fix-windows-slow-response-to-bluetooth-mouse-and-keyboard
    # https://helpdeskgeek.com/how-to-fix-bluetooth-mouse-lag-in-windows-11/
    with winreg.OpenKey(
        winreg.HKEY_LOCAL_MACHINE,
        r"SYSTEM\CurrentControlSet\Enum\USB\VID_8087&PID_0033\5&578cb43&0&14\Device Parameters",
        access=winreg.KEY_ALL_ACCESS,
    ) as registry_key:
        for name in [
            "DeviceSelectiveSuspended",
            "SelectiveSuspendEnabled",
            "SelectiveSuspendSupported",
        ]:
            winreg.SetValueEx(registry_key, name, unused, winreg.REG_DWORD, 0)


def testfn():
    # 2025/11/19
    # disable efficiency mode
    # https://learn.microsoft.com/en-us/answers/questions/3860984/turn-off-efficiency-mode-windows-11-forever
    with winreg.OpenKey(
        winreg.HKEY_LOCAL_MACHINE,
        r"SYSTEM\CurrentControlSet\Control\Power",
        access=winreg.KEY_ALL_ACCESS,
    ) as registry_key:
        winreg.SetValueEx(
            registry_key, "PowerThrottlingOff", unused, winreg.REG_DWORD, 1
        )


if __name__ == "__main__":
    # main()
    testfn()
