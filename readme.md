# dotfiles

This is my highly opinionated emacs configuration (and other configuration files).

This emacs configuration is optimized for speed and stability. Windows and Linux is supported. Both terminal and GUI should work.

This configuration uses Evil, so it has insert, normal, etc states like VIM as well as most common VIM bindings.

I currently use emacs 29. Other versions are not tested and may not work. A build with `treesit` is required.

## how to use this emacs configuration

This section is incomplete. If anyone actually wants to try this, please send an issue and I will probably add more documentation.

### install

To start, make symlinks

```
emacs/.emacs -> ~/.emacs
emacs/early-init.el ~/.emacs.d/early-init.el (optional)
```

remove `emacs/alan-base-bindings.el` and replace with `emacs/alan-base-bindings.example.el`
copy `emacs/alan-private.example.el` to `emacs/alan-private.el`

Start emacs; dependencies will be installed into `~/.emacs.d`.
Some dependencies will fail due to missing system dependencies. Just ignore them for now.
On windows, you may have to start emacs multiple times for most packages to build.

### get started

`;` will be your prefix key. it will also be called `<.>`.

start by pressing `s (execute-extended-command)` (or `M-x` if you are in insert state). This will be how you execute most of your commands.
Here you use arrow keys to move up and down: `<up> (vertico-next)`, `<down> (vertico-previous)`,
`TAB (vertico-insert)` to complete and `<return> (vertico-exit)` to accept, `<escape> (abort-minibuffers)` to cancel.

This lets you search all commands. It also lets you know the binding.
Some important commands to start:
`find-file (w w)`: open a file
`switch-to-buffer (w b)`: switch to a buffer
`consult-theme`: change theme
`describe-function (N f)`: show documentation of a command or function, and let you jump to its source code
`q` or `save-buffers-kill-terminal`: quit emacs
`evil-window-left (w h)`: go the left window
`evil-window-right (w l)`: go the right window
`push-button (RET)`: push a button, a highlighted and underlined text
`evil-goto-definition (; j)`: go to the definition of a function or variable. Works in most places.
`format-all-region-or-buffer (; f)`: format the current buffer
`shell (z s)`: open a shell
`debug (N 0)`: enter the debugger and get a backtrace
`magit-**`: git user interface

### suggestions / tips

I highly recommend rebinding harder to reach keys like `<escape>`, `<left>`, `<DEL>`, `(`, `!` to something else.
You can modify `emacs/alan-base-bindings.el` to do this.

I recommend making your own bindings rather than learn someone elses. Modify `emacs/alan-bindings.el`.

The logs are located in the `*span*` buffer. This is using a package `span.el` currently here but I might publish latter.
I recommend byte compiling `emacs/span.el` and `emacs/span-fmt.el` by visiting them and calling `emacs-lisp-byte-compile-and-load`. Doing so will increase performance and give you cleaner stack traces.

Most of the time, use `<escape>` to quit. If that doesnt work, try `w t (top-level)` (in normal state). If something is slow or hanging, use `C-g` or `C-g C-g C-g` (note that `C-g` is hardcoded in emacs and not possible to change).

If the elisp debugger pops up, you usually press `c` to ignore it and continue.

## Notes to self

### start on new machine

```
nix flake show --option allow-import-from-derivation true

nix build .#profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#profile --print-build-logs --option allow-import-from-derivation true

nix build .#less-build.profile --print-build-logs --option allow-import-from-derivation true
nix profile install .#less-build.profile --print-build-logs --option allow-import-from-derivation true

# add to .profile:
if [ -f "${HOME}/.nix-profile/env/.env" ]; then
	. "${HOME}/.nix-profile/env/.env"
fi

nix profile install .#emacs
nix profile install .#less-build.emacs

cd ~
ln -s dotfiles/emacs/.emacs ~/.emacs
ln -s ../dotfiles/emacs/early-init.el ~/.emacs.d/early-init.el

emacs/span.el -> emacs-lisp-byte-compile-and-load
emacs/span-fmt.el -> emacs-lisp-byte-compile-and-load
```
