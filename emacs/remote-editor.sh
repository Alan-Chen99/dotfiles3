#!/bin/sh
# $EDITOR for shells that Emacs starts on a remote host, installed there by
# `alan-with-editor-install': directly for shell and vterm buffers, and
# through tmux-editor.sh for `alan-vterm-tmux'.
#
# A remote host cannot reach Emacs's server socket, so the request goes out
# on the terminal as an OSC 51;E sequence, which vterm and comint hand to
# `alan-with-editor-open'.  That visits the file as emacsclient would, and
# killing its buffer, or finishing or cancelling a commit message, runs
# `kill -s USR1' or `kill -s USR2' on this process through Tramp.
set -eu

# Hex keeps the directory and file names clear of OSC terminators and of the
# quoting that `vterm--eval' undoes.
p=$(printf '%s\0' "$PWD" "$@" | od -An -v -tx1 | tr -d ' \n')
m="51;Ealan-with-editor-open $$ $p"

# /dev/tty, not stdout: the caller may have redirected stdout.
if [ -n "${TMUX:-}" ]; then
	# tmux drops OSC 51 unless it is wrapped in passthrough, as in vterm_printf.
	printf '\033Ptmux;\033\033]%s\007\033\\' "$m" >/dev/tty
else
	printf '\033]%s\033\\' "$m" >/dev/tty
fi

sleep 604800 &
s=$!
trap 'kill "$s"; exit 0' USR1
trap 'kill "$s"; exit 1' USR2
# A background job of a non-interactive shell ignores C-c, so the sleep
# would outlive this script by up to 7 days.
trap 'kill "$s"; exit 129' HUP
trap 'kill "$s"; exit 130' INT
trap 'kill "$s"; exit 143' TERM
wait "$s"
echo "remote-editor.sh: no answer from Emacs in 7 days" >&2
exit 1
