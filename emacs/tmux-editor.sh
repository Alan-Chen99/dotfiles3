#!/bin/sh
# $EDITOR inside tmux sessions that `alan-vterm-tmux' shows in Emacs.
#
# A session outlives any one Emacs, so the server is looked up on every call
# instead of being fixed in the environment of long-running panes: each attach
# from `alan-vterm-tmux' copies EMACS_SOCKET_NAME and ALAN_EMACSCLIENT into the
# session environment (tmux update-environment).
set -eu

: "${TMUX:?not inside tmux}"
# Session scope only: the global copies belong to whichever Emacs started the
# server.  A client attaching without these variables marks them unset.
v=$(tmux show-environment -s EMACS_SOCKET_NAME)
eval "$v"
v=$(tmux show-environment -s ALAN_EMACSCLIENT)
eval "$v"
: "${EMACS_SOCKET_NAME:?the last client to attach this tmux session was not Emacs}"
: "${ALAN_EMACSCLIENT:?the last client to attach this tmux session was not Emacs}"

# with-editor's sleeping-editor fallback does not survive emacsclient splitting
# it on spaces, so a dead server would end in a shell syntax error rather than
# emacsclient's own message.
unset ALTERNATE_EDITOR

esc() { printf '%s' "$1" | sed 's/[\\"]/\\&/g'; }

# Emacs opens marked files in with-editor-mode, as it does git commit
# messages, so finish and cancel work the same for every program.
for f in "$@"; do
	case $f in
	[-+]*) continue ;;
	esac
	"$ALAN_EMACSCLIENT" --eval "(alan-vterm-tmux-editor-expect \"$(esc "$f")\" \"$(esc "$PWD")\")" >/dev/null
done
exec "$ALAN_EMACSCLIENT" "$@"
