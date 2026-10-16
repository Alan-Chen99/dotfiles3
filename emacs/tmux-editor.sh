#!/bin/sh
# $EDITOR inside tmux sessions that `alan-vterm-tmux' shows in Emacs.
#
# A session outlives any one Emacs, so the editor is looked up on every call
# instead of being fixed in the environment of long-running panes: each attach
# from `alan-vterm-tmux' copies its variables into the session environment
# (tmux update-environment).  An Emacs on this host sets EMACS_SOCKET_NAME and
# ALAN_EMACSCLIENT; an Emacs on another host, reaching this one through Tramp,
# cannot be reached by emacsclient and sets ALAN_REMOTE_EDITOR instead, the
# path of remote-editor.sh here.  Either way a file opens as it does for
# emacsclient outside tmux; alan-with-editor.el says how that ends.
set -eu

: "${TMUX:?not inside tmux}"
# Session scope only: the global copies belong to whichever Emacs started the
# server.  A client attaching without these variables marks them unset.  A
# session not attached since a variable was added to update-environment has
# no entry for it, and "show-environment -s NAME" fails on that, so the whole
# session environment is read instead.
unset EMACS_SOCKET_NAME ALAN_EMACSCLIENT ALAN_REMOTE_EDITOR
v=$(tmux show-environment -s)
# grep exits 1 when the session has none of them.
m=$(printf '%s\n' "$v" | grep -E '^(unset )?(EMACS_SOCKET_NAME|ALAN_EMACSCLIENT|ALAN_REMOTE_EDITOR)[=;]') || [ $? -eq 1 ]
eval "$m"

if [ -n "${EMACS_SOCKET_NAME:-}" ] && [ -n "${ALAN_EMACSCLIENT:-}" ]; then
	# with-editor's sleeping-editor fallback does not survive emacsclient
	# splitting it on spaces, so a dead server would end in a shell syntax
	# error rather than emacsclient's own message.
	unset ALTERNATE_EDITOR
	export EMACS_SOCKET_NAME
	exec "$ALAN_EMACSCLIENT" "$@"
fi
if [ -n "${ALAN_REMOTE_EDITOR:-}" ]; then
	exec "$ALAN_REMOTE_EDITOR" "$@"
fi
echo "tmux-editor.sh: the last client to attach this tmux session was not Emacs" >&2
exit 1
