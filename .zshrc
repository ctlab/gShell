test -f "$OLD_ZDOTDIR/.zshenv" && . "$OLD_ZDOTDIR/.zshenv"
test -f "$OLD_ZDOTDIR/.zshrc"  && . "$OLD_ZDOTDIR/.zshrc"
test -f "$ZDOTDIR/gshell.zsh"  && . "$ZDOTDIR/gshell.zsh"
test -f "~/.zshenv" && source ~/.zshenv
source ~/.zshrc

PROMPT="GSHELL ][ $PROMPT"

trap 'fusermount -uz $(pwd)' EXIT

cd "$TOCD"
