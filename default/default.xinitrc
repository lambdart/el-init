# Xresources
# [[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
xrdb ~/.Xresources

# set keyboard
setxkbmap -option ctrl:swapcaps  # Swap Left Control and Caps Lock
setxkbmap -option ctrl:nocaps    # Make Caps Lock a Control key

# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"
export EMACSLOADPATH=$EMACSLOADPATH:~/.emacs.d/site-lisp:~/.emacs.d/lisp

# finally launch emacs (breaks dashboards)
exec dbus-launch --exit-with-session emacs -f exwm-enable --debug-init
