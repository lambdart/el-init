# Disable access control for the current user.
xhost +SI:localuser:$USER

# Xresources
# xrdb ~/.Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# Make Caps Lock a Control key
# setxkbmap -option ctrl:nocaps

# Swap Left Control and Caps
setxkbmap -option ctrl:swapcaps

# Set default cursor.
xsetroot -cursor_name left_ptr

# Set keyboard repeat rate.
xset r rate 200 60

# Start Compton compositor in background
compton -b

# Uncomment the following block to use the exwm-xim module.
export XMODIFIERS=@im=exwm-xim
export GTK_IM_MODULE=xim
# export QT_IM_MODULE=xim
# export CLUTTER_IM_MODULE=xim

# Set Editor related variables
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Set Emacs Load Path
export EMACSLOADPATH=$EMACSLOADPATH:~/.emacs.d/site-lisp:~/.emacs.d/lisp

# Launch Emacs
exec emacs -f exwm-enable --debug-init

# Other forms
# emacs --daemon
# exec emacs --daemon -f exwm-enable
# exec dbus-launch --exit-with-session emacs --debug-init
# exec dbus-launch --exit-with-session emacs -f exwm-enable --debug-init
# exec dbus-launch --exit-with-session emacsclient -f exwm-enable --debug-init
