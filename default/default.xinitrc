# Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# xhost +SI:localuser:$USER

# swap left control and caps
setxkbmap -option ctrl:swapcaps

# disable caps
setxkbmap -option '' -option 'ctrl:nocaps'

# set default cursor.
xsetroot -cursor_name left_ptr

# set keyboard repeat rate
xset r rate 200 60

# start Compton compositor in background
compton -b

# uncomment the following block to use the exwm-xim module.
# export XMODIFIERS=@im=exwm-xim
# export GTK_IM_MODULE=xim
# export QT_IM_MODULE=xim
# export CLUTTER_IM_MODULE=xim

# set Editor related variables
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# set Emacs Load Path
export EMACSLOADPATH=$EMACSLOADPATH:~/.emacs.d/site-lisp:~/.emacs.d/lisp

# Emacs X Resources
# emacs.minibuffer:none
# emacs.menuBar:off
# emacs.selectionFont:""

# create emacs daemon
# emacs --daemon --debug-init

# execute the client
# exec emacsclient -c

# launch emacs
exec emacs --debug-init
