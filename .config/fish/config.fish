# disable wellcome message
set fish_greeting

# disable underline
set fish_color_valid_path

# custom clear
alias cls="clear"

# emacsclient
alias e="emacsclient"

# emacs no window
# alias nw="emacs -nw"

# custom ps
alias ps="ps xx"

# custom ls
alias l="ls -la"

# set PATH
set -gx PATH $PATH ~/.cask/bin ~/.emacs.d/rtags/build/bin

# clear PARH duplicates
varclear PATH

# EDITOR
set -gx EDITOR "emacsclient"

# set TERM
set -gx TERM "eterm-color"

# set TERMCAP
set -gx TERMCAP "eterm-color:li#39:co#113:cl=\E[H\E[J:cd=\E[J:bs:am:xn:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:ho=\E[H:pt:al=\E[L:dl=\E[M:DL=\E[%dM:AL=\E[%dL:cs=\E[%i%d;%dr:sf=^J:dc=\E[P:DC=\E[%dP:IC=\E[%d@:im=\E[4h:ei=\E[4l:mi::so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:UP=\E[%dA:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:kl=\EOD:kd=\EOB:kr=\EOC:ku=\EOA:kN=\E[6~:kP=\E[5~:@7=\E[4~:kh=\E[1~:mk=\E[8m:cb=\E[1K:op=\E[39;49m:Co#8:pa#64:AB=\E[4%dm:AF=\E[3%dm:cr=^M:bl=^G:do=^J:le=^H:ta=^I:se=\E[27m:ue=\E[24m:kb=^?:kD=^[[3~:sc=\E7:rc=\E8:r1=\Ec:"

# TERMINFO
set -gx TERMINFO "/usr/local/share/emacs/26.3/etc/"
