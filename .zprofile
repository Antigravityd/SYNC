# Honor system-wide environment variables
source /etc/profile

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

[[ -t 0 && $(tty) == /dev/tty2 && $- =~ "l" ]]  && source ~/.zshrc && exec startx
