setxkbmap us -option compose:menu
xmodmap ~/.Xmodmap
# emacs --daemon
/home/dnw/.fehbg &
bash /home/dnw/status.sh &
pulseaudio --start
fcitx5 &
exec dbus-run-session -- dwm
