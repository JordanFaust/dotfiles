#!${pkgs.stdenv.shell}

echo "Reloading polybar"
pkill -u $UID -x polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar main -c $XDG_CONFIG_HOME/polybar-old/config.ini >$XDG_DATA_HOME/polybar.log 2>&1 &
polybar bar -c $XDG_CONFIG_HOME/polybar/config.ini >$XDG_DATA_HOME/polybar.log 2>&1 &
echo "Reloading polybar complete"
