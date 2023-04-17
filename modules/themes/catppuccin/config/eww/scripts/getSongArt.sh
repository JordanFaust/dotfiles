#!/usr/bin/env bash

# TMP_DIR="$HOME/.cache/eww"
# TMP_COVER_PATH=$TMP_DIR/cover.png
# TMP_TEMP_PATH=$TMP_DIR/temp.png
#
# if [[ ! -d $TMP_DIR ]]; then
# 	mkdir -p "$TMP_DIR"
# fi
#
# ART_FROM_SPOTIFY="$(playerctl -p %any,spotify metadata mpris:artUrl | sed -e 's/open.spotify.com/i.scdn.co/g')"
# ART_FROM_BROWSER="$(playerctl -p %any,mpd,firefox,chromium,brave metadata mpris:artUrl | sed -e 's/file:\/\///g')"
#
# if [[ $(playerctl -p spotify,%any,firefox,chromium,brave,mpd metadata mpris:artUrl) ]]; then
# 	curl -s "$ART_FROM_SPOTIFY" --output "$TMP_TEMP_PATH"
# elif [[ -n $ART_FROM_BROWSER ]]; then
# 	cp "$ART_FROM_BROWSER" "$TMP_TEMP_PATH"
# else
# 	cp "$HOME/.config/eww/assets/ui/music-fallback.png" "$TMP_TEMP_PATH"
# fi
#
# cp "$TMP_TEMP_PATH" "$TMP_COVER_PATH"

# an epic effekt
# convert $TMP_TEMP_PATH -alpha set -channel A -evaluate multiply 1.0  $TMP_COVER_PATH
# convert $TMP_TEMP_PATH -gravity center +repage -alpha set -channel A \
# 	-sparse-color Barycentric '%[fx:w*2/32],0 transparent  %[fx:w+0.5],0 opaque' \
# 	-evaluate multiply 0.45 \
# 	$TMP_COVER_PATH

TMP_DIR="/tmp/eww/playerctl"
ART_URLS="${TMP_DIR}/art_urls"
PLAYERS="spotify,%any,firefox,chromium,brave,mpd"
ALBUM=$(playerctl -p $PLAYERS metadata --format '{{ album }}')
ART_FROM_SPOTIFY="$(playerctl -p %any,spotify metadata mpris:artUrl | sed -e 's/open.spotify.com/i.scdn.co/g')"

if [[ ! -d $TMP_DIR ]]; then
  mkdir -p $TMP_DIR
fi

if [[ ! -f $ART_URLS ]]; then
  touch $ART_URLS
fi

get_song_art() {
  local art_from_spotify
  local cover_path

  cover_path="${TMP_DIR}/${ALBUM}.png"

  if [[ -f "$cover_path" ]]; then
    echo "cover already exists"
    return
  fi

  if [[ $(playerctl -p spotify,%any,firefox,chromium,brave,mpd metadata mpris:artUrl) ]]; then
    curl -s "$ART_FROM_SPOTIFY" --output "$cover_path"
  else
    cp "$HOME/.config/eww/assets/ui/music-fallback.png" "$cover_path"
  fi
}

get_cached_art() {
  local cover_path
  local album
  local artist

  album=$(playerctl -p $PLAYERS metadata --format '{{ album }}')
  artist=$(playerctl -p $PLAYERS metadata --format '{{ artist }}')
  cover_path="${TMP_DIR}/${artist} - ${ALBUM}.png"

  echo "$cover_path"
}

daemon_start() {
  local song_art
  local album
  local artist
  local cover_path

  # Follow changes to the art url of the player. Process and download cover art that has not already been
  # downloaded.
  while read -r line
  do
    song_art=$line
    album=$(playerctl -p $PLAYERS metadata --format '{{ album }}')
    artist=$(playerctl -p $PLAYERS metadata --format '{{ artist }}')
    echo "processing album art, album: ${album}, artist: ${artist}"
    cover_path="${TMP_DIR}/${artist} - ${album}.png"

    if [[ -f "${cover_path}" ]]; then
      echo "cover art already exists: ${cover_path}"
      continue
    fi

    echo "downloading art to ${cover_path}"
    curl -s "${song_art}" --output "${cover_path}"
  done < <(playerctl -p spotify,%any,firefox,chromium,brave,mpd metadata mpris:artUrl -F)
}

case $1 in
	"song_art") get_song_art;;
	"cached_art") get_cached_art;;
  "daemon") daemon_start;;
esac
