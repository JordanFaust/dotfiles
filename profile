export PATH="~/bin:$PATH"

# starts yabai
brew services restart skhd
brew services restart yabail

# Startup scripts
wal -R -q -n
. "$HOME/.cargo/env"
