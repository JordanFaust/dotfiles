#!/usr/bin/env zsh

INSIDE_EMACS=${INSIDE_EMACS:-false}
export TERM=screen-256color

#######################
###### TERMINAL #######
#######################
# If not starting a shell in emacs and not currently in TMUX, create or attach to a TMUX session
if [[ "$INSIDE_EMACS" = "false" && -z "$TMUX" ]]; then
    tmux -2 attach -t TMUX || tmuxinator start TMUX -n TMUX -p ~/.dotfiles/config/tmuxinator/session.yaml
fi

#######################
####### PLUGINS #######
#######################

plugins=(
  aws
  docker
  git # https://github.com/robbyrussell/oh-my-zsh/wiki/Plugin:git
  golang
  history-substring-search # ZSH port of Fish history search. Begin typing command, use up arrow to select previous use
  kubectl
  minikube
  tmux
  tmuxinator
  # zsh-autosuggestions # Suggests commands based on your history
  # zsh-completions # More completions
  zsh-syntax-highlighting # Fish shell like syntax highlighting for Zsh
  zsh-history-substring-search
  colored-man-pages # Self-explanatory
  )
autoload -U compinit && compinit # reload completions for zsh-completions

########################
####### ZSH INIT #######
########################

# Disable default <<< Normal mode indicator in right prompt
export RPS1="%{$reset_color%}"

source $ZSH/oh-my-zsh.sh # required

if [ -x "$(command -v rbenv)" ]; then
    if [[ ":${PATH}:" != *"${HOME}/.rbenv/bin:${PATH}" ]]; then
        export PATH="${HOME}/.rbenv/bin:${PATH}"
    fi
    eval "$(rbenv init -)"
fi

### SSH
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh/cache
fi
if [[ ! "$SSH_AUTH_SOCK" ]]; then
    eval "$(<~/.ssh/cache)"
fi

export ZDOTDIR=$HOME/.dotfiles/zdotdir
# Load external files
if [[ -d "${ZDOTDIR:-$HOME}"/zsh.d ]]; then
    for ZSH_FILE in $(ls -A "${ZDOTDIR:-$HOME}"/zsh.d/*.zsh); do
        source "${ZSH_FILE}"
    done
fi

########################
####### ZSH VIM #######
########################

# Vim Keybindings for Shell
bindkey -v
bindkey 'jk' vi-cmd-mode
bindkey 'kj' vi-cmd-mode

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
      [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'

  elif [[ ${KEYMAP} == main ]] ||
        [[ ${KEYMAP} == viins ]] ||
        [[ ${KEYMAP} = '' ]] ||
        [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select

# Use beam shape cursor on startup.
echo -ne '\e[5 q'

# Use beam shape cursor for each new prompt.
preexec() {
    echo -ne '\e[5 q'
}

# Allows for changing in quotes
autoload -U select-quoted
zle -N select-quoted
for m in visual viopp; do
  for c in {a,i}{\',\",\`}; do
    bindkey -M $m $c select-quoted
  done
done

# Allows for changing in brackets
autoload -U select-bracketed
zle -N select-bracketed
for m in visual viopp; do
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $m $c select-bracketed
  done
done

########################
####### FZF ZSH ########
########################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

(cat ~/.cache/wal/sequences &)
