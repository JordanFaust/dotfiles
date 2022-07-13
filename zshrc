#!/usr/bin/env zsh

INSIDE_EMACS=${INSIDE_EMACS:-false}
export TERM=screen-256color

#######################
###### TERMINAL #######
#######################
# If not starting a shell in emacs and not currently in TMUX, create or attach to a TMUX session
if [[ "$INSIDE_EMACS" = "false" && -z "$TMUX" ]]; then
    # tmux -2 attach -t TMUX || tmuxinator start TMUX -n TMUX -p ~/.dotfiles/config/tmuxinator/session.yaml
    tmux
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
  # zsh-syntax-highlighting # Fish shell like syntax highlighting for Zsh
  # zsh-history-substring-search
  colored-man-pages # Self-explanatory
  )
autoload -U compinit && compinit # reload completions for zsh-completions

#######################
######## THEME ########
#######################

ZSH_THEME="spaceship"
SPACESHIP_KUBECTL_SHOW=true
SPACESHIP_KUBECTL_VERSION_SHOW=false
SPACESHIP_PROMPT_ORDER=(
  time          # Time stamps section
  user          # Username section
  dir           # Current directory section
  host          # Hostname section
  git           # Git section (git_branch + git_status)
  hg            # Mercurial section (hg_branch  + hg_status)
  package       # Package version
  node          # Node.js section
  ruby          # Ruby section
  elixir        # Elixir section
  xcode         # Xcode section
  swift         # Swift section
  golang        # Go section
  php           # PHP section
  # rust          # Rust section
  haskell       # Haskell Stack section
  julia         # Julia section
  docker        # Docker section
  aws           # Amazon Web Services section
  gcloud        # Google Cloud Platform section
  venv          # virtualenv section
  conda         # conda virtualenv section
  pyenv         # Pyenv section
  dotnet        # .NET section
  ember         # Ember.js section
  kubectl       # Kubectl context section
  terraform     # Terraform workspace section
  exec_time     # Execution time
  line_sep      # Line break
  battery       # Battery level and status
  vi_mode       # Vi-mode indicator
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  char          # Prompt character
)
SPACESHIP_VI_MODE_SHOW=true
SPACESHIP_VI_MODE_PREFIX=""
SPACESHIP_VI_MODE_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"
SPACESHIP_VI_MODE_INSERT="[I]"
SPACESHIP_VI_MODE_NORMAL="[N]"
SPACESHIP_VI_MODE_COLOR="green"

########################
####### ZSH INIT #######
########################

# Disable default <<< Normal mode indicator in right prompt
export RPS1="%{$reset_color%}"

export ZSH="$HOME/.oh-my-zsh"
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

export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/.rbenv/bin:${PATH}"
eval "$(rbenv init -)"

# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

########################
######## VTERM #########
########################

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = "vterm" ]]; then
    autoload -U promptinit; promptinit
    prompt spaceship
    echo "autoloading spaceshipt"
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

########################
####### FZF ZSH ########
########################

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

(cat ~/.cache/wal/sequences &)
