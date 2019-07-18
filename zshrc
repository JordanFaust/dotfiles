#!/usr/bin/env zsh

INSIDE_EMACS=${INSIDE_EMACS:-false}

#######################
###### TERMINAL #######
#######################
# If not starting a shell in emacs and not currently in TMUX, create or attach to a TMUX session
if [[ "$INSIDE_EMACS" = "false" && -z "$TMUX" ]]; then
    tmux attach -t TMUX || tmuxinator start TMUX -n TMUX -p ~/.dotfiles/config/tmuxinator/session.yaml
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

source $ZSH/oh-my-zsh.sh # required

if [ -x "$(command -v rbenv)" ]; then
    if [[ ":${PATH}:" != *"${HOME}/.rbenv/bin:${PATH}" ]]; then
        export PATH="${HOME}/.rbenv/bin:${PATH}"
    fi
    eval "$(rbenv init -)"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
