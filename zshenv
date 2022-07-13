#!/usr/bin/env zsh

#######################
####### EXPORTS #######
#######################

# Make Emacs the default editor
export EDITOR='emacsclient'
export BUNDLER_EDITOR=$EDITOR
export GIT_EDITOR=$EDITOR
export GIT_TERMINAL_PROMPT=1

# Go
export GOPATH="${HOME}/ws/go"
if [[ ":${PATH}:" != *"/usr/local/go/bin:${GOPATH}/bin:${PATH}" ]]; then
    export PATH="/usr/local/go/bin:${GOPATH}/bin:${PATH}"
fi


# Python
if [ -x "$(command -v python)" ]; then
    USER_BASE_PATH=$(python -m site --user-base)
    if [[ ":${PATH}:" != *"${PATH}:${USER_BASE_PATH}/bin" ]]; then
        export PATH="${PATH}:${USER_BASE_PATH}/bin"
    fi
fi


# Ruby
if [[ ":${PATH}:" != *"${HOME}/.rbenv/bin:${PATH}" ]]; then
    export PATH="${HOME}/.rbenv/bin:${PATH}"
fi

export GEM_HOME=$HOME/.gem
if [[ ":${PATH}:" != *"${GEM_HOME}/bin:${PATH}" ]]; then
    export PATH="${GEM_HOME}/bin:${PATH}"
fi

# Rust
if [[ ":${PATH}:" != *"${HOME}/.cargo/bin:${PATH}" ]]; then
    export PATH="${HOME}/.cargo/bin:${PATH}"
fi

if [[ ":${PATH}:" != *"${PATH}:${HOME}/.utility" ]]; then
    export PATH="${PATH}:${HOME}/.utility"
fi

# awesome-client "local h = require('helpers'); h.debug('stuff')"

# export kubebuilder
export PATH=$PATH:/usr/local/kubebuilder/bin
# krew
export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

# export istio
# export PATH=$PATH:/usr/local/istio-1.6/bin
export PATH=$PATH:/usr/local/istio-1.9.3/bin

export GOCACHE=$GOPATH/cache

export GITHUB_USER="jfaust"

# Ignore duplicate commands in the history
export HISTCONTROL=ignoredups

# Increase the maximum number of lines contained in the history file
# (default is 500)
export HISTFILESIZE=10000

# Increase the maximum number of commands to remember
# (default is 500)
export HISTSIZE=10000

# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"

# Make new shells get the history lines from all previous
# shells instead of the default "last window closed" history
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#######################
####### ALIASES #######
#######################

# Kubernetes Aliases
alias k=kubectl

# Alias kind kubectl interactions to explicitly isolate local testing
alias kindk="kubectl --kubeconfig ~/.kube/kind "
# complete -F __start_kubectl kindk

for opener in browser-exec xdg-open cmd.exe cygstart "start" open; do
    if command -v $opener >/dev/null 2>&1; then
        if [[ "$opener" == "cmd.exe" ]]; then
            # shellcheck disable=SC2139
            alias open="$opener /c start";
        else
            # shellcheck disable=SC2139
            alias open="$opener";
        fi
        break;
    fi
done

# TMUX 256 Colors
alias tmux=tmux -2

# Code Search
alias search="grep -iRl "

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"

# Shortcuts
alias dl="cd ~/Downloads"
alias g="git"
alias h="history"
alias gc=". /usr/local/bin/gitdate && git commit -v "

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
	colorflag="--color"
else # OS X `ls`
	colorflag="-G"
fi

# List all files colorized in long format
# shellcheck disable=SC2139
alias l="ls -lhF ${colorflag}"

# List all files colorized in long format, including dot files
# shellcheck disable=SC2139
alias la="ls -lahF ${colorflag}"

# List only directories
# shellcheck disable=SC2139
alias lsd="ls -lhF ${colorflag} | grep --color=never '^d'"

# Always use color output for `ls`
# shellcheck disable=SC2139
alias ls="command ls ${colorflag}"
export LS_COLORS='no=00:fi=32:di=01;31:ln=01;35:pi=40;33:so=01;35:do=01;31:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:'

# Always enable colored `grep` output
alias grep='grep --color=auto '

# Enable aliases to be sudo’ed
alias sudo='sudo '

# Get week number
alias week='date +%V'

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'

# IP addresses
alias pubip="dig +short myip.opendns.com @resolver1.opendns.com"
alias localip="sudo ifconfig | grep -Eo 'inet (addr:)?([0-9]*\\.){3}[0-9]*' | grep -Eo '([0-9]*\\.){3}[0-9]*' | grep -v '127.0.0.1'"
alias ips="sudo ifconfig -a | grep -o 'inet6\\? \\(addr:\\)\\?\\s\\?\\(\\(\\([0-9]\\+\\.\\)\\{3\\}[0-9]\\+\\)\\|[a-fA-F0-9:]\\+\\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump="sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\\: .*|GET \\/.*\""

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"

# OS X has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"

# OS X has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"

# Trim new lines and copy to clipboard
alias c="tr -d '\\n' | pbcopy"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Merge PDF files
# Usage: `mergepdf -o output.pdf input{1,2,3}.pdf`
alias mergepdf='/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py'

# Intuitive map function
# For example, to list all directories that contain a certain file:
# find . -name .gitattributes | map dirname
alias map="xargs -n1"

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    # shellcheck disable=SC2139,SC2140
    alias "$method"="lwp-request -m \"$method\""
done

# vhosts
alias hosts='sudo vim /etc/hosts'

# copy working directory
alias cwd='pwd | tr -d "\r\n" | pbcopy'

# copy file interactive
alias cp='cp -i'

# move file interactive
alias mv='mv -i'

# untar
alias untar='tar xvf'

export PATH="${HOME}/.rbenv/bin:${PATH}"
eval "$(rbenv init -)"

# ASDF
. "$HOME/.asdf/asdf.sh"

export PATH="$(perl -e 'print join(":", grep { not $seen{$_}++ } split(/:/, $ENV{PATH}))')"
. "$HOME/.cargo/env"
