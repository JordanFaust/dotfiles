{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.zsh;
  linuxOnlyInit = optionalString pkgs.stdenv.isLinux ''
    alias y='xclip -selection clipboard -in'
    alias p='xclip -selection clipboard -out'
    function r {
      local time=$1; shift
      sched "$time" "notify-send --urgency=critical 'Reminder' ''$@'; ding"
    }; compdef r=sched
  '';
in {
  options.modules.shell.zsh = mkOption {
    description = "Zsh user shell configuration";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "zsh";
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nix-zsh-completions
      bat
      eza
      fasd
      fd
      fzf
      jq
      yq-go
      ripgrep
      tldr
    ];

    home.sessionVariables = {
      ZDOTDIR = "${config.xdg.configHome}/zsh";
      ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
      ZGEN_DIR = "${config.xdg.dataHome}/zgenom";
    };

    programs.zsh = {
      enable = true;
      dotDir = "${config.xdg.configHome}/zsh";

      history = {
        path = "${config.xdg.cacheHome}/zsh/zhistory";
        size = 100000;
        save = 100000;
        extended = true;
        share = true;
        ignoreDups = true;
        ignoreAllDups = true;
        ignoreSpace = true;
        expireDuplicatesFirst = true;
        findNoDups = true;
      };

      envExtra = ''
        function _cache {
          (( $+commands[$1] )) || return 1
          local cache_dir="''${XDG_CACHE_HOME:-$HOME/.cache}/''${SHELL##*/}"
          local cache="$cache_dir/$1"
          if [[ ! -f $cache || ! -s $cache ]]; then
              echo "Caching $1"
              mkdir -p $cache_dir
              "$@" >$cache
              chmod 600 $cache
          fi
          if [[ -o interactive ]]; then
              source $cache || rm -f $cache
          fi
        }

        function _source {
          for file in "$@"; do
            [ -r $file ] && source $file
          done
        }

        if (( EUID != 0 )); then
          umask 027
        else
          umask 077
        fi

        export PATH="''${KREW_ROOT:-$HOME/.krew}/bin:$PATH"

        _source $ZDOTDIR/local.zshenv

        # Sensitive API keys (read from /etc/sensitive at runtime — NOT Nix interpolation)
        export OPENAI_API_KEY="$(cat /etc/sensitive/openai 2>/dev/null)"
        export GEMINI_API_KEY="$(cat /etc/sensitive/gemini 2>/dev/null)"
        export TAVILY_API_KEY="$(cat /etc/sensitive/tavily 2>/dev/null)"
        export ARTIFACTORY_USERNAME="$(cat /etc/sensitive/artifactory 2>/dev/null | cut -d':' -f1)"
        export ARTIFACTORY_PASSWORD="$(cat /etc/sensitive/artifactory 2>/dev/null | cut -d':' -f2)"
        export JIRA_API_TOKEN="$(cat /etc/sensitive/jira 2>/dev/null)"
        export SONARQUBE_TOKEN="$(cat /etc/sensitive/sonarqube 2>/dev/null)"
        export GITHUB_TOKEN="$(gh auth token 2>/dev/null)"
        export CONTEXT7_API_KEY="$(cat /etc/sensitive/context7 2>/dev/null)"

        # Export additional configuration options
        export OPENCODE_EXPERIMENTAL_LSP_TOOL=true
      '';

      profileExtra = ''
        eval "$(mise activate zsh --shims)"
      '';

      initContent = ''
        ## Plugins
        export ZGEN_AUTOLOAD_COMPINIT=0
        export ZVM_INIT_MODE=sourcing
        export ZVM_VI_INSERT_ESCAPE_BINDKEY=jk
        export _FASD_DATA="$XDG_CACHE_HOME/fasd"
        export _FASD_VIMINFO="$XDG_CACHE_HOME/viminfo"

        if (( $+commands[fd] )); then
          export FZF_DEFAULT_OPTS='
          --layout=reverse --info=inline --ansi
          --border=rounded --margin=20%,20%
          --padding=1
          --height=40%
          '
          export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS --bind='ctrl-r:toggle-header'"
          export FZF_DEFAULT_COMMAND="fd ."
          export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
          export FZF_ALT_C_COMMAND="fd -t d . $HOME"
          export FZF_CTRL_R_OPTS="--reverse --ansi"
        fi

        ## ZSH configuration
        WORDCHARS='_-*?[]~&.;!#$%^(){}<>'
        unsetopt BRACE_CCL
        setopt COMBINING_CHARS RC_QUOTES HASH_LIST_ALL
        unsetopt CORRECT_ALL NOMATCH MAIL_WARNING BEEP
        setopt IGNOREEOF

        ## Jobs
        setopt LONG_LIST_JOBS AUTO_RESUME NOTIFY
        unsetopt BG_NICE HUP CHECK_JOBS

        ## History (additional options not in programs.zsh.history)
        setopt BANG_HIST APPEND_HISTORY INC_APPEND_HISTORY HIST_SAVE_NO_DUPS HIST_VERIFY HIST_BEEP HIST_REDUCE_BLANKS

        ## Directories
        DIRSTACKSIZE=9
        unsetopt AUTO_CD
        setopt AUTO_PUSHD PUSHD_IGNORE_DUPS PUSHD_SILENT PUSHD_TO_HOME CDABLE_VARS MULTIOS EXTENDED_GLOB
        unsetopt GLOB_DOTS AUTO_NAME_DIRS

        ## Programming Language Config
        export GOPRIVATE="github.com/procore,$GOPRIVATE"

        ## Zgenom
        if [ ! -d "$ZGEN_DIR" ]; then
          echo "Installing jandamm/zgenom"
          git clone https://github.com/jandamm/zgenom "$ZGEN_DIR"
        fi
        source $ZGEN_DIR/zgenom.zsh
        zgenom autoupdate

        if ! zgenom saved; then
          echo "Initializing zgenom"
          rm -f $ZDOTDIR/*.zwc(N) \
                $XDG_CACHE_HOME/zsh/*(N) \
                $ZGEN_INIT.zwc

          zgenom load junegunn/fzf shell
          zgenom load jeffreytse/zsh-vi-mode
          zgenom load zdharma-continuum/fast-syntax-highlighting
          zgenom load zsh-users/zsh-completions src
          zgenom load zsh-users/zsh-autosuggestions
          zgenom load zsh-users/zsh-history-substring-search
          zgenom load romkatv/powerlevel10k powerlevel10k
          zgenom load hlissner/zsh-autopair autopair.zsh
          zgenom load chisui/zsh-nix-shell nix-shell.plugin.zsh

          zgenom save
          zgenom compile $ZDOTDIR
        fi

        ## Keybinds
        bindkey -M viins '^a' beginning-of-line
        bindkey -M viins '^d' push-line-or-edit
        bindkey '\e[A' history-substring-search-up
        bindkey '\eOA' history-substring-search-up
        bindkey '\e[B' history-substring-search-down
        bindkey '\eOB' history-substring-search-down
        fancy-ctrl-z () {
          if [[ $#BUFFER -eq 0 ]]; then
            BUFFER="fg"
            zle accept-line
          else
            zle push-input
            zle clear-screen
          fi
        }
        zle -N fancy-ctrl-z
        bindkey '^Z' fancy-ctrl-z
        if (( $+commands[fzf] )); then
          bindkey '^R' fzf-history-widget
        fi
        if (( $+commands[fasd] )); then
          bindkey -M viins '^x^f' fasd-complete-f
          bindkey -M viins '^x^d' fasd-complete-d
        fi
        if [ -n "$TMUX" ]; then
          _tmux_pane_words() {
            local expl
            local -a w
            if [[ -z "$TMUX_PANE" ]]; then
              _message "not running inside tmux!"
              return 1
            fi
            w=( ''${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
            _wanted values expl 'words from current tmux pane' compadd -a w
          }
          zle -C tmux-pane-words-prefix   complete-word _generic
          zle -C tmux-pane-words-anywhere complete-word _generic
          bindkey -M viins '^x^n' tmux-pane-words-prefix
          bindkey -M viins '^x^o' tmux-pane-words-anywhere
          zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
          zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
          zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'
        fi
        history-beginning-search-backward-then-append() {
          zle history-beginning-search-backward
          zle vi-add-eol
        }
        zle -N history-beginning-search-backward-then-append
        bindkey -M viins '^x^l' history-beginning-search-backward-then-append

        ## Completion
        fpath+=( $ZDOTDIR/completions )
        ZSH_AUTOSUGGEST_STRATEGY=(completion)
        ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=30
        zstyle ':completion:*:paths' path-completion yes
        zstyle ':bracketed-paste-magic' active-widgets '.self-*'
        setopt COMPLETE_IN_WORD PATH_DIRS AUTO_MENU AUTO_LIST
        unsetopt MENU_COMPLETE COMPLETE_ALIASES CASE_GLOB
        zstyle ':completion:*' completer _complete _list _match _approximate
        zstyle ':completion:*:match:*' original only
        zstyle ':completion:*:approximate:*' max-errors 1 numeric
        zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
        zstyle ':completion:*:functions' ignored-patterns '(_*|.*|pre(cmd|exec))'
        zstyle ':completion:*:corrections' format '%B%F{green}%d (errors: %e)%f%b'
        zstyle ':completion:*:messages' format '%B%F{yellow}%d%f%b'
        zstyle ':completion:*:warnings' format '%B%F{red}No such %d%f%b'
        zstyle ':completion:*:errors' format '%B%F{red}No such %d%f%b'
        zstyle ':completion:*:descriptions' format $'%{\e[35;1m%}%d%{\e[0m%}'
        zstyle ':completion:*:default' list-prompt '%S%M matches%s'
        zstyle ':completion:*:*:cd:*' ignore-parents parent pwd
        zstyle ':completion:*' squeeze-slashes true
        zstyle ':completion:*:history-words' stop yes
        zstyle ':completion:*:history-words' remove-all-dups yes
        zstyle ':completion:*:history-words' list false
        zstyle ':completion:*:history-words' menu yes
        zstyle ':completion::*:(-command-|export):*' fake-parameters ''${''${''${_comps[(I)-value-*]#*,}%%,*}:#-*-}
        zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
        zstyle -e ':completion:*:hosts' hosts 'reply=(
          ''${=''${=''${=''${=''${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
          ''${=''${''${''${''${(@M)''${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
        )'
        zstyle ':completion:*:users' ignored-patterns \
          adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
          dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
          hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
          mailman mailnull mldonkey mysql nagios \
          named netdump news nfsnobody nobody 'nixbld*' nscd ntp nut nx openvpn \
          operator pcap postfix postgres privoxy pulse pvm quagga radvd \
          rpc rpcuser rpm shutdown squid sshd sync 'systemd-*' uucp vcsa xfs '_*'
        zstyle '*' single-ignored show
        zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
        zstyle ':completion:*:rm:*' file-patterns '*:all-files'
        zstyle ':completion:*:*:*:*:processes' command 'ps -u $LOGNAME -o pid,user,command -w'
        zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
        zstyle ':completion:*:*:kill:*' menu yes select
        zstyle ':completion:*:*:kill:*' force-list always
        zstyle ':completion:*:*:kill:*' insert-ids single
        zstyle ':completion:*:manuals' separate-sections true
        zstyle ':completion:*:manuals.(^1*)' insert-sections true
        zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
        zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
        zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
        zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'
        zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
        zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
        zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
        zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host users hosts-ipaddr
        zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
        zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
        zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

        ## Bootstrap interactive sessions
        if [[ $TERM != dumb ]]; then
          autoload -Uz compinit && compinit -u -d $ZSH_CACHE/zcompdump
          _cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}
          _source $ZDOTDIR/local.zshrc
          autopair-init
        fi

        # Conditional aliases and functions (mkAfter)
        if (( $+commands[eza] )); then
          alias eza="eza --group-directories-first --git"
          alias l="eza -blF"
          alias ll="eza -abghilmu"
          alias llm='ll --sort=modified'
          alias la="LC_COLLATE=C eza -ablF"
          alias tree='eza --tree'
        fi

        if (( $+commands[fasd] )); then
          unalias z 2>/dev/null
          function z {
            [ $# -gt 0 ] && _z "$*" && return
            cd "$(_z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "''${*##-* }" | sed 's/^[0-9,.]* *//')"
          }
        fi

        autoload -U zmv

        function claude-gw {
          ANTHROPIC_AUTH_TOKEN="$(cat /etc/sensitive/anthropic 2>/dev/null)" \
          ANTHROPIC_BASE_URL="$(cat /etc/sensitive/anthropic-base-url 2>/dev/null)" \
          command claude "$@"
        }

        function take {
          mkdir "$1" && cd "$1"
        }; compdef take=mkdir

        function zman {
          PAGER="less -g -I -s '+/^       "$1"'" man zshall
        }

        rcp() {
          rsync -azPJ \
            --include=.git/ \
            --filter=':- .gitignore' \
            --filter=":- $XDG_CONFIG_HOME/git/ignore" \
            "$@"
        }; compdef rcp=rsync
        alias rcpd='rcp --delete --delete-after'
        alias rcpu='rcp --chmod=go='
        alias rcpdu='rcpd --chmod=go='

        function awsp {
          selected_profile=$(grep profile ''${HOME}/.aws/config \
            | awk '{print $2}' | sed 's,],,g' \
            | fzf --layout reverse --height 10% --border)
          export AWS_PROFILE=''${selected_profile}
          aws sts get-caller-identity --query "Account" &> /dev/null
          if [[ "$?" -ne 0 ]]; then
            echo "Logging in via AWS SSO"
            aws sso login --profile ''${selected_profile}
          fi
        }

        ${linuxOnlyInit}

        eval "$(mise activate zsh)"
      '';

      shellAliases = {
        ".." = "cd ..";
        "..." = "cd ../..";
        "...." = "cd ../../..";
        "-" = "cd -";
        q = "exit";
        clr = "clear";
        sudo = "sudo ";
        rm = "rm -i";
        cp = "cp -i";
        mv = "mv -i";
        mkdir = "mkdir -pv";
        wget = "wget -c";
        path = ''echo -e ''${PATH//:/\\n}'';
        ports = "netstat -tulanp";
        mk = "make";
        gurl = "curl --compressed";
        shutdown = "sudo shutdown";
        reboot = "sudo reboot";
        k = "kubectl";
        jc = "journalctl -xe";
        jcu = "journalctl --user -xe";
        sc = "systemctl";
        ssc = "sudo systemctl";
      };
    };

    # _hey completion (from config/zsh/completions/)
    xdg.configFile."zsh/completions/_hey" = {
      text = ''
        #compdef hey

        _hey_dispatch () {
            local cmd="$@"
            local offset=$#
            CURRENT=$((CURRENT-$offset))
            words=($cmd "''${words[$((2 + offset)),-1]}")
            _$cmd
        }

        _hey_command_list () {
            hey | sed '1,/Available commands:/d' | awk '{ print $1 }'
        }

        _hey () {
            local -a commands
            IFS=$'\n' commands=($(_hey_command_list))

            if (( CURRENT == 2 )); then
                _describe -t commands "hey commands" commands
                return
            fi
            integer ret=1
            case ''${words[2]} in
                ch|channel) _hey_dispatch nix-channel ;;
                b|build)    _hey_dispatch nix-build ;;
                ch|check)   _hey_dispatch nix flake check ;;
                sh|show)    _hey_dispatch nix flake show ;;
                s|search)   _hey_dispatch nix search nixpkgs ;;
                -*)         _hey_dispatch nix-env ;;
                *)          _message "Command not found: ''${words[2]}" ;;
            esac
            return ret
        }

        _hey
      '';
    };
  };
}
