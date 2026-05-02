{
  config,
  lib,
  pkgs,
  osConfig,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.git;
  gnupgEnabled = lib.attrByPath ["modules" "shell" "gnupg" "enable"] false osConfig;
in {
  options.modules.shell.git = mkOption {
    description = "Git version control configuration";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options.enable = mkEnableOption "git";
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [
        gh
        git-open
        diff-so-fancy
        git-extras
        act
      ]
      ++ lib.optionals gnupgEnabled [git-crypt];

    programs.git = {
      enable = true;
      signing.format = null;

      settings = {
        user = {
          name = "Jordan Faust";
          email = "jordan.faust@procore.com";
        };
        commit.gpgSign = false;
        tag.gpgSign = false;
        core = {
          autocrlf = "input";
          compression = 9;
          whitespace = "trailing-space";
        };
        advice = {
          addEmptyPathspec = false;
          pushNonFastForward = false;
          statusHints = false;
        };
        blame = {
          coloring = "highlightRecent";
          date = "relative";
        };
        diff = {
          context = 3;
          renames = "copies";
          interHunkContext = 10;
        };
        init.defaultBranch = "main";
        log = {
          abbrevCommit = true;
          graphColors = "blue,yellow,cyan,magenta,green,red";
        };
        status = {
          branch = true;
          short = true;
          showStash = true;
          showUntrackedFiles = "all";
        };
        pager = {
          branch = false;
          diff = "diff-so-fancy | $PAGER";
        };
        push = {
          autoSetupRemote = true;
          default = "current";
          followTags = true;
          gpgSign = false;
        };
        pull.rebase = true;
        rebase.autoStash = true;
        "color \"blame\"" = {
          highlightRecent = "black bold,1 year ago,white,1 month ago,default,7 days ago,blue";
        };
        "color \"branch\"" = {
          current = "magenta";
          local = "default";
          remote = "yellow";
          upstream = "green";
          plain = "blue";
        };
        "color \"diff\"" = {
          meta = "black bold";
          frag = "magenta";
          context = "white";
          whitespace = "yellow reverse";
        };
        github.user = "JordanFaust";
        gitlab.user = "JordanFaust";
        "url \"git@github.com:\"" = {
          insteadOf = "https://github.com/";
        };
        "filter \"lfs\"" = {
          required = true;
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          clean = "git-lfs clean -- %f";
        };
        alias = {
          unadd = "reset HEAD";
          ranked-authors = "!git authors | sort | uniq -c | sort -n";
          emails = "!git log --format=\"%aE\" | sort -u";
          email-domains = "!git log --format=\"%aE\" | awk -F'@' '{print $2}' | sort -u";
        };
      };

      ignores = [
        # emacs
        "*~"
        "*.*~"
        "\\#*"
        ".#*"
        # vim
        "*.swp"
        ".*.sw[a-z]"
        "*.un~"
        ".netrwhist"
        # OS
        ".DS_Store?"
        ".DS_Store"
        ".CFUserTextEncoding"
        ".Trash"
        ".Xauthority"
        "thumbs.db"
        "Thumbs.db"
        "Icon?"
        # code
        ".ccls-cache/"
        ".sass-cache/"
        "__pycache__/"
        # compiled
        "*.class"
        "*.exe"
        "*.o"
        "*.pyc"
        "*.elc"
      ];

      attributes = [
        "*.lisp diff=lisp"
        "*.el   diff=lisp"
        "*.org  diff=org"
      ];
    };

    # Shell aliases and fzf integration (from former config/git/aliases.zsh)
    programs.zsh.initContent = lib.mkOrder 1500 ''
      g() { [[ $# = 0 ]] && git status --short . || git $*; }

      alias cdg='cd $(git rev-parse --show-toplevel)'
      alias git='noglob git'
      alias ga='git add'
      alias gap='git add --patch'
      alias gb='git branch -av'
      alias gop='git open'
      alias gbl='git blame'
      alias gc='git commit'
      alias gcm='git commit -m'
      alias gca='git commit --amend'
      alias gcf='git commit --fixup'
      alias gcl='git clone'
      alias gco='git checkout'
      alias gcoo='git checkout --'
      alias gf='git fetch'
      alias gi='git init'
      alias gl='git log --graph --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
      alias gll='git log --pretty="format:%C(yellow)%h%Creset %C(red)%G?%Creset%C(green)%d%Creset %s %Cblue(%cr) %C(bold blue)<%aN>%Creset"'
      alias gL='gl --stat'
      alias gp='git push'
      alias gpl='git pull --rebase --autostash'
      alias gs='git status --short .'
      alias gss='git status'
      alias gst='git stash'
      alias gr='git reset HEAD'
      alias gv='git rev-parse'

      if (( $+commands[fzf] )); then
        __git_log () {
          git log \
            --color=always \
            --graph \
            --all \
            --date=short \
            --format="%C(bold blue)%h%C(reset) %C(green)%ad%C(reset) | %C(white)%s %C(red)[%an] %C(bold yellow)%d"
        }

        _fzf_complete_git() {
          ARGS="$@"
          if [[ $ARGS == 'git cp'* || \
                $ARGS == 'git cherry-pick'* || \
                $ARGS == 'git co'* || \
                $ARGS == 'git checkout'* || \
                $ARGS == 'git reset'* || \
                $ARGS == 'git show'* || \
                $ARGS == 'git log'* ]]; then
            _fzf_complete "--reverse --multi" "$@" < <(__git_log)
          else
            eval "zle ''${fzf_default_completion:-expand-or-complete}"
          fi
        }

        _fzf_complete_git_post() {
          sed -e 's/^[^a-z0-9]*//' | awk '{print $1}'
        }
      fi
    '';
  };
}
