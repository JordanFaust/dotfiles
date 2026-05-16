{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = mkOption {
    description = "tmux terminal multiplexer configuration";
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "tmux";
              sessionManager = {
                searchPaths = mkOption {
                  description = "Search paths for git repository discovery";
                  type = with lib.types;
                    listOf (submoduleWith {
                      modules = [
                        {
                          options = {
                            path = mkOption {
                              type = str;
                              description = "Absolute path to search";
                            };
                            name = mkOption {
                              type = str;
                              default = "";
                              description = "Fixed session name override (optional)";
                            };
                            depth = mkOption {
                              type = int;
                              default = 0;
                              description = "0 = directory is itself a repo; 1 = scan one level deep";
                            };
                          };
                        }
                      ];
                    });
                  default = [];
                };
              };
            };
          }
        ];
      });
    default = {};
  };

  config = mkIf cfg.enable (let
    tsmPkg = pkgs.callPackage ../../../../packages/tsm {};
    tmuxPalettePkg = pkgs.callPackage ../../../../packages/tmux-palette {};

    tomlSearchPaths =
      lib.concatMapStrings (sp: ''
        [[search_paths]]
        path  = "${sp.path}"
        name  = "${sp.name}"
        depth = ${toString sp.depth}

      '')
      cfg.sessionManager.searchPaths;
  in {
    home.packages = with pkgs; [
      tmate
      tsmPkg
      tmuxPalettePkg
    ];

    home.sessionVariables = {
      TMUX_HOME = "${config.xdg.configHome}/tmux";
      TMUXIFIER = "${config.xdg.dataHome}/tmuxifier";
      TMUXIFIER_LAYOUT_PATH = "${config.xdg.dataHome}/tmuxifier";
    };

    home.sessionPath = ["${config.xdg.dataHome}/tmuxifier/bin"];

    xdg.configFile."tmux/tsm.toml" = lib.mkIf (cfg.sessionManager.searchPaths != []) {
      text = tomlSearchPaths;
    };

    xdg.configFile."tmux/scripts/swap-pane.sh" = {
      source = ./scripts/swap-pane.sh;
      executable = true;
    };

    # tmux-palette custom palette: session picker powered by tsm finder json.
    # Groups repos by Current / Active / Recent / All with status-dot icons.
    xdg.configFile."tmux-palette/palettes/sessions.json".text = builtins.toJSON {
      title = "Sessions";
      icon = "";
      grouped = true;
      command = "tsm finder json";
    };

    # Add a "Switch Session" entry to the main tmux-palette commands palette so
    # it is reachable from the global C-Space binding as well as bind p.
    xdg.configFile."tmux-palette/commands.json".text = builtins.toJSON [
      {
        icon = "";
        title = "Switch Session";
        category = "Tmux";
        action = {palette = "sessions";};
      }
    ];

    # Catppuccin Macchiato theme for tmux-palette.
    # Colours match the rest of the desktop theme (same values used in ansi.go).
    xdg.configFile."tmux-palette/themes/catppuccin-macchiato.json".text = builtins.toJSON {
      bg = "#24273a"; # Base
      panel = "#1e2030"; # Mantle
      selected = "#363a4f"; # Surface0
      fg = "#cad3f5"; # Text
      muted = "#6e738d"; # Overlay0
      accent = "#8aadf4"; # Blue
    };

    # Activate the Macchiato theme.
    xdg.configFile."tmux-palette/theme.json".text = builtins.toJSON {
      name = "catppuccin-macchiato";
    };

    programs.zsh.initContent = ''
      alias ta='tmux attach'
      alias tl='tmux ls'

      if [[ -n $TMUX ]]; then
        alias tf='tmux find-window'
        alias mine='tmux detach -a'
        tt() {
          tmux send-keys -t .+ C-u && \
            tmux set-buffer "$*" && \
            tmux paste-buffer -t .+ && \
            tmux send-keys -t .+ Enter;
        }
        tn() {
          local name="''${1:-$(basename $PWD)}"
          TMUX= tmux new-session -d -s "$name"
          tmux switch-client -t "$name"
          tmux display-message "Session #S created"
        }
      else
        tdup() { tmux new-session -t "''${1:-$(tmux display-message -p '#S')}"; }
      fi
    '';

    programs.tmux = {
      enable = true;
      prefix = "C-Space";
      mouse = true;
      keyMode = "vi";
      baseIndex = 1;
      historyLimit = 50000;
      escapeTime = 0;
      aggressiveResize = false;
      focusEvents = true;
      disableConfirmationPrompt = true;
      terminal = "tmux-256color";

      plugins = with pkgs.tmuxPlugins; [
        copycat
        prefix-highlight
        yank
        resurrect
      ];

      extraConfig = ''
        set -g extended-keys on
        set -ga terminal-features '*:extkeys'
        set -ag terminal-overrides ",xterm-256color:RGB"
        set -g allow-passthrough on

        # Window management
        setw -g allow-rename off
        setw -g automatic-rename off
        set  -g renumber-windows on
        setw -g pane-base-index 1

        # Display
        set  -g display-time 1500
        set  -g display-panes-time 800
        set -sg repeat-time 600
        set  -g status-interval 5

        # Session
        set -g detach-on-destroy off
        set-option -g set-clipboard external

        ${lib.optionalString pkgs.stdenv.isLinux "set -s copy-command 'wl-copy'"}

        ########################################
        # Keybinds
        ########################################

        bind c new-window      -c "#{pane_current_path}"
        bind V split-window -h -c "#{pane_current_path}"
        bind S split-window -v -c "#{pane_current_path}"

        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        bind H run '$TMUX_HOME/scripts/swap-pane.sh left'
        bind J run '$TMUX_HOME/scripts/swap-pane.sh down'
        bind K run '$TMUX_HOME/scripts/swap-pane.sh up'
        bind L run '$TMUX_HOME/scripts/swap-pane.sh right'
        bind M run '$TMUX_HOME/scripts/swap-pane.sh master'

        bind f resize-pane -Z
        bind p run-shell '${tmuxPalettePkg}/bin/tmux-palette.sh sessions'
        bind P run-shell '${tmuxPalettePkg}/bin/tmux-palette.sh'
        bind w choose-window
        bind / choose-session
        bind . choose-window

        bind | select-layout even-horizontal
        bind _ select-layout even-vertical
        bind x kill-pane
        bind X kill-window
        bind q kill-session
        bind Q kill-server

        # Smart pane switching (vim-tmux-navigator)
        is_vim='echo "#{pane_current_command}" | grep -iqE "(^|\/)g?(view|n?vim?x?)(diff)?$"'
        bind -n C-h if-shell "$is_vim" "send-keys C-h" "select-pane -L"
        bind -n C-j if-shell "$is_vim" "send-keys C-j" "select-pane -D"
        bind -n C-k if-shell "$is_vim" "send-keys C-k" "select-pane -U"
        bind -n C-l if-shell "$is_vim" "send-keys C-l" "select-pane -R"
        bind C-w last-pane
        bind C-n next-window
        bind C-p previous-window

        bind = select-layout even-vertical
        bind + select-layout even-horizontal
        bind - break-pane
        bind _ join-pane

        bind C-l send-keys C-l \; send-keys -R \; clear-history
        bind r source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded"
        bind ^r refresh-client

        ########################################
        # Copy mode
        ########################################

        bind Enter copy-mode
        bind b list-buffers
        bind B choose-buffer

        bind -T copy-mode-vi v send-keys -X begin-selection
        bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
        bind -T copy-mode-vi Escape send-keys -X cancel
        bind -T copy-mode-vi C-g send-keys -X cancel
        bind -T copy-mode-vi H send-keys -X start-of-line
        bind -T copy-mode-vi L send-keys -X end-of-line

        ${lib.optionalString pkgs.stdenv.isLinux ''
          bind v run-shell "wl-paste --no-newline | tmux load-buffer - ; tmux paste-buffer"
          bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "wl-copy"
          bind -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel "wl-copy"
        ''}

        ########################################
        # Plugins
        ########################################

        set -g @open-editor 'C-e'
        set -g @open-S 'https://www.duckduckgo.com/'
        set -g @resurrect-processes 'ssh sqlite3 "git log"'

      '';
    };
  });
}
