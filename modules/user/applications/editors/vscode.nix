{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.applications.vscode;
in {
  options.modules.applications.vscode = mkOption {
    description = ''
      The (forced) one and only text editor.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              #
              # Neovim
              #
              enable = mkEnableOption "vscode";
            };
          }
        ];
      });
    default = {
      enable = true;
    };
  };

  # Always enable neovim, no matter the installation
  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        # fzf-picker dependnecies
        fzf
        ripgrep
        bat
      ];
    };

    programs.vscode = {
      enable = true;
      # Set the package to the FHS-wrapped version of VS Code
      package = pkgs.vscode.fhs;
      mutableExtensionsDir = true;

      profiles = {
        default = {
          # enableUpdateCheck = false;
          # enableExtensionUpdateCheck = false;

          extensions = with pkgs.vscode-extensions;
            [
              # Theme
              catppuccin.catppuccin-vsc
              catppuccin.catppuccin-vsc-icons
              # Neovim
              asvetliakov.vscode-neovim
              # Search Extensions
              # Extensions
              vspacecode.whichkey
            ]
            ++ (with pkgs.vscode-marketplace; [
              jellydn.fzf-picker
            ]);

          userSettings = {

            #
            # General Settings
            #

            workbench.settings.editor = "json"; # Show setting in json as default
            # Comment below to show status bar
            workbench.statusBar.visible = false;

            #
            # Window Settings
            #

            window.titleBarStyle = "native";
            window.customTitleBarVisibility = "never";
            # Increase size of explorer, headers, menus, etc.
            window.zoomLevel = 1.5;

            #
            # Theme
            #
            workbench.colorTheme = "Catppuccin Mocha";
            workbench.iconTheme = "catppuccin-macchiato";

            # we try to make semantic highlighting look good
            editor.semanticHighlighting.enabled = true;
            # prevent VSCode from modifying the terminal colors
            terminal.integrated.minimumContrastRatio = 1;
            # make the window's titlebar use the workbench colors
            # window.titleBarStyle = "custom";

            # pink as the accent color
            # catppuccin.accentColor = "pink";
            # # use your accent (pink) on the statusBar as well
            # catppuccin.customUIColors = {
            #   mocha = {
            #     statusBar.foreground = "accent";
            #   };
            # };

            # Set sidebar to the right
            workbench.sideBar.location = "right";
            custom-ui-style.stylesheet = {
              # Setup for CSS for Which-Key
              # Let the quick pick take the full window height, so that more bindings are visible.
              ".quick-input-widget > .quick-input-list > .monaco-list" = "max-height: 100vh !important;";
              # Hide editor top-right buttons, except for the dirty file indicator and settings goto icon
              ".editor .title .actions-container .action-item a:not(.codicon-close-dirty):not(.codicon-preferences-open-settings)" = "display: none !important;";
            };

            #
            # Terminal
            #
            terminal.integrated.fontFamily = "MonoLisa Variable Medium";
            terminal.integrated.fontSize = 14;
            terminal.integrated.sendKeybindingsToShell = true;

            #
            # Editor
            #

            # Inlay hints
            editor.inlayHints.enabled = "on";
            editor.inlayHints.fontFamily = "MonoLisa Variable Medium";
            editor.inlayHints.fontSize = 1;
            editor.inlayHints.padding = true;
            editor.minimap.enabled = false;

            # General editor settings
            editor.autoIndent = "full";
            editor.cursorBlinking = "solid";
            editor.cursorStyle = "line";
            editor.cursorWidth = 5;
            editor.fontFamily = "MonoLisa Variable, OperatorMonoLig Nerd Font, JetBrainsMono Nerd Font Mono, Menlo, Monaco, 'Courier New', monospacere";
            editor.codeLensFontFamily = "MonoLisa Variable Medium";
            editor.fontLigatures = true;
            editor.fontSize = 14;
            # editor.fontWeight = "400";
            editor.formatOnSave = true;
            editor.inlineSuggest.enabled = true;
            editor.insertSpaces = false;
            editor.largeFileOptimizations = false;
            editor.letterSpacing = 0.5;
            editor.lineHeight = 25;
            editor.renderWhitespace = "all";
            editor.suggestSelection = "first";
            editor.tabCompletion = "on";
            editor.wordSeparators = "/\\()\"':,.;<>~!@#$%^&*|+=[]{}`?-";
            editor.semanticTokenColorCustomizations = {
              enabled = true;
            };

            # Zen Mode
            zenMode.fullScreen = false;
            zenMode.hideLineNumbers = false;
            zenMode.centerLayout = false;
            zenMode.silentNotifications = false;

            #
            # Explorer Settings
            #

            explorer.confirmDelete = false;
            explorer.compactFolders = false;
            explorer.confirmDragAndDrop = false;
            errorLens.enabledDiagnosticLevels = ["error"];
            errorLens.excludeBySource = ["cSpell"];


            #
            # Languages
            #

            # Go
            gopls = {
              ui.semanticTokens = true;
            };

            # Javascript
            javascript.validate.enable = false;
            javascript.inlayHints.enumMemberValues.enabled = true;
            javascript.inlayHints.functionLikeReturnTypes.enabled = true;
            javascript.inlayHints.parameterNames.enabled = "literals";
            javascript.inlayHints.variableTypes.enabled = false;
            javascript.updateImportsOnFileMove.enabled = "always";
            typescript.enablePromptUseWorkspaceTsdk = true;
            typescript.inlayHints.enumMemberValues.enabled = true;
            typescript.inlayHints.functionLikeReturnTypes.enabled = true;
            typescript.inlayHints.parameterNames.enabled = "literals";
            typescript.inlayHints.variableTypes.enabled = false;
            typescript.referencesCodeLens.enabled = true;
            typescript.updateImportsOnFileMove.enabled = "always";
            typescript.preferences.preferTypeOnlyAutoImports = true;
            # make sure you're using the local typescript
            typescript.tsdk = "node_modules/typescript/lib";

            # Diff
            diffEditor.codeLens = true;
            diffEditor.hideUnchangedRegions.enabled = true;

            #
            # Neovim Settings
            #

            # Neovim setting, refer https://open-vsx.org/extension/asvetliakov/vscode-neovim
            extensions.experimental.affinity = {
              "asvetliakov.vscode-neovim" = 1;
            };

            # Point the extension to the wrapper script
            # vscode-neovim.neovimExecutablePaths.linux = "${nvimWrapperScript}";
            vscode-neovim.compositeKeys = {
              jj = {
                command = "vscode-neovim.escape";
              };
              jk = {
                command = "vscode-neovim.escape";
              };
            };

            #
            # Which Key
            #
            whichkey.sortOrder = "alphabetically";

            whichkey.bindings = [
              {
                key = " ";
                name = "Commands";
                type = "command";
                command = "workbench.action.showCommands";
              }
              {
                key = "\t"; # Represents the Tab character
                name = "Last editor";
                type = "commands";
                commands = [
                  "workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup"
                  "list.select"
                ];
              }
              {
                key = "?";
                name = "Search keybindings";
                type = "command";
                command = "whichkey.searchBindings";
              }
              {
                key = ".";
                name = "Repeat most recent action";
                type = "command";
                command = "whichkey.repeatMostRecent";
              }
              {
                key = "b";
                name = "+Buffers/Editors";
                type = "bindings";
                bindings = [
                  {
                    key = "d";
                    name = "Close active editor";
                    type = "command";
                    command = "workbench.action.closeActiveEditor";
                  }
                  {
                    key = "o";
                    name = "Close active editor"; # Name seems incorrect, likely means "Close other editors"
                    type = "command";
                    command = "workbench.action.closeOtherEditors";
                  }
                ];
              }
              {
                key = "c";
                name = "+Code";
                type = "bindings";
                bindings = [
                  {
                    key = "a";
                    name = "Code Action";
                    type = "command";
                    command = "editor.action.codeAction";
                  }
                  {
                    key = "A";
                    name = "Source action";
                    type = "command";
                    command = "editor.action.sourceAction";
                  }
                  {
                    key = "i";
                    name = "Organize Imports";
                    type = "command";
                    command = "editor.action.organizeImports";
                  }
                  {
                    key = "r";
                    name = "Rename";
                    type = "command";
                    command = "editor.action.rename";
                  }
                  {
                    key = "R";
                    name = "Refactor";
                    type = "command";
                    command = "editor.action.Refactor";
                  }
                  {
                    key = "f";
                    name = "Format";
                    type = "command";
                    command = "editor.action.formatDocument";
                  }
                  {
                    key = "=";
                    name = "Format selection";
                    type = "command";
                    command = "editor.action.formatSelection";
                  }
                  {
                    key = ".";
                    name = "Quick Fix";
                    type = "command";
                    command = "editor.action.quickFix";
                  }
                ];
              }
              {
                key = "x";
                name = "+Errors";
                type = "bindings";
                bindings = [
                  {
                    key = "x";
                    name = "List errors";
                    type = "command";
                    command = "workbench.actions.view.problems";
                  }
                  {
                    key = "n";
                    name = "Next error";
                    type = "command";
                    command = "editor.action.marker.next";
                  }
                  {
                    key = "p";
                    name = "Previous error";
                    type = "command";
                    command = "editor.action.marker.prev";
                  }
                ];
              }
              {
                key = "f";
                name = "+File";
                type = "bindings";
                bindings = [
                  {
                    key = "a";
                    name = "Show all opened files";
                    type = "command";
                    command = "workbench.action.showAllEditors";
                  }
                  {
                    key = "e";
                    name = "Show active in explorer";
                    type = "command";
                    command = "workbench.action.toggleSidebarVisibility";
                  }
                  {
                    key = "f";
                    name = "Quick Open file";
                    type = "command";
                    command = "fzf-picker.findFiles";
                  }
                  {
                    key = "F";
                    name = "Open file in folder";
                    type = "command";
                    command = "workbench.action.files.openFileFolder";
                  }
                  {
                    key = "n";
                    name = "New Untitled";
                    type = "command";
                    command = "workbench.action.files.newUntitledFile";
                  }
                  {
                    key = "p";
                    name = "Switch project";
                    type = "command";
                    command = "workbench.action.openRecent";
                  }
                  {
                    key = "s";
                    name = "Save file";
                    type = "command";
                    command = "workbench.action.files.save";
                  }
                  {
                    key = "S";
                    name = "Save all files";
                    type = "command";
                    command = "workbench.action.files.saveAll";
                  }
                  {
                    key = "w";
                    name = "Open active in new window";
                    type = "command";
                    command = "workbench.action.files.showOpenedFileInNewWindow";
                  }
                ];
              }
              {
                key = "g";
                name = "+Git";
                type = "bindings";
                bindings = [
                  {
                    key = "f";
                    name = "Fetch";
                    type = "command";
                    command = "git.fetch";
                  }
                  {
                    key = "i";
                    name = "Init";
                    type = "command";
                    command = "git.init";
                  }
                  {
                    key = "s";
                    name = "Git status";
                    type = "command";
                    command = "workbench.view.scm";
                  }
                  {
                    key = "r";
                    name = "Pull Request";
                    type = "command";
                    command = "workbench.view.extension.github-pull-request";
                  }
                ];
              }
              {
                key = "s";
                name = "+Search/Symbol";
                type = "bindings";
                bindings = [
                  {
                    key = "s";
                    name = "Symbol in file";
                    type = "command";
                    command = "workbench.action.gotoSymbol";
                  }
                  {
                    key = "S";
                    name = "All symbols in workspace";
                    type = "command";
                    command = "workbench.action.showAllSymbols";
                  }
                  {
                    key = "w";
                    name = "Search word in a project";
                    type = "commands";
                    commands = [
                      "editor.action.addSelectionToNextFindMatch"
                      "workbench.action.findInFiles"
                    ];
                  }
                  {
                    key = "r";
                    name = "Search all references";
                    type = "command";
                    command = "editor.action.referenceSearch.trigger";
                  }
                  {
                    key = "R";
                    name = "Search all references in side bar";
                    type = "command";
                    command = "references-view.find";
                  }
                ];
              }
              {
                key = "u";
                name = "+UI";
                type = "bindings";
                bindings = [
                  {
                    key = "c";
                    name = "Select theme color";
                    type = "command";
                    command = "workbench.action.selectTheme";
                  }
                  {
                    key = "x";
                    name = "Show extensions";
                    type = "command";
                    command = "workbench.view.extensions";
                  }
                  {
                    key = "o";
                    name = "Show output";
                    type = "command";
                    command = "workbench.action.output.toggleOutput";
                  }
                  {
                    key = "d";
                    name = "Show debug console";
                    type = "command";
                    command = "workbench.debug.action.toggleRepl";
                  }
                ];
              }
              {
                key = "t";
                name = "+Toggles";
                type = "bindings";
                bindings = [
                  {
                    key = "c";
                    name = "Toggle find case sensitive";
                    type = "command";
                    command = "toggleFindCaseSensitive";
                  }
                  {
                    key = "r";
                    name = "Toggle screencast record";
                    type = "command";
                    command = "workbench.action.toggleScreencastMode";
                  }
                  {
                    key = "s";
                    name = "Toggle status bar";
                    type = "command";
                    command = "workbench.action.toggleStatusbarVisibility";
                  }
                  {
                    key = "w";
                    name = "Toggle ignore trim whitespace in diff";
                    type = "command";
                    command = "toggle.diff.ignoreTrimWhitespace";
                  }
                  {
                    key = "W";
                    name = "Toggle word wrap";
                    type = "command";
                    command = "editor.action.toggleWordWrap";
                  }
                ];
              }
              {
                key = "w";
                name = "+Window";
                type = "bindings";
                bindings = [
                  {
                    key = "-";
                    name = "Split editor below";
                    type = "command";
                    command = "workbench.action.splitEditorDown";
                  }
                  {
                    key = "/";
                    name = "Split editor right";
                    type = "command";
                    command = "workbench.action.splitEditor";
                  }
                  {
                    key = "t";
                    name = "Toggle editor group sizes";
                    type = "command";
                    command = "workbench.action.toggleEditorWidths";
                  }
                  {
                    key = "m";
                    name = "Maximize editor group";
                    type = "command";
                    command = "workbench.action.minimizeOtherEditors";
                  }
                  # Keep 'j' for down - Matches your request
                  {
                    key = "j";
                    name = "Navigate down";
                    type = "command";
                    command = "workbench.action.navigateDown";
                  }
                  # Keep 'k' for up - Matches your request
                  {
                    key = "k";
                    name = "Navigate up";
                    type = "command";
                    command = "workbench.action.navigateUp";
                  }
                  # Modify 'h' to navigate RIGHT - Swapping command
                  {
                    key = "h";
                    name = "Navigate right"; # Updated name
                    type = "command";
                    command = "workbench.action.navigateRight"; # Updated command
                  }
                  # Modify 'l' to navigate LEFT - Swapping command
                  {
                    key = "l";
                    name = "Navigate left"; # Updated name
                    type = "command";
                    command = "workbench.action.navigateLeft"; # Updated command
                  }
                ];
              }
              {
                key = "z";
                name = "+Folding";
                type = "bindings";
                bindings = [
                  {
                    key = "a";
                    name = "Toggle: around a point";
                    type = "command";
                    command = "editor.toggleFold";
                  }
                  {
                    key = "c";
                    name = "Close: at a point";
                    type = "command";
                    command = "editor.fold";
                  }
                  {
                    key = "b";
                    name = "Close: all block comments";
                    type = "command";
                    command = "editor.foldAllBlockComments";
                  }
                  {
                    key = "g";
                    name = "Close: all regions";
                    type = "command";
                    command = "editor.foldAllMarkerRegions";
                  }
                  {
                    key = "m";
                    name = "Close: all";
                    type = "command";
                    command = "editor.foldAll";
                  }
                  {
                    key = "o";
                    name = "Open: at a point";
                    type = "command";
                    command = "editor.unfold";
                  }
                  {
                    key = "O";
                    name = "Open: recursively";
                    type = "command";
                    command = "editor.unfoldRecursively";
                  }
                  {
                    key = "G";
                    name = "Open: all regions";
                    type = "command";
                    command = "editor.unfoldAllMarkerRegions";
                  }
                  {
                    key = "r";
                    name = "Open: all";
                    type = "command";
                    command = "editor.unfoldAll";
                  }
                ];
              }
              {
                key = "!";
                name = "Show terminal";
                type = "command";
                command = "workbench.action.terminal.focus";
              }
              {
                key = "/";
                name = "Search in a project";
                type = "command";
                command = "workbench.action.findInFiles";
              }
            ];
          };

          keybindings = [
            {
              command = "editor.toggleFold";
              key = "z a";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.unfoldAll";
              key = "z shift+r";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.foldAll";
              key = "z shift+m";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.unfold";
              key = "z o";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.unfoldRecursively";
              key = "z shift+o";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.fold";
              key = "z c";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.foldRecursively";
              key = "z shift+c";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.foldAllExcept";
              key = "z shift+v";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.gotoNextFold";
              key = "z j";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              command = "editor.gotoPreviousFold";
              key = "z k";
              when = "editorTextFocus && neovim.mode == normal";
            }
            #  Use single tab
            {
              key = "ctrl+k shift+t";
              command = "workbench.action.showEditorTab";
            }
            # Use multiple tabs
            {
              key = "ctrl+k shift+m";
              command = "workbench.action.showMultipleEditorTabs";
            }
            # Toggle status bar
            {
              key = "ctrl+k shift+s";
              command = "workbench.action.toggleStatusbarVisibility";
            }
            # Toggle record screen cast
            {
              key = "ctrl+k shift+r";
              command = "workbench.action.toggleScreencastMode";
            }
            # Toggle error lens - warning
            {
              key = "ctrl+k shift+w";
              command = "errorLens.toggleWarning";
            }
            # TODO: Wait for this to resolve https://github.com/usernamehw/vscode-error-lens/issues/208
            # Setup which-key
            {
              key = "ctrl+space"; # Disable Spotlight and use Raycast with Alt+space, refer https://manual.raycast.com/hotkey
              command = "whichkey.show";
              when = "editorTextFocus";
            }
            # Toggle full screen
            {
              key = "ctrl+k f";
              command = "workbench.action.toggleMaximizedPanel";
            }
            # Open Github Pull Request
            {
              key = "ctrl+k g";
              command = "workbench.view.extension.github-pull-requests";
            }
            # Toggle version lens
            {
              key = "ctrl+k shift+v";
              command = "versionlens.icons.showVersionLenses";
            }
            # Zen mode
            {
              key = "ctrl+k c";
              command = "workbench.action.toggleCenteredLayout";
            }
            # Chat on terminal
            {
              key = "ctrl+shift+k";
              command = "cursorai.action.generateInTerminal";
              when = "terminalFocus && terminalHasBeenCreated || terminalFocus && terminalProcessSupported";
            }
            {
              key = "ctrl+k";
              command = "-cursorai.action.generateInTerminal";
              when = "terminalFocus && terminalHasBeenCreated || terminalFocus && terminalProcessSupported";
            }
            # Example: Remove the default "Go to Line..." binding for Ctrl+G
            {
              key = "ctrl+g";
              command = "-workbench.action.gotoLine";
              # Optional: Add a 'when' clause if you only want to unbind it
              # in specific contexts (e.g., only when editing text)
              # when = "editorTextFocus";
            }
            # Use ctrl+g to escape
            {
              key = "ctrl+g";
              command = "vscode-neovim.escape";
              when = "editorTextFocus && neovim.mode == normal";
            }
            {
              key = "ctrl+g";
              command = "workbench.action.closeQuickOpen";
              when = "inQuickOpen";
            }
            {
              key = "ctrl+g";
              command = "workbench.action.toggleSidebarVisibility";
              when = "explorerViewletVisible"; # Only active when the Explorer is visible
            }
            # Set whichkey root trigger
            {
              key = "space";
              command = "whichkey.show";
              when = "editorTextFocus && neovim.mode == normal";
            }
          ];
        };
      };
    };
  };
}
