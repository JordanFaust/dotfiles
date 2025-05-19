{ ... }: {
  programs.vscode.profiles.default.userSettings = {
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

    # Set sidebar to the right
    workbench.sideBar.location = "left";
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
    terminal.integrated.fontSize = 14;
    terminal.integrated.sendKeybindingsToShell = true;

    #
    # Editor
    #

    # Font
    editor.fontFamily = "MonoLisa Variable, OperatorMonoLig Nerd Font, JetBrainsMono Nerd Font Mono, Menlo, Monaco, 'Courier New', monospacere";
    editor.inlayHints.fontFamily = "MonoLisa Variable Medium";
    editor.codeLensFontFamily = "MonoLisa Variable Medium";
    terminal.integrated.fontFamily = "MonoLisa Variable Medium";
    scm.inputFontFamily = "MonoLisa Variable Medium";
    chat.editor.fontFamily = "MonoLisa Variable Medium";
    debug.console.fontFamily = "MonoLisa Variable Medium";
    notebook.output.fontFamily = "MonoLisa Variable Medium";
    markdown.preview.fontFamily = "MonoLisa Variable Medium";

    # Inlay hints
    editor.inlayHints.enabled = "on";
    editor.inlayHints.fontSize = 1;
    editor.inlayHints.padding = true;
    editor.minimap.enabled = false;

    # General editor settings
    editor.autoIndent = "full";
    editor.cursorBlinking = "solid";
    editor.cursorStyle = "line";
    editor.cursorWidth = 5;
    editor.fontLigatures = true;
    editor.fontSize = 14;
    editor.fontWeight = "400";
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
    whichkey.delay = 400;
    whichkey.bindings = [
      # {
      #   key = " ";
      #   name = "Commands";
      #   type = "command";
      #   command = "workbench.action.showCommands";
      # }
      {
        key = " ";
        name = "Last editor";
        type = "command";
        commands = [
          "workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup"
          "list.select"
        ];
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
}
