_: {
  programs.vscode.profiles.default.userSettings = {
    #
    # General Settings
    #

    workbench = {
      settings.editor = "json";
      statusBar.visible = false;
      colorTheme = "Catppuccin Mocha";
      iconTheme = "catppuccin-macchiato";
      sideBar.location = "left";
      fontAliasing = "antialiased";
      view.showQuietly = {
        workbench.panel.output = true;
      };
    };

    #
    # Window Settings
    #

    window = {
      titleBarStyle = "custom";
      customTitleBarVisibility = "never";
      zoomLevel = 1.8;
    };

    #
    # Terminal
    #

    terminal.integrated = {
      fontSize = 15;
      fontWeight = "500";
      fontWeightBold = "700";
      fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
      lineHeight = 1.5;
      sendKeybindingsToShell = true;
      profiles.osx.tmux = {
        path = "tmux";
        args = [
          "new-session"
          "-A"
          "-s"
          "\${workspaceFolderBasename}"
        ];
      };
      defaultProfile.osx = "tmux";
      automationProfile.osx = {
        path = "/bin/zsh";
      };
    };

    #
    # Editor
    #

    editor = {
      # Font
      fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
      codeLensFontFamily = "MonoLisa Variable";
      codeLensFontSize = 12;

      # Inlay hints — smaller than editor text to stay subordinate
      inlayHints = {
        fontFamily = "MonoLisa Variable";
        fontSize = 12;
        enabled = "on";
        padding = true;
      };

      # Inline suggest
      inlineSuggest = {
        fontFamily = "MonoLisa Variable";
        enabled = true;
      };

      minimap.enabled = false;
      semanticHighlighting.enabled = true;
      semanticTokenColorCustomizations.enabled = true;

      # General
      autoIndent = "full";
      cursorBlinking = "solid";
      cursorStyle = "line";
      cursorWidth = 5;
      fontLigatures = true;
      fontSize = 15;
      fontWeight = "500";
      formatOnSave = true;
      insertSpaces = false;
      largeFileOptimizations = false;
      letterSpacing = 0.5;
      lineHeight = 23;
      renderWhitespace = "all";
      suggestSelection = "first";
      tabCompletion = "on";
      wordSeparators = "/\\()\"':,.;<>~!@#$%^&*|+=[]{}`?-";
    };

    # Other font overrides
    scm.inputFontFamily = "MonoLisa Variable";
    chat.editor.fontFamily = "MonoLisa Variable";
    chat.editor.fontSize = 15;
    debug.console.fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
    debug.console.fontSize = 15;
    notebook.output.fontFamily = "MonoLisa Variable";
    markdown.preview.fontFamily = "MonoLisa Variable";

    # Zen Mode
    zenMode = {
      fullScreen = false;
      hideLineNumbers = false;
      centerLayout = false;
      silentNotifications = false;
    };

    #
    # Explorer Settings
    #

    explorer = {
      confirmDelete = false;
      compactFolders = false;
      confirmDragAndDrop = false;
    };
    errorLens = {
      enabledDiagnosticLevels = ["error"];
      excludeBySource = ["cSpell"];
    };

    #
    # Languages
    #

    javascript = {
      validate.enable = false;
      inlayHints = {
        enumMemberValues.enabled = true;
        functionLikeReturnTypes.enabled = true;
        parameterNames.enabled = "literals";
        variableTypes.enabled = false;
      };
      updateImportsOnFileMove.enabled = "always";
    };

    typescript = {
      enablePromptUseWorkspaceTsdk = true;
      inlayHints = {
        enumMemberValues.enabled = true;
        functionLikeReturnTypes.enabled = true;
        parameterNames.enabled = "literals";
        variableTypes.enabled = false;
      };
      referencesCodeLens.enabled = true;
      updateImportsOnFileMove.enabled = "always";
      preferences.preferTypeOnlyAutoImports = true;
      tsdk = "node_modules/typescript/lib";
    };

    #
    # Code Telescope (Telescope-style fuzzy finder)
    #

    codeTelescope = {
      layout.mode = "classic";
      matching.algorithm = "subsequence";
      preview.showLineNumbers = true;
      keybindings.close = "ctrl+g";
      wsFileFinder.textDisplay = "relative";
    };

    # Diff
    diffEditor = {
      codeLens = true;
      hideUnchangedRegions.enabled = true;
    };

    # Git
    git.confirmSync = false;

    #
    # Nix — language server + formatter (alejandra, matching neovim)
    #

    nix = {
      enableLanguageServer = true;
      serverPath = "nil";
      serverSettings.nil.formatting.command = ["alejandra"];
    };

    #
    # Neovim Settings
    #

    extensions.experimental.affinity = {
      "asvetliakov.vscode-neovim" = 1;
    };

    # Exclude Ctrl+E (explorer toggle), Ctrl+A (agent toggle), Ctrl+H/L
    # (directional focus nav) from neovim so VS Code keybindings handle them.
    # Default list: ["a", "b", "d", "e", "f", "h", "i", "j", "o", "r", "t", "u", "w"]
    "vscode-neovim" = {
      compositeKeys = {
        jj.command = "vscode-neovim.escape";
        jk.command = "vscode-neovim.escape";
      };
      ctrlKeysForNormalMode = [
        "b"
        "d"
        "f"
        "i"
        "j"
        "o"
        "r"
        "t"
        "u"
        "w"
      ];
      ctrlKeysForInsertMode = [
        "d"
        "j"
        "o"
        "r"
        "t"
        "u"
        "w"
      ];
    };
  };
}
