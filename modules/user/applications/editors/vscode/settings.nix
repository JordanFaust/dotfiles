{ ... }: {
  programs.vscode.profiles.default.userSettings = {
    #
    # General Settings
    #

    workbench.settings.editor = "json";
    workbench.statusBar.visible = false;

    #
    # Window Settings
    #

    window.titleBarStyle = "custom";
    window.customTitleBarVisibility = "never";
    window.zoomLevel = 1.7;

    #
    # Theme
    #

    workbench.colorTheme = "Catppuccin Mocha";
    workbench.iconTheme = "catppuccin-macchiato";
    editor.semanticHighlighting.enabled = true;
    workbench.sideBar.location = "left";

    #
    # Terminal
    #

    terminal.integrated.fontSize = 15;
    terminal.integrated.fontWeight = "500";
    terminal.integrated.fontWeightBold = "700";
    terminal.integrated.lineHeight = 1.5;
    terminal.integrated.sendKeybindingsToShell = true;

    #
    # Editor
    #

    # Font
    editor.fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
    editor.inlayHints.fontFamily = "MonoLisa Variable";
    editor.codeLensFontFamily = "MonoLisa Variable";
    editor.inlineSuggest.fontFamily = "MonoLisa Variable";
    terminal.integrated.fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
    scm.inputFontFamily = "MonoLisa Variable";
    chat.editor.fontFamily = "MonoLisa Variable";
    debug.console.fontFamily = "MonoLisa Variable, Symbols Nerd Font Mono";
    notebook.output.fontFamily = "MonoLisa Variable";
    markdown.preview.fontFamily = "MonoLisa Variable";
    workbench.fontAliasing = "antialiased";

    # Inlay hints
    editor.inlayHints.enabled = "on";
    editor.inlayHints.padding = true;
    editor.minimap.enabled = false;

    # General editor settings
    editor.autoIndent = "full";
    editor.cursorBlinking = "solid";
    editor.cursorStyle = "line";
    editor.cursorWidth = 5;
    editor.fontLigatures = true;
    editor.fontSize = 15;
    editor.fontWeight = "500";
    editor.formatOnSave = true;
    editor.inlineSuggest.enabled = true;
    editor.insertSpaces = false;
    editor.largeFileOptimizations = false;
    editor.letterSpacing = 0.5;
    editor.lineHeight = 23;
    editor.renderWhitespace = "all";
    editor.suggestSelection = "first";
    editor.tabCompletion = "on";
    editor.wordSeparators = "/\\()\"':,.;<>~!@#$%^&*|+=[]{}`?-";
    editor.semanticTokenColorCustomizations = {
      enabled = true;
    };
    workbench.view.showQuietly = {
      workbench.panel.output = true;
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

    # Javascript / Typescript
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
    typescript.tsdk = "node_modules/typescript/lib";

    #
    # Code Telescope (Telescope-style fuzzy finder)
    #

    codeTelescope.layout.mode = "classic";
    codeTelescope.matching.algorithm = "subsequence";
    codeTelescope.preview.showLineNumbers = true;
    codeTelescope.keybindings.close = "ctrl+g";
    codeTelescope.wsFileFinder.textDisplay = "relative";

    # Diff
    diffEditor.codeLens = true;
    diffEditor.hideUnchangedRegions.enabled = true;

    # Git
    git.confirmSync = false;

    #
    # Neovim Settings
    #

    extensions.experimental.affinity = {
      "asvetliakov.vscode-neovim" = 1;
    };

    vscode-neovim.compositeKeys = {
      jj = {
        command = "vscode-neovim.escape";
      };
      jk = {
        command = "vscode-neovim.escape";
      };
    };

    # Exclude Ctrl+E (explorer toggle), Ctrl+A (agent toggle), Ctrl+H/L
    # (directional focus nav) from neovim so VS Code keybindings handle them.
    # Default list: ["a", "b", "d", "e", "f", "h", "i", "j", "o", "r", "t", "u", "w"]
    vscode-neovim.ctrlKeysForNormalMode = ["b" "d" "f" "i" "j" "o" "r" "t" "u" "w"];
    vscode-neovim.ctrlKeysForInsertMode = ["d" "j" "o" "r" "t" "u" "w"];
  };
}
