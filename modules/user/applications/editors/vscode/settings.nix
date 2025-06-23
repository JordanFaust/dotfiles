{ ... }: {
  programs.vscode.profiles.default.userSettings = {
    #
    # General Settings
    #

    workbench.settings.editor = "json"; # Show setting in json as default
    # Comment below to show status bar
    workbench.statusBar.visible = false;
    # Cursor resolution for activity bar navigation
    # workbench.activityBar.orientation = "veritical";
    # workbench.activityBar.location = "default";

    #
    # Window Settings
    #

    window.titleBarStyle = "native";
    window.customTitleBarVisibility = "never";
    # Increase size of explorer, headers, menus, etc.
    window.zoomLevel = 1.7;

    #
    # Theme
    #
    workbench.colorTheme = "Catppuccin Mocha";
    workbench.iconTheme = "catppuccin-macchiato";

    # we try to make semantic highlighting look good
    editor.semanticHighlighting.enabled = true;
    # make the window's titlebar use the workbench colors
    # window.titleBarStyle = "custom";

    # Set sidebar to the right
    workbench.sideBar.location = "left";

    #
    # Terminal
    #
    terminal.integrated.fontSize = 14;
    terminal.integrated.sendKeybindingsToShell = true;
    # terminal.integrated.minimumContrastRatio = 1;

    #
    # Editor
    #

    # Font
    editor.fontFamily = "MonoLisa Variable";
    editor.inlayHints.fontFamily = "MonoLisa Variable";
    editor.codeLensFontFamily = "MonoLisa Variable";
    editor.inlineSuggest.fontFamily = "MonoLisa Variable";
    terminal.integrated.fontFamily = "MonoLisa Variable";
    scm.inputFontFamily = "MonoLisa Variable";
    chat.editor.fontFamily = "MonoLisa Variable";
    debug.console.fontFamily = "MonoLisa Variable";
    notebook.output.fontFamily = "MonoLisa Variable";
    markdown.preview.fontFamily = "MonoLisa Variable";
    workbench.fontAliasing = "antialiased";

    # Inlay hints
    editor.inlayHints.enabled = "on";
    # editor.inlayHints.fontSize = 1;
    editor.inlayHints.padding = true;
    editor.minimap.enabled = false;

    # General editor settings
    editor.autoIndent = "full";
    editor.cursorBlinking = "solid";
    editor.cursorStyle = "line";
    editor.cursorWidth = 5;
    editor.fontLigatures = true;
    editor.fontSize = 14;
    editor.fontWeight = "bold";
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

    # Go
    # gopls = {
    #   ui.semanticTokens = true;
    # };

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
  };
}
