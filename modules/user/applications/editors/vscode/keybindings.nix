{}: {
  programs.vscode.profiles.default.keybindings = [
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
    # Toggle explorer
    {
      key = "space+f+e";
      command = "workbench.action.toggleSidebarVisibility";
      when = "sideBarVisible";
    }
  ];
}
