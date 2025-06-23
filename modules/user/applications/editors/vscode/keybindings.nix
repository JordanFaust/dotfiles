{ ... }: {
  programs.vscode.profiles.default.keybindings = [
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
    # {
    #   key = "ctrl+space"; # Disable Spotlight and use Raycast with Alt+space, refer https://manual.raycast.com/hotkey
    #   command = "whichkey.show";
    #   when = "editorTextFocus";
    # }
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

    #
    # Ctrl+G Universal Escape Implementation
    #

    # Example: Remove the default "Go to Line..." binding for Ctrl+G
    {
      key = "ctrl+g";
      command = "-workbench.action.gotoLine";
      # Optional: Add a 'when' clause if you only want to unbind it
      # in specific contexts (e.g., only when editing text)
      # when = "editorTextFocus";
    }
    # Use ctrl+g to escape in Neovim normal mode
    {
      key = "ctrl+g";
      command = "vscode-neovim.escape";
      when = "editorTextFocus && neovim.mode == normal";
    }
    # Close Quick Open
    {
      key = "ctrl+g";
      command = "workbench.action.closeQuickOpen";
      when = "inQuickOpen";
    }
    # Close sidebar when Explorer is visible
    {
      key = "ctrl+g";
      command = "workbench.action.toggleSidebarVisibility";
      # only active when the Explorer is visible and focused
      when = "sideBarVisible";
    }
    # Close sidebar when visible but not focused (universal sidebar closer)
    {
      key = "ctrl+g";
      command = "workbench.action.toggleSidebarVisibility";
      when = "editorTextFocus && sideBarVisible && !sideBarFocus";
    }
    # Close AI chat when in chat context
    {
      key = "ctrl+g";
      command = "aichat.close-sidebar";
      when = "chatIsEnabled && inChat";
    }
    # Close any focused panel (terminal, output, problems, etc.) and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "panelFocus";
    }
    # Close panel when visible but not focused (e.g., test panel visible while editing)
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "editorTextFocus && panelVisible && !panelFocus";
    }
    # Close terminal and return focus to editor
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "terminalFocus";
    }
    # Close search view and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "searchViewletVisible && searchViewletFocus";
    }
    # Close any sidebar view and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "sideBarFocus";
    }
    # Close command palette
    {
      key = "ctrl+g";
      command = "workbench.action.closeQuickOpen";
      when = "commandPaletteFocus";
    }
    # Close any notification and return to editor
    {
      key = "ctrl+g";
      command = "notifications.hideToasts";
      when = "notificationFocus";
    }
    # Close find widget in editor
    {
      key = "ctrl+g";
      command = "closeFindWidget";
      when = "editorFocus && findWidgetVisible";
    }
    # Close peek view (references, definitions, etc.)
    {
      key = "ctrl+g";
      command = "closeReferenceSearch";
      when = "referenceSearchVisible";
    }
    # Close problems view and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "problemsViewFocus";
    }
    # Close output view and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "outputViewFocus";
    }
    # Close test results view and return to editor
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "testResultsViewFocus";
    }

    #
    # Enhanced Focus Management
    #

    # Universal "return to editor" binding
    {
      key = "ctrl+0";
      command = "workbench.action.focusActiveEditorGroup";
    }
    # Quick sidebar toggle that maintains editor focus
    {
      key = "ctrl+k e";
      command = "workbench.action.toggleSidebarVisibility";
    }
    # Focus terminal without toggling
    {
      key = "ctrl+k t";
      command = "workbench.action.terminal.focus";
    }
    # Focus explorer without toggling
    {
      key = "ctrl+k shift+e";
      command = "workbench.view.explorer";
    }

    #
    # Cursor-Specific AI Feature Bindings
    #

    # Cursor Chat - New chat
    {
      key = "ctrl+k c";
      command = "aichat.newchataction";
    }
    # Cursor Composer - Start composer prompt
    {
      key = "ctrl+k i";
      command = "composer.startComposerPrompt";
      when = "editorTextFocus";
    }
    # Cursor Chat - Toggle sidebar
    {
      key = "ctrl+k shift+c";
      command = "aichat.toggle-sidebar";
    }
    # Cursor - Apply AI suggestion (if available)
    {
      key = "ctrl+k a";
      command = "aichat.apply-suggestion";
      when = "editorTextFocus";
    }
    # Cursor - Explain code selection
    {
      key = "ctrl+k x";
      command = "aichat.explain-selection";
      when = "editorHasSelection";
    }
    # Cursor - Generate code from comment
    {
      key = "ctrl+k shift+g";
      command = "aichat.generate-from-comment";
      when = "editorTextFocus";
    }

    # # --- 2. Toggle Cursor Chat Window (<leader> + a + a) ---
    # {
    #   key = "space a a";
    #   command = "workbench.action.cursor.chat.toggle";
    #   when = "editorTextFocus && neovim.mode == normal";
    # }
    #
    # # --- 3. Workbench Navigation (<leader> + w + <key>) ---
    # {
    #   key = "space w l";
    #   command = "workbench.action.navigateRight";
    #   when = "editorTextFocus && neovim.mode == normal";
    # }
    # {
    #   key = "space w h";
    #   command = "workbench.action.navigateLeft";
    #   when = "editorTextFocus && neovim.mode == normal";
    # }
    # {
    #   key = "space w j";
    #   command = "workbench.action.navigateDown";
    #   when = "editorTextFocus && neovim.mode == normal";
    # }
    # {
    #   key = "space w k";
    #   command = "workbench.action.navigateUp";
    #   when = "editorTextFocus && neovim.mode == normal";
    # }
  ];
}
