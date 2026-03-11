{ ... }:
{
  programs.vscode.profiles.default.keybindings = [
    #
    # Ctrl+E — Toggle Explorer (unified sidebar)
    #

    {
      key = "ctrl+e";
      command = "workbench.action.toggleSidebarVisibility";
    }

    #
    # Ctrl+A — Toggle Agent View (auxiliary bar / right panel)
    #

    {
      key = "ctrl+a";
      command = "workbench.action.toggleAuxiliaryBar";
    }

    #
    # Ctrl+G — Universal Escape / Dismiss
    #
    # Emacs-inspired escape hatch: dismiss whatever UI element has focus
    # and return to the editor. Replaces the default "Go to Line..." binding.
    #

    {
      key = "ctrl+g";
      command = "-workbench.action.gotoLine";
    }
    # Dismiss quick open / command palette
    {
      key = "ctrl+g";
      command = "workbench.action.closeQuickOpen";
      when = "inQuickOpen";
    }
    # Close find widget
    {
      key = "ctrl+g";
      command = "closeFindWidget";
      when = "editorFocus && findWidgetVisible";
    }
    # Close peek view (references, definitions)
    {
      key = "ctrl+g";
      command = "closeReferenceSearch";
      when = "referenceSearchVisible";
    }
    # Dismiss notifications
    {
      key = "ctrl+g";
      command = "notifications.hideToasts";
      when = "notificationFocus";
    }
    # Close panel (terminal, output, problems, test results)
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "panelFocus";
    }
    # Return to editor from sidebar
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "sideBarFocus";
    }
    # Return to editor from auxiliary bar (agent panel)
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "auxiliaryBarFocus";
    }

    #
    # Code Telescope — Global pickers (work from any focus context)
    # Mirrors leader-key bindings in vscode.lua but accessible everywhere.
    #

    {
      key = "ctrl+alt+f";
      command = "code-telescope.fuzzy.file";
    }
    {
      key = "ctrl+alt+g";
      command = "code-telescope.fuzzy.wsText";
    }
    {
      key = "ctrl+alt+r";
      command = "code-telescope.fuzzy.recentFiles";
    }
    {
      key = "ctrl+alt+s";
      command = "code-telescope.fuzzy.wsSymbols";
    }
    {
      key = "ctrl+alt+u";
      command = "code-telescope.fuzzy.documentSymbols";
      when = "editorTextFocus";
    }
    {
      key = "ctrl+alt+d";
      command = "code-telescope.fuzzy.diagnostics";
    }
    {
      key = "ctrl+alt+b";
      command = "code-telescope.fuzzy.branch";
    }
    {
      key = "ctrl+alt+l";
      command = "code-telescope.fuzzy.fileText";
      when = "editorTextFocus";
    }

    #
    # Focus Management
    #

    # Return to editor from anywhere
    {
      key = "ctrl+0";
      command = "workbench.action.focusActiveEditorGroup";
    }
    # Focus terminal
    {
      key = "ctrl+k t";
      command = "workbench.action.terminal.focus";
    }
  ];
}
