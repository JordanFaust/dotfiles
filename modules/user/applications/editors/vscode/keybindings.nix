{...}: {
  programs.vscode.profiles.default.keybindings = [
    # ──────────────────────────────────────────────────────────────────────────
    # Panel Toggles
    # ──────────────────────────────────────────────────────────────────────────

    # Ctrl+E — Toggle Explorer (sidebar)
    {
      key = "ctrl+e";
      command = "workbench.action.toggleSidebarVisibility";
    }

    # Ctrl+A — Toggle Agent View (auxiliary bar / right panel)
    {
      key = "ctrl+a";
      command = "workbench.action.toggleAuxiliaryBar";
    }

    # Ctrl+K T — Focus Terminal (opens panel if hidden)
    # Ctrl+K E — Focus Editor
    # Ctrl+K F — Toggle fullscreen terminal panel
    # Ctrl+K A — Toggle Agent Chat (composer pane)
    #
    # Chords are used because sendKeybindingsToShell=true (required for
    # vscode-neovim) forwards bare key events to the shell. Chords always
    # skip the shell regardless of that setting.
    {
      key = "ctrl+k t";
      command = "workbench.action.terminal.focus";
      when = "!terminalFocus";
    }
    {
      key = "ctrl+k t";
      command = "workbench.action.focusActiveEditorGroup";
      when = "terminalFocus";
    }
    {
      key = "ctrl+k e";
      command = "workbench.action.focusActiveEditorGroup";
    }
    {
      key = "ctrl+k f";
      command = "workbench.action.toggleMaximizedPanel";
    }
    {
      key = "ctrl+k a";
      command = "runCommands";
      when = "!auxiliaryBarFocus && agentChatMaximized";
      args.commands = [
        "workbench.action.maximizeChatSize"
        "composer.openAsPane"
      ];
    }
    {
      key = "ctrl+k a";
      command = "composer.openAsPane";
      when = "!auxiliaryBarFocus && !agentChatMaximized";
    }
    {
      key = "ctrl+k a";
      command = "workbench.action.terminal.focus";
      when = "auxiliaryBarFocus";
    }

    # ──────────────────────────────────────────────────────────────────────────
    # Universal Escape — Ctrl+G
    #
    # Emacs-inspired escape hatch: dismiss whatever UI element has focus and
    # return to the editor. Replaces the default "Go to Line..." binding.
    # Each when clause targets a specific surface; VS Code matches the first
    # rule that applies (evaluated bottom-to-top in the full ruleset).
    # ──────────────────────────────────────────────────────────────────────────

    {
      key = "ctrl+g";
      command = "-workbench.action.gotoLine";
    }
    {
      key = "ctrl+g";
      command = "workbench.action.closeQuickOpen";
      when = "inQuickOpen";
    }
    {
      key = "ctrl+g";
      command = "closeFindWidget";
      when = "editorFocus && findWidgetVisible";
    }
    {
      key = "ctrl+g";
      command = "closeReferenceSearch";
      when = "referenceSearchVisible";
    }
    {
      key = "ctrl+g";
      command = "notifications.hideToasts";
      when = "notificationFocus";
    }
    {
      key = "ctrl+g";
      command = "workbench.action.closePanel";
      when = "panelFocus";
    }
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "sideBarFocus";
    }
    {
      key = "ctrl+g";
      command = "workbench.action.focusActiveEditorGroup";
      when = "auxiliaryBarFocus";
    }

    # ──────────────────────────────────────────────────────────────────────────
    # Fuzzy Finders — Code Telescope
    #
    # Global pickers accessible from any focus context. Mirrors leader-key
    # bindings in vscode.lua but available without entering normal mode.
    # ──────────────────────────────────────────────────────────────────────────

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

    # ──────────────────────────────────────────────────────────────────────────
    # Focus Management
    # ──────────────────────────────────────────────────────────────────────────

    # Return to editor from anywhere
    {
      key = "ctrl+0";
      command = "workbench.action.focusActiveEditorGroup";
    }

    # Ctrl+K H/J/K/L — Directional focus navigation
    #
    # Chords bypass sendKeybindingsToShell, so these work from the terminal.
    # H/L navigate horizontally; J/K navigate vertically between editor
    # and terminal when the panel is not maximized.
    #
    #  Ctrl+K H ←                    → Ctrl+K L
    # ┌──────────────────┐    ┌─────────────────────┐
    # │    Terminal /     │    │      AI Chat        │
    # │    Editor         │    │     (Ctrl+K A)      │
    # │   (Ctrl+K T)     │    │                     │
    # └──────────────────┘    └─────────────────────┘
    #
    # When panel is not maximized (editor + terminal both visible):
    #
    #              Ctrl+K K ↑
    #         ┌──────────────────┐
    #         │     Editor       │
    #         ├──────────────────┤
    #         │    Terminal      │
    #         └──────────────────┘
    #              Ctrl+K J ↓
    {
      key = "ctrl+k h";
      command = "workbench.action.terminal.focus";
      when = "panelMaximized";
    }
    {
      key = "ctrl+k h";
      command = "workbench.action.focusActiveEditorGroup";
      when = "!panelMaximized";
    }
    {
      key = "ctrl+k ctrl+h";
      command = "workbench.action.terminal.focus";
      when = "panelMaximized";
    }
    {
      key = "ctrl+k ctrl+h";
      command = "workbench.action.focusActiveEditorGroup";
      when = "!panelMaximized";
    }
    {
      key = "ctrl+k l";
      command = "composer.openAsPane";
    }
    {
      key = "ctrl+k ctrl+l";
      command = "composer.openAsPane";
    }
    {
      key = "ctrl+k k";
      command = "workbench.action.focusActiveEditorGroup";
      when = "!panelMaximized && terminalFocus";
    }
    {
      key = "ctrl+k ctrl+k";
      command = "workbench.action.focusActiveEditorGroup";
      when = "!panelMaximized && terminalFocus";
    }
    {
      key = "ctrl+k j";
      command = "workbench.action.terminal.focus";
      when = "!panelMaximized && editorFocus";
    }
    {
      key = "ctrl+k ctrl+j";
      command = "workbench.action.terminal.focus";
      when = "!panelMaximized && editorFocus";
    }
  ];
}
