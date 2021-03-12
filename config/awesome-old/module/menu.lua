local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local menubar = require("menubar")
-- custom modules
local globals = require("globals")
local registry = require("widgets.registry")

-- environment
local awesome = _G.awesome

-- @module menu
local menu = {}

-- Launcher widget for the menu
--
-- @name launcher
-- luacheck: allow defined
-- local launcher = nil

function menu.init()
    local awesome_menu = {
        { "hotkeys", function() return false, hotkeys_popup.show_help end, beautiful.keyboard_icon},
        { "restart", awesome.restart, beautiful.reboot_icon },
        { "quit", function() registry.emit("exitscreen", "exitscreen::show") end, beautiful.poweroff_icon}
    }

    local main_menu = awful.menu({
        items = {
            { "awesome", awesome_menu, beautiful.home_icon },
            { "firefox", globals.browser, beautiful.firefox_icon },
            { "terminal", globals.terminal, beautiful.terminal_icon },
            { "emacs", globals.editor, beautiful.editor_icon },
            { "slack", globals.slack, beautiful.slack_icon },
            { "files", globals.filemanager, beautiful.files_icon },
            { "search", "rofi -show combi", beautiful.search_icon },
            { "appearance", "lxappearance", beautiful.appearance_icon },
        }
    })

    -- setup signals
    main_menu:connect_signal("menu::show", function()
        main_menu:show()
    end)
    main_menu:connect_signal("menu::hide", function()
        main_menu:hide()
    end)
    main_menu:connect_signal("menu::toggle", function()
        main_menu:toggle()
    end)

    -- add to registry
    registry.add("menu", main_menu)

    awful.widget.launcher({
        image = beautiful.awesome_icon,
        menu = main_menu
    })
    menubar.utils.terminal = globals.terminal -- Set the terminal for applications that require it
end

menu.init()

return menu
