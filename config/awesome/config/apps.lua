local awful = require("awful")
local filesystem = require("gears.filesystem")
local helpers = require('helpers')
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

-- @module apps
local apps = {
    default = {
        emacs = {
            name = "Spacemacs",
            command = "emacs",
            tag = 1,
            properties = {}
        },
        firefox = {
            name = "Firefox",
            command = "firefox",
            tag = 1,
            properties = {}
        },
        urxvt = {
            name = "Terminal",
            command = "urxvt -e /usr/bin/zsh",
            tag = 2,
            properties = {}
        },
        slack = {
            name = "Slack",
            command = "slack",
            tag = 2,
            properties = {
                size_hints = {
                    min_width = dpi(1728),
                    max_width = dpi(1728),
                    min_height = dpi(1400),
                    max_height = dpi(1400)
                }
            }
        },
        spotify = {
            name = "Spotify",
            command = "spotify",
            tag = 3,
            properties = {}
        },
        cava = {
            name = "Cava",
            command = "urxvt -name cava -e cava",
            tag = 3,
            properties = {}
        },
        monitoring = {
            name = "Monitoring",
            command = "urxvt -name monitoring -e tmuxinator start monitoring -n monitoring -p /home/jfaust/.dotfiles/config/tmuxinator/monitoring.yaml",
            tag = 4,
            properties = {}
        }
    },
    rofi = {
        command = "rofi -show drun -theme " .. filesystem.get_configuration_dir() .. "/config/rofi.rasi",
    },
    startup = {
        -- Startup Processes
        feh = {
            name = "feh",
            command = "feh --bg-fill " .. filesystem.get_configuration_dir() .. "nightly-nordic.png"
        },
        compton =  {
            name = "compton",
            command = "compton -b --config ~/.config/compton.conf"
        },
        -- xsettingsd = {
        --     name = "xsettingsd",
        --     command = "xsettingsd &"
        -- },
        desktopevents = {
            name = "desktopevents-data",
            command = "nohup /usr/bin/desktopevents-data --file=/tmp/desktopevents/data.json >/dev/null &"
        },
    }
}

function apps:launch_or_focus(app)
    local config = self.default[app]
    local tag = awful.screen.focused().tags[config.tag]

    -- focus tag
    tag:view_only()

    local exists = false
    for _, client in pairs(tag:clients()) do
        if client.name == config.name then
            exists = true
            client:raise()
            client:jump_to()
        end
    end

    if not exists then
        -- awful.spawn.with_shell(config.command)
        awful.spawn(config.command, config.properties)
    end
end

return apps
