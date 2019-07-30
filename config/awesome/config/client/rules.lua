-- awesome modules
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local helpers = require("helpers")
local globals = require("globals")
local client_keys = require("config.keys.client")
local client_buttons = require("config.client.buttons")

local client = _G.client

-- Hack to work around Spotify being horrible
client.connect_signal("property::class", function(c)
    if c.name == "Spotify" then
        local tag = awful.screen.focused().tags[3]
        c:move_to_tag(tag)
    end
end)

awful.rules.rules = {
    -- All clients will match this rule.
    {
        rule = { },
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = client_keys,
            buttons = client_buttons,
            screen = awful.screen.preferred,
            size_hints_honor = true,
            honor_workarea = true,
            honor_padding = true,
            -- floating = true,
            placement = awful.placement.no_overlap+awful.placement.no_offscreen,
            callback = function(c)
                c.width = globals.screen_width * 0.45
                c.height = globals.screen_height * 0.5
                c.floating = true
                awful.placement.centered(c,{honor_workarea=true})
            end
        }
    },

    -- Add titlebars to normal clients and dialogs
    {
        rule_any = {
            type = { "normal", "dialog" },
            properties = {
                titlebars_enabled = true
            },
        },
    },

    -- Floating clients
    {
        rule_any = {
            instance = {
                "DTA",  -- Firefox addon DownThemAll.
                "copyq",  -- Includes session name in class.
            },
            class = {
                "mpv",
                "Gpick",
                "Lxappearance",
                "Nm-connection-editor",
                "File-roller",
                "fst",
            },
            name = {
                "Event Tester",  -- xev
            },
            role = {
                "AlarmWindow",  -- Thunderbird's calendar.
                "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
            },
            type = {
                "dialog",
            }
        },
        properties = {
            floating = true,
            ontop = false
        }
    },

    -- Centered clients
    {
        rule_any = {
          type = {
              "dialog",
          },
          class = {
              "Steam",
              "discord",
          },
          role = {
              "GtkFileChooserDialog",
              "conversation",
          }
        },
        properties = {},
        callback = function (c)
            awful.placement.centered(c,{honor_workarea=true})
        end
    },

    -- "Switch to tag"
    -- These clients make you switch to their tag when they appear
    {
        rule_any = {
            class = {
            -- "Firefox",
            -- "Chromium-browser",
            -- "qutebrowser",
            },
        },
        properties = { switchtotag = true }
    },

    -- Titlebars ON (explicitly)
    -- Titlebars of these clients will be shown regardless of the theme setting
    -- {
    --     rule_any = {
    --         class = {
    --           "Firefox",
    --           "Emacs",
    --           "Gnome-terminal",
    --           "URxvt",
    --           "Slack"
    --         },
    --         type = {
    --             "dialog",
    --         },
    --         role = {
    --             "conversation",
    --         }
    --     },
    --     properties = {
    --     },
    --     callback = function (c)
    --         awful.titlebar.show(c)
    --     end
    -- },

    -- {{{ Fixed Workflow Window Placement

    -- Firefox
    {
        rule_any = {
            class = {
                "Firefox",
                "firefox"
            }
        },
        properties = {
            floating = true
        },
        callback = function (c)
            c.name = "Firefox"
            -- change the icon
            local icon = gears.surface(beautiful.browser_icon)
            c.icon = icon._native
            -- Prevent changing tabs from changing the tasklist name
            c:connect_signal("property::name", function(_)
                c.name = "Firefox"
            end)
            c:geometry({
                x = dpi(64),
                y = dpi(96),
                width = dpi(1704),
                height = dpi(1357)
            })
        end
    },

    -- Slack
    {
        rule_any = {
            class = {
                "Slack",
                "slack"
            }
        },
        properties = {
            floating = true,
        },
        callback = function (c)
            c.name = "Slack"
            -- change the icon
            local icon = gears.surface(beautiful.slack_icon)
            c.icon = icon._native

            c:geometry({
                x = dpi(64),
                y = dpi(96),
                width = dpi(1712),
                height = dpi(1357)
            })
        end
    },

    -- Emacs
    {
        rule_any = {
            class = {
                "Emacs",
                "emacs"
            }
        },
        properties = {
            floating = true,
        },
        callback = function (c)
            c.name = "Spacemacs"
            -- change the icon
            local icon = gears.surface(beautiful.tasklist_editor_icon)
            c.icon = icon._native

            -- TODO: Figure out a desktop config to prevent this from happening
            -- This does not appear to only be tied to desktop files. It appears
            -- projectile is doing something to change this
            local timer = gears.timer.start_new(2, function()
                  c.icon = icon._native
                  return true
            end)
            c:connect_signal("focus", function()
                  timer:again()
            end)

            c:connect_signal("unfocus", function()
                  timer:stop()
            end)

            c:geometry({
                x = dpi(1836),
                y = dpi(96),
                width = dpi(1720),
                height = dpi(1368)
            })
        end
    },

    -- Terminal
    {
        rule_any = {
            class = {
                "Gnome-terminal",
                "gnome-terminal-server",
                "URxvt"
            }
        },
        -- Exclude the package updater terminal
        except_any = {
            instance = {
                "package-updater",
                "monitoring",
                "cava"
            }
        },
        properties = {
            floating = true,
        },
        callback = function (c)
            c.name = "Terminal"
            -- change the icon
            local icon = gears.surface(beautiful.tasklist_terminal_icon)
            c.icon = icon._native

            c:connect_signal("property::name", function(_)
                c.name = "Terminal"
            end)
            c:geometry({
                x = dpi(1836),
                y = dpi(96),
                width = dpi(1720),
                height = dpi(1368)
            })
        end
    },
    -- }}}
    -- File managers
    {
        rule_any = {
            class = {
                "Nemo",
                "Thunar"
            },
        },
        except_any = {
            type = { "dialog" }
        },
        properties = {
            floating = true,
            width = globals.screen_width * 0.55,
            height = globals.screen_height * 0.75
        }
    },

    -- Rofi configuration
    -- Needed only if option "-normal-window" is used
    {
        rule_any = {
            class = {
                "Rofi",
            },
        },
        properties = {
            skip_taskbar = true,
            floating = true,
            ontop = true,
            sticky = true
        },
        callback = function (c)
            awful.placement.centered(c,{honor_workarea=true})
            if not beautiful.titlebars_imitate_borders then
                awful.titlebar.hide(c)
            end
        end
    },

    -- Screenruler
    {
        rule_any = {
            class = {
                "Screenruler",
            },
        },
        properties = {
            border_width = 0,
            floating = true,
            ontop = true
        },
        callback = function (c)
            awful.titlebar.hide(c)
            awful.placement.centered(c,{honor_workarea=true})
        end
    },

    -- Image viewers
    {
        rule_any = {
            class = {
                "feh",
                "Sxiv",
            },
        -- }, properties = { floating = true },
        },
        properties = {
            floating = true,
            width = globals.screen_width * 0.7,
            height = globals.screen_height * 0.75
        },
        callback = function (c)
            awful.placement.centered(c,{honor_workarea=true})
        end
    },

    ---------------------------------------------
    -- Start application on specific workspace --
    ---------------------------------------------
    -- Browsing
    {
        rule_any = {
            class = {
                "Firefox",
                -- "Gnome-terminal",
                "Emacs",
                -- "Slack"
            },
            except_any = {
                role = { "GtkFileChooserDialog" },
                type = { "dialog" }
            },
        },
        properties = {
            screen = 1,
            tag = "code"
        }
    },

    {
        rule_any = {
            class = {
                -- "Firefox",
                "URxvt",
                "Slack",
                -- "Slack"
            },
            except_any = {
                role = { "GtkFileChooserDialog" },
                type = { "dialog" }
            },
        },
        properties = {
            screen = 1,
            tag = "social"
        },
    },

    -- Media
    {
        rule_any = {
            class = {
                "Spotify"
            }
        },
        properties = {
            screen = 1,
            tag = "music"
        },
        callback = function (c)
            c.name = "Spotify"
            -- change the icon
            local icon = gears.surface(beautiful.spotify_icon)
            c.icon = icon._native

            c:connect_signal("property::name", function(_)
                c.name = "Spotify"
            end)

            c:geometry({
                    x = dpi(930),
                    y = dpi(560),
                    width = dpi(1704),
                    height = dpi(900)
            })

            gears.timer.delayed_call(function()
                c.urgent = false
            end)
        end
    },
    {
        rule = {
            instance = "cava"
        },
        properties = {
            screen = 1,
            tag = "music"
        },
        callback = function (c)
            c.name = "Cava"
            -- change the icon
            local icon = gears.surface(beautiful.tasklist_terminal_icon)
            c.icon = icon._native

            c:connect_signal("property::name", function(_)
                c.name = "Cava"
            end)

            awful.titlebar.hide(c)

            c:geometry({
                x = dpi(930),
                y = dpi(96),
                width = dpi(1712),
                height = dpi(400)
            })

            gears.timer.delayed_call(function()
                c.urgent = false
            end)
        end
    },

    -- Package Updater Terminal
    {
        rule = {
            instance = "package-updater"
        },
        properties = {
            screen = 1,
            tag = _G.mouse.screen.selected_tag
        },
        callback = function(c)
            local icon = gears.surface(beautiful.terminal_icon)
            c.icon = icon._native
            c.name = "Package Updater"
        end
    },

    -- Monitoring UI
    {
        rule = {
            instance = "monitoring"
        },
        properties = {
            tag = "misc"
        },
        callback = function(c)
            c.name = "Monitoring"
            -- change the icon
            local icon = gears.surface(beautiful.tasklist_terminal_icon)
            c.icon = icon._native

            c:connect_signal("property::name", function(_)
                c.name = "Monitoring"
            end)

            c:geometry({
                x = dpi(64),
                y = dpi(96),
                width = dpi(3486),
                height = dpi(1376)
            })
        end
    }

}
