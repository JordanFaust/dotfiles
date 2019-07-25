local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local helpers = require("helpers")
local mat_icon = require("widgets.material.icon")
local mat_list_item = require('widgets.material.list-item')
local pad = helpers.pad
local registry = require("widgets.registry")
local exit = require("widgets.sidebar.exit")
local temperature = require("widgets.sidebar.temperature")
local volume = require("widgets.sidebar.volume")
local cpu = require("widgets.sidebar.cpu")
local ram = require("widgets.sidebar.ram")
local time = require("widgets.sidebar.time")
local date = require("widgets.sidebar.date")
local fancydate = require("widgets.sidebar.fancydate")
local spotify = require("widgets.shared.spotify")
local disk = require("widgets.sidebar.disk")
local search = require("widgets.sidebar.search")
local vpn = require("widgets.sidebar.vpn")
local weather = require("widgets.shared.weather")
local meetings = require("widgets.sidebar.meetings")

-- @module sidebar
local sidebar = {}

-- Common configuration across all sidebar components
-- @name config
local config = {
    -- common progress bar properties
    progressbar = {
        -- height of the bar
        forced_height = dpi(9),
        -- forced width of the bar
        forced_width = dpi(250),
        -- max value of bar value
        max = 100,
        -- default to 50 until data source provides updated value
        value = 50,
        -- margins
        margins = {
            top = dpi(8),
            botton = dpi(8)
        },
    },
    widget = {
        forced_width = dpi(265)
    },
    icon = {
        size = dpi(36)
    },
    spotify = {
        button_size = dpi(48)
    }
}

local function searchbar_widget()
    local widget = wibox.widget {
        wibox.widget {
            icon = beautiful.magnify_icon,
            size = dpi(24),
            widget = mat_icon
        },
        wibox.widget {
            text = 'Search Applications',
            font = 'Roboto medium 13',
            widget = wibox.widget.textbox
        },
        clickable = true,
        widget = mat_list_item
    }

    widget:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                registry.emit("dashboard", "dashboard::search")
            end)
        )
    )

    return widget
end

local function time_widget()
    local overrides = {
        time = {
            font = "sans 60"
        }
    }
    return time.widget(config, overrides)
end

local function date_widget()
    local overrides = {
        date = {
            font = "sans medium 22"
        }
    }
    return date.widget(config, overrides)
end

local function fancydate_widget()
    local overrides = {
        fancydate = {
            font = "sans 14"
        }
    }
    return fancydate.widget(config, overrides)
end

local function spotify_playing()
    local overrides = {
        icon = {
            size = dpi(48),
        },
        spotify = {
            font = "sans 14"
        }
    }
    return spotify.now_playing(config, overrides)
end

local function spotify_controls()
    local overrides = {
        icon = {
            size = dpi(48),
            toggle = {
                icon = beautiful.playerctl_toggle_icon
            },
            previous = {
                icon = beautiful.playerctl_prev_icon
            },
            nxt = {
                icon = beautiful.playerctl_next_icon
            }
        }
    }
    return spotify.controls(config, overrides)
end

local function volume_widget()
    local overrides = {
        progressbar = {
            color = beautiful.volume_bar_active_color or "#5AA3CC",
            muted_color = beautiful.volume_bar_muted_color or "#666666",
            background = beautiful.volume_bar_active_background_color or "#222222",
            muted_background = beautiful.volume_bar_muted_background_color or "#222222"
        },
        icon = {
            image = beautiful.volume_icon
        }
    }
    return volume.widget(config, overrides)
end

local function temperature_widget()
    local overrides = {
        progressbar = {
            color = beautiful.temperature_bar_active_color or "#5AA3CC",
            background = beautiful.temperature_bar_background_color or "#222222",
        },
        icon = {
            image = beautiful.temperature_icon
        }
    }
    return temperature.widget(config, overrides)
end

local function cpu_widget()
    local overrides = {
        progressbar = {
            color = beautiful.cpu_bar_active_color or "#5AA3CC",
            background = beautiful.cpu_bar_background_color or "#222222"
        },
        icon = {
            image = beautiful.cpu_icon
        }
    }
    return cpu.widget(config, overrides)
end

local function ram_widget()
    local overrides = {
        progressbar = {
            color = beautiful.ram_bar_active_color or "#5AA3CC",
            background = beautiful.ram_bar_background_color or "#222222"
        },
        icon = {
            image = beautiful.ram_icon
        }
    }

    return ram.widget(config, overrides)
end

local function vpn_widget()
    local overrides = {
        progressbar = {
            color = beautiful.vpn_bar_active_color or "#E49186",
            background = beautiful.vpn_bar_background_color or "#222222",
            value = 0
        },
        icon = {
            image = beautiful.vpn_icon,
        }
    }
    return vpn.widget(config, overrides)
end

local function disk_widget()
    local overrides = {
        progressbar = {
            color = beautiful.disk_bar_active_color or "#E49186",
            background = beautiful.disk_bar_background_color or "#222222",
            value = 0
        },
        icon = {
            image = beautiful.files_icon
        }
    }
    return disk.widget(config, overrides)
end

local function search_widget()
    local overrides = {
        search = {
            font = "sans 14"
        },
        icon = {
            image = beautiful.search_icon
        }
    }
    return search.widget(config, overrides)
end

local function exit_widget()
    local widget = exit.widget()

    return widget
end

local function buttons()
    return gears.table.join(
        awful.button({ }, 2, function ()
            sidebar.visible = false
        end)
    )
end

local function set_signals(widget)
    widget:connect_signal("mouse::leave", function()
        widget.visible = false
    end)
    widget:connect_signal("sidebar::hide", function()
        widget.visible = false
    end)
    widget:connect_signal("sidebar::show", function()
        widget.visible = true
    end)
    widget:connect_signal("sidebar::search", function()
        _G.awesome.spawn(
            "rofi-apps",
            false,
            false,
            false,
            false,
            function()
                registry.emit("sidebar", "sidebar::hide-content")
                -- panel:toggle()
            end
        )
    end)
end

local function set_activations(widget)
    if beautiful.sidebar_hide_on_mouse_leave then
        local activator = wibox({y = widget.y, width = 1, visible = true, ontop = false, opacity = 0, below = true})
        activator.height = widget.height
        -- sidebar_activator.height = sidebar.height - beautiful.wibar_height
        activator:connect_signal("mouse::enter", function ()
            widget.visible = true
        end)

        if beautiful.sidebar_position == "right" then
          activator.x = awful.screen.focused().geometry.width - activator.width
        else
          activator.x = 0
        end

        activator:buttons(
          gears.table.join(
            awful.button({ }, 4, function ()
                awful.tag.viewprev()
            end),
            awful.button({ }, 5, function ()
                awful.tag.viewnext()
            end)
        ))
    end
end

function sidebar.dashboard()
    local _searchbar = searchbar_widget()
    local _time = time_widget()
    local _date = date_widget()
    local _fancydate = fancydate_widget()
    local _spotify_controls = spotify_controls()
    local _now_playing = spotify_playing()
    local _volume = volume_widget()
    local _cpu = cpu_widget()
    local _temperature = temperature_widget()
    local _ram = ram_widget()
    local _vpn = vpn_widget()
    local _disk = disk_widget()
    -- local _search = search_widget()
    -- local _exit = exit_widget()
    local _weather = weather.sidebar()
    local _meetings = meetings.widget()

    -- Item placement
    local widget = wibox.widget {
        { ----------- TOP GROUP -----------
            _searchbar,
            pad(1),
            pad(1),
            _time,
            _date,
            _fancydate,
            pad(1),
            _weather,
            pad(1),
            layout = wibox.layout.fixed.vertical
        },
        { ----------- MIDDLE GROUP -----------
            -- playerctl_buttons,
            _spotify_controls,
            {
                -- Put some padding at the left and right edge so that
                -- it looks better with extremely long titles/artists
                pad(2),
                _now_playing,
                pad(1),
                layout = wibox.layout.align.horizontal,
            },
            pad(1),
            _volume,
            _cpu,
            _temperature,
            _ram,
            _vpn,
            _disk,
            pad(1),
            pad(1),
            _meetings,
            layout = wibox.layout.fixed.vertical
        },
        { ----------- BOTTOM GROUP -----------
            { -- Search and exit screen
                {
                    pad(9),
                    -- _search,
                    pad(5),
                    -- _exit,
                    pad(2),
                    layout = wibox.layout.fixed.horizontal
                },
                nil,
                layout = wibox.layout.align.horizontal,
                expand = "none"
            },
            pad(1),
            layout = wibox.layout.fixed.vertical
        },
        layout = wibox.layout.align.vertical,
        -- expand = "none"
    }

    widget:buttons(buttons())

    -- set_signals(widget)
    -- set_activations(widget)
    registry.add("sidebar", widget)

    return widget
end

function sidebar.create()
    local widget = wibox({
        x = 0,
        y = 0,
        visible = false,
        ontop = true,
        type = "dock"
    })
    widget.bg = beautiful.sidebar_bg or beautiful.wibar_bg or "#111111"
    widget.fg = beautiful.sidebar_fg or beautiful.wibar_fg or "#FFFFFF"
    widget.opacity = beautiful.sidebar_opacity or 1
    widget.height = beautiful.sidebar_height or awful.screen.focused().geometry.height
    widget.width = beautiful.sidebar_width or dpi(300)
    widget.y = beautiful.sidebar_y or 0
    local radius = beautiful.sidebar_border_radius or 0
    widget.x = beautiful.sidebar_x or 0
    widget.shape = helpers.prrect(radius, false, true, true, false)

    widget:buttons(buttons())

    set_signals(widget)
    -- set_activations(widget)
    registry.add("sidebar", widget)

    local _time = time_widget()
    local _date = date_widget()
    local _fancydate = fancydate_widget()
    local _spotify_controls = spotify_controls()
    local _now_playing = spotify_playing()
    local _volume = volume_widget()
    local _cpu = cpu_widget()
    local _temperature = temperature_widget()
    local _ram = ram_widget()
    local _vpn = vpn_widget()
    local _disk = disk_widget()
    local _search = search_widget()
    local _exit = exit_widget()
    local _weather = weather.sidebar()
    local _meetings = meetings.widget()

    -- Item placement
    widget:setup {
        { ----------- TOP GROUP -----------
            pad(1),
            pad(1),
            _time,
            _date,
            _fancydate,
            pad(1),
            _weather,
            pad(1),
            layout = wibox.layout.fixed.vertical
        },
        { ----------- MIDDLE GROUP -----------
            -- playerctl_buttons,
            _spotify_controls,
            {
                -- Put some padding at the left and right edge so that
                -- it looks better with extremely long titles/artists
                pad(2),
                _now_playing,
                pad(1),
                layout = wibox.layout.align.horizontal,
            },
            pad(1),
            _volume,
            _cpu,
            _temperature,
            _ram,
            _vpn,
            _disk,
            pad(1),
            pad(1),
            _meetings,
            layout = wibox.layout.fixed.vertical
        },
        { ----------- BOTTOM GROUP -----------
            { -- Search and exit screen
                {
                    pad(9),
                    _search,
                    pad(5),
                    _exit,
                    pad(2),
                    layout = wibox.layout.fixed.horizontal
                },
                nil,
                layout = wibox.layout.align.horizontal,
                expand = "none"
            },
            pad(1),
            layout = wibox.layout.fixed.vertical
        },
        layout = wibox.layout.align.vertical,
        -- expand = "none"
    }

    return widget
end

return sidebar
