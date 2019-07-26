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
local time = require("widgets.sidebar.time")
local date = require("widgets.sidebar.date")
local fancydate = require("widgets.sidebar.fancydate")
local weather = require("widgets.shared.weather")
local meetings = require("widgets.sidebar.meetings")
local monitoring = require("widgets.sidebar.monitoring")

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
    local _monitoring = monitoring.new()
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
            -- pad(1),
            layout = wibox.layout.fixed.vertical
        },
        { ----------- MIDDLE GROUP -----------
            {

                widget = wibox.container.background,
                bg = "#ff008000",
                fg = "#ffffff",
                {
                    layout = wibox.layout.fixed.vertical,
                    {
                        widget = wibox.container.margin,
                        margins = 50,
                        {
                            widget = wibox.container.mirror,
                            reflection = { horizontal = true, vertical = false },
                            _monitoring
                        }
                    }
                }
            },
            _meetings,
            layout = wibox.layout.fixed.vertical
        },
        layout = wibox.layout.align.vertical,
    }

    widget:buttons(buttons())

    registry.add("sidebar", widget)

    return widget
end

-- LEAVE FOR NOW
-- function sidebar.create()
--     local widget = wibox({
--         x = 0,
--         y = 0,
--         visible = false,
--         ontop = true,
--         type = "dock"
--     })
--     widget.bg = beautiful.sidebar_bg or beautiful.wibar_bg or "#111111"
--     widget.fg = beautiful.sidebar_fg or beautiful.wibar_fg or "#FFFFFF"
--     widget.opacity = beautiful.sidebar_opacity or 1
--     widget.height = beautiful.sidebar_height or awful.screen.focused().geometry.height
--     widget.width = beautiful.sidebar_width or dpi(300)
--     widget.y = beautiful.sidebar_y or 0
--     local radius = beautiful.sidebar_border_radius or 0
--     widget.x = beautiful.sidebar_x or 0
--     widget.shape = helpers.prrect(radius, false, true, true, false)

--     widget:buttons(buttons())

--     set_signals(widget)
--     -- set_activations(widget)
--     registry.add("sidebar", widget)

--     local _time = time_widget()
--     local _date = date_widget()
--     local _fancydate = fancydate_widget()
--     local _spotify_controls = spotify_controls()
--     local _now_playing = spotify_playing()
--     local _volume = volume_widget()
--     local _cpu = cpu_widget()
--     local _temperature = temperature_widget()
--     local _ram = ram_widget()
--     local _vpn = vpn_widget()
--     local _disk = disk_widget()
--     local _search = search_widget()
--     local _exit = exit_widget()
--     local _weather = weather.sidebar()
--     local _meetings = meetings.widget()

--     -- Item placement
--     widget:setup {
--         { ----------- TOP GROUP -----------
--             pad(1),
--             pad(1),
--             _time,
--             _date,
--             _fancydate,
--             pad(1),
--             _weather,
--             pad(1),
--             layout = wibox.layout.fixed.vertical
--         },
--         { ----------- MIDDLE GROUP -----------
--             -- playerctl_buttons,
--             _spotify_controls,
--             {
--                 -- Put some padding at the left and right edge so that
--                 -- it looks better with extremely long titles/artists
--                 pad(2),
--                 _now_playing,
--                 pad(1),
--                 layout = wibox.layout.align.horizontal,
--             },
--             pad(1),
--             _volume,
--             _cpu,
--             _temperature,
--             _ram,
--             _vpn,
--             _disk,
--             pad(1),
--             pad(1),
--             _meetings,
--             layout = wibox.layout.fixed.vertical
--         },
--         { ----------- BOTTOM GROUP -----------
--             { -- Search and exit screen
--                 {
--                     pad(9),
--                     _search,
--                     pad(5),
--                     _exit,
--                     pad(2),
--                     layout = wibox.layout.fixed.horizontal
--                 },
--                 nil,
--                 layout = wibox.layout.align.horizontal,
--                 expand = "none"
--             },
--             pad(1),
--             layout = wibox.layout.fixed.vertical
--         },
--         layout = wibox.layout.align.vertical,
--         -- expand = "none"
--     }

--     return widget
-- end

return sidebar
