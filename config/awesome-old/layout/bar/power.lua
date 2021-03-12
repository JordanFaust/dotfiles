-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local nordic = require("nordic")

-- @module layout.bar.power
local power = { mt = {} }

-- local function popup(widget)
--     local popup = awful.popup {
--         widget = {
--             -- {
--             --     {
--             --         text = "Suspend",
--             --         align = "center",
--             --         valign = "center",
--             --         forced_height = dpi(18),
--             --         forced_width = dpi(200),
--             --         widget = wibox.widget.textbox
--             --     },
--             --     {
--             --         orientation = "horizontal",
--             --         thickness = dpi(1),
--             --         widget = wibox.widget.separator
--             --     },
--             --     {
--             --         text = "Reboot",
--             --         bg = beautiful.xforeground,
--             --         align = "center",
--             --         valign = "center",
--             --         forced_height = dpi(18),
--             --         forced_width = dpi(200),
--             --         widget = wibox.widget.textbox
--             --     },
--             --     {
--             --         orientation = "horizontal",
--             --         thickness = dpi(1),
--             --         widget = wibox.widget.separator
--             --     },
--             --     {
--             --         text = "Shutdown",
--             --         bg = beautiful.xforeground,
--             --         align = "center",
--             --         valign = "center",
--             --         forced_height = dpi(18),
--             --         forced_width = dpi(200),
--             --         widget = wibox.widget.textbox
--             --     },
--             --     layout = wibox.layout.align.vertical
--             -- },
--             -- bg = beautiful.frost_4,
--             -- widget = wibox.container.background
--             {
--                 {
--                     {
--                         {
--                             markup = nordic.util.colorize_text("Suspend", beautiful.snow_storm_3),
--                             widget = wibox.widget.textbox
--                         },
--                         margins = dpi(20),
--                         widget = wibox.container.margin
--                     },
--                     {
--                         {
--                             markup = nordic.util.colorize_text("Restart", beautiful.snow_storm_3),
--                             widget = wibox.widget.textbox
--                         },
--                         margins = dpi(20),
--                         widget = wibox.container.margin
--                     },
--                     {
--                         {
--                             markup = nordic.util.colorize_text("Shutdown", beautiful.snow_storm_3),
--                             widget = wibox.widget.textbox
--                         },
--                         margins = dpi(20),
--                         widget = wibox.container.margin
--                     },
--                     spacing_widget = {
--                         shape = gears.shape.circle,
--                         color = beautiful.polar_night_4,
--                         widget = wibox.widget.separator
--                     },
--                     spacing = 1,
--                     layout = wibox.layout.fixed.vertical
--                 },
--                 bg = beautiful.frost_4,
--                 widget = wibox.container.background
--             },
--             layout = wibox.layout.fixed.vertical
--         },
--         ontop = true,
--         visible = false,
--         maximum_height = dpi(200),
--         maximum_width = dpi(200),
--         preferred_positions = 'bottom',
--         preferred_anchors = 'front'
--     }
--     -- widget:connect_signal("button::release", function()
--     --     gears.debug.dump(widget)
--     --     if popup.visible then
--     --         popup.visible = false
--     --     else
--     --         popup.visible = true
--     --     end
--     --     -- popup.x = dpi(2200)
--     --     -- popup.y = dpi(16) + dpi(48)
--     --     -- gears.debug.dump(popup.current_anchor)
--     --     -- popup:move_next_to(widget, 'bottom')
--     -- end)
--     popup:connect_signal("mouse::leave", function(_)
--         popup.visible = false
--     end)
--     popup:move_next_to(widget, 'bottom')
-- end

local function popup()
    local widget = wibox {
        width = dpi(96),
        height = dpi(166),
        visible = false,
        ontop = true
    }

    widget:setup {
        -- {
            -- {
                {
                    {
                        {
                            markup = nordic.util.colorize_text("Suspend", beautiful.snow_storm_3),
                            widget = wibox.widget.textbox
                        },
                        top = dpi(20),
                        bottom = dpi(20),
                        left = dpi(12),
                        right = dpi(12),
                        widget = wibox.container.margin
                    },
                    id = "suspend",
                    bg = beautiful.frost_4,
                    widget = wibox.container.background,
                },
                {
                    {
                        {
                            markup = nordic.util.colorize_text("Restart", beautiful.snow_storm_3),
                            widget = wibox.widget.textbox
                        },
                        top = dpi(20),
                        bottom = dpi(20),
                        left = dpi(12),
                        right = dpi(12),
                        widget = wibox.container.margin
                    },
                    id = "restart",
                    bg = beautiful.frost_4,
                    widget = wibox.container.background
                },
                {
                    {
                        {
                            markup = nordic.util.colorize_text("Shutdown", beautiful.snow_storm_3),
                            widget = wibox.widget.textbox
                        },
                        top = dpi(20),
                        bottom = dpi(20),
                        left = dpi(12),
                        right = dpi(12),
                        widget = wibox.container.margin
                    },
                    id = "shutdown",
                    bg = beautiful.frost_4,
                    widget = wibox.container.background
                },
        --         spacing_widget = {
        --             shape = gears.shape.circle,
        --             color = beautiful.polar_night_4,
        --             widget = wibox.widget.separator
        --         },
        --         spacing = 1,
        --         layout = wibox.layout.fixed.vertical
        --     },
        --     bg = beautiful.frost_4,
        --     widget = wibox.container.background
        -- },
        layout = wibox.layout.fixed.vertical
    }

    local options = {
        suspend = widget:get_children_by_id("suspend")[1],
        restart = widget:get_children_by_id("restart")[1],
        shutdown = widget:get_children_by_id("shutdown")[1]
    }

    for _, option in pairs(options) do
        option:connect_signal("mouse::enter", function(_)
                                  gears.debug.dump("mouse::enter")
            if option.bg ~= beautiful.frost_4 then
                option.backup     = option.bg
                option.has_backup = true
            end
            option.bg = nordic.color.lighten(beautiful.frost_4, 20)
        end)

        option:connect_signal("mouse::leave", function(_)
            if option.has_backup then option.bg = option.backup end
        end)
    end

    widget:connect_signal("mouse::leave", function(_)
        widget.visible = false
    end)

    return widget
end

local function buttons(popup)
    return gears.table.join(
        awful.button({}, 1, function(widget)
            local x = widget.x
            popup.x = dpi(x) + dpi(234)
            popup.y = 16 + 48 + 2
            popup.visible = not popup.visible
        end)
    )
end

function power.new(args)
    local widget = wibox.widget {
        {

            {
                id = "icon",
                image = args.icon,
                forced_height = dpi(args.size),
                forced_width = dpi(args.size),
                widget = wibox.widget.imagebox
            },
            top = dpi(12),
            bottom = dpi(12),
            left = dpi(36),
            right = dpi(24),
            layout = wibox.container.margin
        },
        bg = beautiful.frost_4,
        forced_width = dpi(96),
        forced_height = dpi(48),
        widget = wibox.container.background
    }

    local popup_widget = popup()

    widget:buttons(buttons(popup_widget))

    -- Add highlight on mouse enter/leave
    widget:connect_signal('mouse::enter', function()
        if widget.bg ~= beautiful.frost_4 then
            widget.backup     = widget.bg
            widget.has_backup = true
        end
        widget.bg = nordic.color.lighten(beautiful.frost_4, 20)
    end)
    widget:connect_signal('mouse::leave', function()
        if widget.has_backup then widget.bg = widget.backup end
    end)

    -- popup(widget)
    -- widget:connect_signal("button::release", function()
    --     popup(widget)
    -- end)

    registry.add("tagbar::power", widget)

    return widget
end

function power.mt:__call(...) --luacheck: no unused args
    return power.new(...)
end

return setmetatable(power, power.mt)
