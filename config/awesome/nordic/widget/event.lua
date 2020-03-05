-- awesome modules
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- custom modules
local globals = require("globals")
local shape = require("nordic.core.shape")

-- @submodule nordic.widget.event
local event = { mt = {} }

local function buttons(location)
    local command = globals.browser .. " " .. location
    return gears.table.join(
        awful.button({ }, 1, function()
            awful.spawn.easy_async_with_shell(command, function()
                naughty.notify({
                    preset = naughty.config.presets.normal,
                    title = "Meeting",
                    text = location
                })
            end)
        end)
    )
end

--- Create a new meeting event widget.
-- @method new
-- @tparam string args.description The event description
-- @tparam string args.start_time The start time
-- @tparam string args.end_time The end time
-- @tparam string args.location The event location
-- @tparam string args.time_font The font for the event time
-- @tparam string args.description_font The font for the description
-- @tparam[opt=320] int args.width The width of the event
-- @tparam[opt=90] int args.height The height of the event
-- @tparam boolean args.muted Indicates if the event should be muted in color
-- @return The event
function event.new(args)
    local task_width = args.width or dpi(320)
    local task_height = args.width or dpi(90)

    local description = args.description
    local task_starts = args.start_time
    local task_ends = args.end_time
    local location = args.location
    local muted = args.muted
    local time_font = args.time_font or "Fira Mono Medium 14"
    local description_font = args.description_font or "Fira Mono 13"

    local task_bg = beautiful.xcolor7
    local task_fg = beautiful.xcolor8
    local time_fg = args.time_fg or task_fg
    local muted_task_bg = beautiful.xcolor6

    local background = task_bg
    if muted then
        background = muted_task_bg
    end

    local widget = wibox.widget {
        widget = wibox.container.background,
        point = { x = dpi(35), y = task_height * args.count + 5 },
        bg = beautiful.bg_dark,
        forced_width = task_width,
        forced_height = task_height,
        {
            widget = wibox.container.margin,
            left = 17,
            right = 17,
            top = 6,
            bottom = 6,
            {
                id = "event",
                widget = wibox.container.background,
                shape = shape.rrect(4),
                -- bg = cpaint.darken(task_bg, 80),
                bg = background,
                {
                    widget = wibox.container.margin,
                    bottom = 3,
                    {
                        widget = wibox.container.background,
                        shape = shape.rrect(4),
                        bg = background,
                        {
                            layout = wibox.layout.align.horizontal,
                            { -- the textboxes
                                widget = wibox.container.margin,
                                top = 16,
                                bottom = 8,
                                left = 16,
                                {
                                    layout = wibox.layout.flex.vertical,
                                    {
                                        widget = wibox.container.background,
                                        fg = time_fg,
                                        {
                                            widget = wibox.widget.textbox,
                                            font = time_font,
                                            text = task_starts .. " - " .. task_ends,
                                        },
                                    },
                                    {
                                        widget = wibox.container.background,
                                        fg = task_fg,
                                        {
                                            widget = wibox.widget.textbox,
                                            font = description_font,
                                            text = description,
                                        },
                                    },
                                },
                            }
                        },
                    },
                },
            },
        },
    }

    if string.find(location, "http?") or string.find(location, "disney.bluejeans?") then
        widget:buttons(buttons(location))
    end

    return widget
end

function event.mt:__call(...)
    return event.new(...)
end

return setmetatable(event, event.mt)
