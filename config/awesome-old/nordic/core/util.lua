-- awesome modules
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require("wibox")
local naughty = require("naughty")

-- @submodule nordic.util
local util = {}

-- environment
-- luacheck: globals client
local client = _G.client

function util.colorize_text(txt, fg)
    return "<span foreground='" .. fg .."'>" .. txt .. "</span>"
end

function util.strikethrough_text(text)
    return "<strike>" .. text .. "</strike>"
end

function util.client_menu_toggle()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = dpi(250) } })
        end
    end
end

function util.pad(size)
    local str = ""
    for _ = 1, size do
        str = str .. " "
    end
    local pad = wibox.widget.textbox(str)
    return pad
end

function util.move_to_edge(c, direction)
    local workarea = awful.screen.focused().workarea
    local client_geometry = c:geometry()
    if direction == "up" then
        c:geometry({
            nil,
            y = workarea.y + beautiful.screen_margin * 2,
            nil,
            nil
        })
    elseif direction == "down" then
        local padding = beautiful.screen_margin * 2 - beautiful.border_width * 2
        c:geometry({
            nil,
            y = workarea.height + workarea.y - client_geometry.height - padding,
            nil,
            nil
        })
    elseif direction == "left" then
        c:geometry({
            x = workarea.x + beautiful.screen_margin * 2,
            nil,
            nil,
            nil
        })
    elseif direction == "right" then
        local padding = beautiful.screen_margin * 2 - beautiful.border_width * 2
        c:geometry({
            x = workarea.width + workarea.x - client_geometry.width - padding,
            nil,
            nil,
            nil
        })
    end
end

function util.create_titlebar(c, titlebar_buttons, titlebar_position, titlebar_size)
    awful.titlebar(c, {font = beautiful.titlebar_font, position = titlebar_position, size = titlebar_size}) : setup {
        {
            buttons = titlebar_buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        {
            buttons = titlebar_buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        {
            buttons = titlebar_buttons,
            layout = wibox.layout.fixed.horizontal
        },
        layout = wibox.layout.align.horizontal
    }
end


local double_tap_timer = nil
function util.single_double_tap(single_tap_function, double_tap_function)
  if double_tap_timer then
    double_tap_timer:stop()
    double_tap_timer = nil
    double_tap_function()
    -- naughty.notify({text = "We got a double tap"})
    return
  end

  double_tap_timer =
    gears.timer.start_new(0.20, function()
                            double_tap_timer = nil
                            -- naughty.notify({text = "We got a single tap"})
                            single_tap_function()
                            return false
    end)
end

function util.toggle_scratchpad()
    local screen = awful.screen.focused()

    -- Get rid of it if it is focused
    if client.focus ~= nil and client.focus.class == "scratchpad" then
        -- 1. Minimize scratchpad - Does not work?
        -- client.focus.minimized = true

        -- 2. Move scratchpad to "Miscellaneous" tag
        local tag = screen.tags[10]
        if tag then
            client.focus:move_to_tag(tag)
        end
        return
    end

    -- Move scratchpad to current tag
    local current_tag = screen.selected_tag
    local scratchpad_client = function (c)
      return awful.rules.match(c, {class = "scratchpad"})
    end
    for c in awful.client.iterate(scratchpad_client) do
      c.minimized = false
      c:move_to_tag(current_tag)
      client.focus = c
      c:raise()
    end

    -- if client.focus ~= nil and client.focus.class == "scratchpad" then
    --     client.focus.minimized = true
    --     return
    -- end
    -- local matcher = function (c)
    --     return awful.rules.match(c, {class = 'scratchpad'})
    -- end
    -- awful.client.run_or_raise( "scratchpad" , matcher)
end

-- Add a clickable effect on a widget by changing the cursor on mouse::enter and mouse::leave
function util.add_clickable_effect(w)
    local original_cursor = "left_ptr"
    local hover_cursor = "hand1"

    w:connect_signal("mouse::enter", function ()
        w = _G.mouse.current_wibox
        if w then
            w.cursor = hover_cursor
        end
    end)

    w:connect_signal("mouse::leave", function ()
        w = _G.mouse.current_wibox
        if w then
            w.cursor = original_cursor
        end
    end)
end

--- Display a message using a naughty notification.
--
-- @param message the message to display.
function util.debug(message, config)
    if config then
        naughty.notify{
            title = "DEBUG",
            text = message,
            timeout = 5,
            font = config.font
        }
        return
    end

    naughty.notify{
        title = "DEBUG",
        text = tostring(message),
        timeout = 10
    }
end

function util.log(message)
    gears.debug.dump(os.date("%Y-%m-%d %T ") .. message)
end

return util
