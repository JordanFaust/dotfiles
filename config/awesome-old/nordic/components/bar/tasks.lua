-- awesome modules
local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")

-- @module layout.bar.tasks
local tasks = { mt = {} }

local function buttons()
    return awful.util.table.join(
        awful.button({}, 1, function(c)
            if c == _G.client.focus then
                c.minimized = true
            else
                -- Without this, the following
                -- :isvisible() makes no sense
                c.minimized = false
                if not c:isvisible() and c.first_tag then
                    c.first_tag:view_only()
                end
                -- This will also un-minimize
                -- the client, if needed
                _G.client.focus = c
                c:raise()
            end
        end),
        awful.button({}, 2, function(c)
            c.kill(c)
        end),
        awful.button({}, 4, function()
            awful.client.focus.byidx(1)
        end),
        awful.button({}, 5, function()
            awful.client.focus.byidx(-1)
        end)
    )
end

--- Creates the bar task widget
-- @method new
-- @tparam string args.name The name of the registered parent component
-- @tparam string args.screen The screen the component will be created on
-- @return The bar components task widget
function tasks.new(args)
    return awful.widget.tasklist {
        screen   = args.screen,
        filter   = awful.widget.tasklist.filter.currenttags,
        buttons  = buttons(),
        layout   = {
            spacing_widget = {},
            spacing = dpi(0),
            layout  = wibox.layout.fixed.horizontal
        },
        widget_template = {
            {
                {
                    forced_height = dpi(48),
                    forced_width = dpi(96),
                    id = 'background_role',
                    widget = wibox.container.background,
                    {
                        {
                            {
                                id     = 'clienticon',
                                forced_width = dpi(24),
                                forced_height = dpi(24),
                                widget = awful.widget.clienticon,
                            },
                            top = dpi(12),
                            bottom = dpi(8),
                            left = dpi(12),
                            right = dpi(24),
                            widget  = wibox.container.margin
                        },
                        {
                            {
                                id = 'close',
                                image = beautiful.tasklist_task_close,
                                forced_width = dpi(24),
                                forced_height = dpi(24),
                                widget = wibox.widget.imagebox
                            },
                            top = dpi(12),
                            right = dpi(8),
                            widget = wibox.container.margin,
                        },
                        layout = wibox.layout.align.horizontal,
                    }
                },
                layout = wibox.layout.align.horizontal,
            },
            nil,
            create_callback = function(self, client, index, objects) --luacheck: no unused args
                local icon = self:get_children_by_id('clienticon')[1]
                icon.client = client

                -- Make the exit icon close the client
                local close = self:get_children_by_id('close')[1]
                close:connect_signal("button::release", function(_)
                    client.kill(client)
                end)
            end,
            update_callback = function(self, client, index, objects) --luacheck: no unused args
                -- Tell the bar necessary components need to be redrawn when a task is added or removed
                registry.emit("layout::bar", "redraw::needed")
            end,
            layout = wibox.layout.align.vertical,
        },
    }
end

function tasks.mt:__call(...) --luacheck: no unused args
    return tasks.new(...)
end

return setmetatable(tasks, tasks.mt)
