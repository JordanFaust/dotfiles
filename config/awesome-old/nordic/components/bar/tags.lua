-- awesome modules
local awful = require('awful')
local wibox = require('wibox')
local beautiful = require('beautiful')
local dpi = beautiful.xresources.apply_dpi

-- custom modules
local mod = require("config.keys.mod")
local nordic = {
    core = require("nordic.core")
}

-- @module layout.bar.tags
local tags = { mt = {} }

local function buttons()
    return awful.util.table.join(
        awful.button({}, 1, function(t)
            t:view_only()
        end),
        awful.button({ mod.superkey }, 1, function(t)
            if _G.client.focus then
                _G.client.focus:move_to_tag(t)
                t:view_only()
            end
        end),
        awful.button({}, 3, awful.tag.viewtoggle),
        awful.button({ mod.superkey }, 3, function(t)
            if _G.client.focus then
                _G.client.focus:toggle_tag(t)
            end
        end),
        awful.button({}, 4, function(t)
            awful.tag.viewprev(t.screen)
        end),
        awful.button({}, 5, function(t)
            awful.tag.viewnext(t.screen)
        end)
    )
end

function tags.new(args)
    return awful.widget.taglist {
        screen  = args.screen,
        filter  = awful.widget.taglist.filter.all,
        buttons = buttons(),
        style   = {},
        layout   = {
            spacing_widget = {},
            layout  = wibox.layout.fixed.horizontal
        },
        widget_template = {
            {
                {
                    {
                        {
                            id     = 'icon_role',
                            forced_width = dpi(24),
                            forced_height = dpi(24),
                            widget = wibox.widget.imagebox,
                        },
                        top = dpi(12),
                        bottom = dpi(8),
                        left = dpi(12),
                        right = dpi(18),
                        widget  = wibox.container.margin
                    },
                    {
                        {
                            id     = 'text_role',
                            widget = wibox.widget.textbox,
                        },
                        top = dpi(9),
                        bottom = dpi(8),
                        widget  = wibox.container.margin
                    },
                    layout = wibox.layout.fixed.horizontal,
                },
                layout = wibox.layout.fixed.horizontal,
            },
            id     = 'background_role',
            forced_height = dpi(48),
            forced_width  = dpi(120),
            widget = wibox.container.background,
            create_callback = function(self, c3, index, objects) --luacheck: no unused args
                -- Add support for hover colors and an index label
                self:connect_signal('mouse::enter', function()
                    if self.bg ~= beautiful.frost_4 then
                        self.backup     = self.bg
                        self.has_backup = true
                    end
                    self.bg = nordic.core.color.lighten(beautiful.frost_4, 20)
                end)
                self:connect_signal('mouse::leave', function()
                    if self.has_backup then self.bg = self.backup end
                end)
                -- Make sure color doesn't switch to old bg on mouse leave
                self:connect_signal('button::release', function()
                    self.backup = nordic.core.color.lighten(beautiful.frost_4, 20)
                end)
            end,
        },
    }
end

function tags.mt:__call(...) --luacheck: no unused args
    return tags.new(...)
end

return setmetatable(tags, tags.mt)
