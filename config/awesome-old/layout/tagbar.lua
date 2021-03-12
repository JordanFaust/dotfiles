local awful = require('awful')
local beautiful = require('beautiful')
local wibox = require('wibox')
local gears = require('gears')
local mat_icon = require('widgets.material.icon')
local dpi = require('beautiful').xresources.apply_dpi
-- custom modules
local registry = require("widgets.registry")
local mod = require("config.keys.mod")
local apps = require("config.apps")
local clickable_container = require("widgets.material.clickable-container")
local packageupdater = require("widgets.packageupdater")
local vpn = require("widgets.vpn")

local button = _G.button

-- @module tagbar
local tagbar = {}

local function create_buttons(buttons, object)
    if buttons then
        local btns = {}
        for _, b in ipairs(buttons) do
          -- Create a proxy button object: it will receive the real
          -- press and release events, and will propagate them to the
          -- button object the user provided, but with the object as
          -- argument.
            local btn = button {modifiers = b.modifiers, button = b.button}
            btn:connect_signal("press", function()
                b:emit_signal("press", object)
            end)
            btn:connect_signal("release", function()
                b:emit_signal("release", object)
            end)

            btns[#btns + 1] = btn
        end

        return btns
    end
end

local function list_update(w, buttons, label, data, objects)
  -- update the widgets, creating them if needed
    w:reset()
    for i, o in ipairs(objects) do
        local cache = data[o]
        local ib, tb, bgb, tbm, ibm, l, bg_clickable
        if cache then
            ib = cache.ib
            tb = cache.tb
            bgb = cache.bgb
            tbm = cache.tbm
            ibm = cache.ibm
        else
            ib = wibox.widget.imagebox()
            tb = wibox.widget.textbox()
            bgb = wibox.container.background()
            tbm = wibox.container.margin(tb, dpi(4), dpi(16))
            ibm = wibox.container.margin(ib, dpi(12), dpi(12), dpi(12), dpi(12))
            l = wibox.layout.fixed.horizontal()
            bg_clickable = clickable_container()

            -- All of this is added in a fixed widget
            l:fill_space(true)
            l:add(ibm)
            -- l:add(tbm)
            bg_clickable:set_widget(l)

            -- And all of this gets a background
            bgb:set_widget(bg_clickable)

            bgb:buttons(create_buttons(buttons, o))

            data[o] = {
                ib = ib,
                tb = tb,
                bgb = bgb,
                tbm = tbm,
                ibm = ibm
            }
        end

        local text, bg, bg_image, icon, args = label(o, tb)
        args = args or {}

        -- The text might be invalid, so use pcall.
        if text == nil or text == '' then
            tbm:set_margins(0)
        else
            if not tb:set_markup_silently(text) then
                tb:set_markup('<i>&lt;Invalid text&gt;</i>')
            end
        end

        bgb:set_bg(bg)
        if type(bg_image) == 'function' then
          -- TODO: Why does this pass nil as an argument?
            bg_image = bg_image(tb, o, nil, objects, i)
        end
        bgb:set_bgimage(bg_image)

        if icon then
            ib.image = icon
        else
            ibm:set_margins(0)
        end

        bgb.shape = args.shape
        bgb.shape_border_width = args.shape_border_width
        bgb.shape_border_color = args.shape_border_color

        w:add(bgb)
    end
end

local function taglist(screen)
    local widget = awful.widget.taglist({
        screen = screen,
        filter = awful.widget.taglist.filter.all,
        buttons = awful.util.table.join(
            awful.button(
                {},
                1,
                function(t)
                    t:view_only()
                end
            ),
            awful.button(
                {mod.superkey},
                1,
                function(t)
                    if _G.client.focus then
                        _G.client.focus:move_to_tag(t)
                        t:view_only()
                    end
                end
            ),
            awful.button({}, 3, awful.tag.viewtoggle),
            awful.button(
                {mod.superkey},
                3,
                function(t)
                    if _G.client.focus then
                        _G.client.focus:toggle_tag(t)
                    end
                end
            ),
            awful.button(
                {},
                4,
                function(t)
                    awful.tag.viewprev(t.screen)
                end
            ),
            awful.button(
                {},
                5,
                function(t)
                    awful.tag.viewnext(t.screen)
                end
            )
        ),
        update_function = list_update,
        layout = wibox.layout.fixed.vertical
    })
    return widget
end

local function actionbar(screen, panel, action_bar_width)
  -- Clock / Calendar 24h format
  local textclock = wibox.widget.textclock('<span font="Roboto Mono bold 11">%H\n%M</span>')

  local clock_widget = wibox.container.margin(textclock, dpi(13), dpi(13), dpi(8), dpi(8))
  local _packageupdater = packageupdater.create()
  local _vpn = vpn.create()
  local systray = wibox.widget.systray()
  systray:set_horizontal(false)

  local menu_icon =
    wibox.widget {
    icon = beautiful.menu_icon,
    size = dpi(24),
    widget = mat_icon
  }

  local home_button = wibox.widget {
    wibox.widget {
      menu_icon,
      widget = clickable_container
    },
    bg = beautiful.xbackground,
    widget = wibox.container.background
  }

  home_button:buttons(
    gears.table.join(
      awful.button(
        {},
        1,
        nil,
        function()
            registry.emit("dashboard", "dashboard::toggle", { search = false })
          -- panel:toggle()
        end
      )
    )
  )

  panel:connect_signal("dashboard::opened", function()
      menu_icon.icon = beautiful.close_icon
  end)

  panel:connect_signal("dashboard::closed", function()
      menu_icon.icon = beautiful.menu_icon
  end)

  local widget = wibox.widget {
    id = 'action_bar',
    layout = wibox.layout.align.vertical,
    forced_width = action_bar_width,
    {
      -- Left widgets
        layout = wibox.layout.fixed.vertical,
        home_button,
        -- Create a taglist widget
        taglist(screen)
    },
    --s.mytasklist, -- Middle widget
    nil,
    {
      -- Right widgets
        layout = wibox.layout.fixed.vertical,
        -- wibox.container.margin(systray, dpi(10), dpi(10)),
        -- require('widget.package-updater'),
        _vpn,
        _packageupdater,
        -- require('widget.wifi'),
        -- require('widget.battery'),
        -- Clock
        clock_widget
    }
  }
  return widget
end

function tagbar.create(screen, sidebar)
    local action_bar_width = dpi(48)
    local panel_content_width = dpi(400)

    local panel = wibox {
        screen = screen,
        width = action_bar_width,
        height = screen.geometry.height,
        x = screen.geometry.x,
        y = screen.geometry.y,
        ontop = true,
        bg = beautiful.xbackground,
        fg = beautiful.xforeground
    }

    panel.opened = false

    panel:struts({
        left = action_bar_width
    })

    local backdrop = wibox {
        ontop = true,
        screen = screen,
        bg = beautiful.xbackground,
        type = 'dock',
        x = screen.geometry.x,
        y = screen.geometry.y,
        width = action_bar_width,
        height = screen.geometry.height
    }

    panel:connect_signal("dashboard::search", function()
        _G.awesome.spawn(
            apps.rofi.command,
            false,
            false,
            false,
            false,
            function()
                registry.emit("dashboard", "dashboard::toggle")
            end
        )
    end)

    panel:connect_signal("dashboard::open", function(self, config)
        self.opened = true
        self.width = action_bar_width + panel_content_width
        backdrop.visible = true
        -- toggle visibility to force panel to the top
        self.visible = false
        self.visible = true
        self:get_children_by_id("panel_content")[1].visible = true
        if config.search then
            self:emit_signal("dashboard::search")
        end
        self:emit_signal("dashboard::opened")
    end)

    panel:connect_signal("dashboard::close", function(self)
        self.opened = false
        self.width = action_bar_width
        self:get_children_by_id("panel_content")[1].visible = false
        backdrop.visible = false
        self:emit_signal("dashboard::closed")
    end)

    panel:connect_signal("dashboard::toggle", function(self, config)
        self.opened = not self.opened
        if self.opened then
            self:emit_signal("dashboard::open", config)
        else
            self:emit_signal("dashboard::close")
        end
    end)

    backdrop:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                registry.emit("dashboard", "dashboard::toggle", { search = false })
            end)
        )
    )

    registry.add("dashboard", panel)

    panel:setup {
        {
            id = 'panel_content',
            bg = beautiful.xbackground,
            widget = wibox.container.background,
            visible = false,
            forced_width = panel_content_width,
            {
                sidebar,
                layout = wibox.layout.stack
            }
        },
        actionbar(screen, panel, action_bar_width),
        layout = wibox.layout.align.horizontal,
    }
    return panel
end

return tagbar
