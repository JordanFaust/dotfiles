-- awesome modules
local awful = require("awful")
local gears = require("gears")
-- local surface = require("gears.surface")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
-- custom modules
local helpers = require("helpers")
local proxy = require("dbus_proxy")

-- @module player
local player = {
    current_artist = nil,
    current_title = nil,
    current_cover_url = nil,
    running = false,
    mt = {}
}

-- Client for the default spotifyd application. This has limited support
-- and uses the application
local function spotify_client()
    return proxy.Proxy:new({
        bus = proxy.Bus.SESSION,
        name = "org.mpris.MediaPlayer2.spotify",
        path = "/org/mpris/MediaPlayer2",
        interface = "org.mpris.MediaPlayer2.spotify"
    })
end

local function create_button(symbol, color, command, id)
    -- Use a signle wibox.widget { ... } to enable external lookups
    -- by ID on the internal widgets. Currently widget:get_children_by_id('role')
    -- does not work across wibox.widget { ... } boundaries.
    local button = wibox.widget {
        id = id,
        {
            id = 'icon_role',
            markup = helpers.colorize_text(symbol, color),
            font = "FiraCode Nerd Font Mono 20",
            align = "center",
            valigin = "center",
            widget = wibox.widget.textbox
        },
        forced_height = dpi(30),
        forced_width = dpi(30),
        widget = wibox.container.background
    }

    button:buttons(gears.table.join(
                       awful.button({}, 1, function() command() end)))

    button:connect_signal("mouse::enter", function()
        local icon = button:get_children_by_id('icon_role')[1]
        icon.markup = helpers.colorize_text(icon.text, beautiful.xforeground)
    end)

    button:connect_signal("mouse::leave", function()
        local icon = button:get_children_by_id('icon_role')[1]
        icon.markup = helpers.colorize_text(icon.text, color)
    end)

    return button
end

function player.new(args) --luacheck: no unused args
    local art = wibox.widget {
        image = gears.filesystem.get_configuration_dir() .. "images/default.png",
      resize = true,
      forced_height = dpi(80),
      forced_width = dpi(80),
      clip_shape = helpers.rrect(beautiful.border_radius - 5),
      widget = wibox.widget.imagebox
  }

  local title_widget = wibox.widget {
      markup = 'Nothing Playing',
      align = 'center',
      valign = 'center',
      ellipsize = 'middle',
      widget = wibox.widget.textbox
  }

  local artist_widget = wibox.widget {
      markup = 'Nothing Playing',
      align = 'center',
      valign = 'center',
      ellipsize = 'middle',
      wrap = 'word_char',
      widget = wibox.widget.textbox
  }

  local slider = wibox.widget {
      forced_height = dpi(5),
      bar_shape = helpers.rrect(beautiful.border_radius),
      shape = helpers.rrect(beautiful.border_radius),
      background_color = beautiful.xcolor0,
      color = {
          type = 'linear',
          from = {0, 0},
          to = {200, 50},
          stops = {{0, beautiful.xcolor0}, {0.75, beautiful.xcolor5}}
      },
      value = 25,
      max_value = 100,
      widget = wibox.widget.progressbar
  }

  local play_command = function()
    local client = spotify_client()
    client:PlayPause()
  end
  local prev_command = function()
    local client = spotify_client()
    -- reset slider on track change
    slider.value = 0
    client:Previous()
  end
  local next_command = function()
    local client = spotify_client()
    -- reset slider on track change
    slider.value = 0
    client:Next()
  end

  local playerctl_play_symbol = create_button("", beautiful.xcolor4, play_command, 'playpause')
  local playerctl_prev_symbol = create_button("玲", beautiful.xcolor4, prev_command, 'previous')
  local playerctl_next_symbol = create_button("怜", beautiful.xcolor4, next_command, 'next')

  local widget = wibox.widget {
      {
          art,
          left = dpi(22),
          top = dpi(17),
          bottom = dpi(17),
          layout = wibox.container.margin
      },
      {
          {
              {
                  {
                      title_widget,
                      artist_widget,
                      layout = wibox.layout.fixed.vertical
                  },
                  top = 10,
                  left = 25,
                  right = 25,
                  widget = wibox.container.margin
              },
              {
                  nil,
                  {
                      playerctl_prev_symbol,
                      playerctl_play_symbol,
                      playerctl_next_symbol,
                      spacing = dpi(40),
                      layout = wibox.layout.fixed.horizontal
                  },
                  nil,
                  expand = "none",
                  layout = wibox.layout.align.horizontal
              },
              {
                  slider,
                  top = dpi(10),
                  left = dpi(25),
                  right = dpi(25),
                  widget = wibox.container.margin
              },
              layout = wibox.layout.align.vertical
          },
          top = dpi(0),
          bottom = dpi(10),
          widget = wibox.container.margin
      },
      layout = wibox.layout.align.horizontal
  }

  _G.awesome.connect_signal("evil::spotifyd::track", function(track)
      -- Update title, artist, and album art
      title_widget:set_markup_silently(
          '<span foreground="' .. beautiful.xcolor5 .. '">' .. track.title .. '</span>')
      artist_widget:set_markup_silently(
          '<span foreground="' .. beautiful.xcolor6 .. '">' .. track.artist .. '</span>')
      art:set_image(gears.surface.load_uncached(track.album_art_path))

      -- Toggle play/pause button if playback status changed
      local playpause = playerctl_play_symbol:get_children_by_id('icon_role')[1]
      if track.playback_status == "Playing" then
          playpause:set_markup_silently(helpers.colorize_text("", beautiful.xcolor4))
      else
          playpause:set_markup_silently(helpers.colorize_text("", beautiful.xcolor4))
      end

      -- Update slider
      slider.value = (track.position /track.length) * 100
  end)

  return widget
end

function player.mt:__call(...) --luacheck: no unused args
  return player.new(...)
end

return setmetatable(player, player.mt)
