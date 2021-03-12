local awful = require('awful')
local gears = require('gears')
local beautiful = require("beautiful")
-- local icons = require('theme.icons')

local tag = _G.tag

local tags = {
  -- {
  --   icon = beautiful.chrome,
  --   type = 'chrome',
  --   defaultApp = 'google-chrome-beta',
  --   screen = 1
  -- },
  {
    icon = beautiful.code_icon,
    type = 'code',
    name = 'code',
    screen = 1
  },
  {
    icon = beautiful.social_icon,
    type = 'social',
    name = 'social',
    screen = 1
  },
  -- {
  --   icon = beautiful.folder,
  --   type = 'files',
  --   defaultApp = 'nautilus',
  --   screen = 1
  -- },
  {
    icon = beautiful.tag_music_icon,
    type = 'music',
    name = 'music',
    screen = 1
  },
  -- {
  --   icon = beautiful.game,
  --   type = 'game',
  --   defaultApp = '',
  --   screen = 1
  -- },
  {
    icon = beautiful.lab_icon,
    type = 'any',
    name = 'misc',
    screen = 1
  }
}

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.top,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,
    awful.layout.suit.max
}

awful.screen.connect_for_each_screen(
    function(s)
        for i, config in pairs(tags) do
            awful.tag.add(i,
              {
                  icon = config.icon,
                  -- icon_only = true,
                  name = config.name,
                  layout = awful.layout.suit.tile,
                  gap_single_client = false,
                  gap = 4,
                  screen = s,
                  selected = i == 1
              }
          )
        end
    end
)

tag.connect_signal(
  'property::layout',
  function(t)
    local currentLayout = awful.tag.getproperty(t, 'layout')
    if (currentLayout == awful.layout.suit.max) then
      t.gap = 0
    else
      t.gap = 4
    end
  end
)
