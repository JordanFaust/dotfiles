local awful = require('awful')
local gears = require('gears')
local beautiful = require("beautiful")
-- local icons = require('theme.icons')

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
    screen = 1
  },
  {
    icon = beautiful.social_icon,
    type = 'social',
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
        for i, tag in pairs(tags) do
            awful.tag.add(i,
              {
                  icon = tag.icon,
                  icon_only = true,
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
-- -- awesome modules
-- local awful = require("awful")
-- local beautiful = require("beautiful")

-- awful.layout.layouts = {
--     awful.layout.suit.tile,
--     awful.layout.suit.floating,
--     awful.layout.suit.max,
-- }

-- awful.screen.connect_for_each_screen(function(s)

--     -- Each screen has its own tag table.
--     -- Tag layouts
--     local l = awful.layout.suit -- Alias to save time :)
--     -- local layouts = { l.max, l.floating, l.max, l.max , l.tile,
--     --     l.max, l.max, l.max, l.floating, l.tile}
--     local layouts = { l.max, l.max, l.max, l.tile , l.tile,
--         l.max, l.max, l.max, l.tile, l.max}

--     -- Tag names
--     local tagnames = beautiful.tagnames or { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" }

--     -- Create tags
--     awful.tag.add(tagnames[1], {
--         layout = layouts[1],
--         screen = s,
--         selected = true,
--     })
--     awful.tag.add(tagnames[2], {
--         layout = layouts[2],
--         screen = s,
--     })
--     awful.tag.add(tagnames[3], {
--         layout = layouts[3],
--         screen = s,
--     })
--     awful.tag.add(tagnames[4], {
--         layout = layouts[4],
--         master_width_factor = 0.6,
--         screen = s,
--     })
--     awful.tag.add(tagnames[5], {
--         layout = layouts[5],
--         master_width_factor = 0.65,
--         screen = s,
--     })
--     awful.tag.add(tagnames[6], {
--         layout = layouts[6],
--         screen = s,
--     })
--     awful.tag.add(tagnames[7], {
--         layout = layouts[7],
--         screen = s,
--     })
--     awful.tag.add(tagnames[8], {
--         layout = layouts[8],
--         screen = s,
--     })
--     awful.tag.add(tagnames[9], {
--         layout = layouts[9],
--         screen = s,
--     })
--     awful.tag.add(tagnames[10], {
--         layout = layouts[10],
--         screen = s,
--     })

--     -- Create all tags at once (without seperate configuration for each tag)
--     -- awful.tag(tagnames, s, layouts)
-- end)
