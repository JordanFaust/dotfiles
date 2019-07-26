-- awesome modules
local gears = require("gears")

-- @module shape
local shape = {}

-- Create rounded rectangle shape
shape.rrect = function(radius)
    return function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, radius)
    end
end

shape.rbar = function()
  return function(cr, width, height)
    gears.shape.rounded_bar(cr, width, height)
  end
end

shape.prrect = function(radius, tl, tr, br, bl)
  return function(cr, width, height)
    gears.shape.partially_rounded_rect(cr, width, height, tl, tr, br, bl, radius)
  end
end

-- Create info bubble shape
-- TODO
shape.infobubble = function(radius)
  return function(cr, width, height)
    gears.shape.infobubble(cr, width, height, radius)
  end
end

-- Create rectangle shape
shape.rect = function()
    return function(cr, width, height)
        gears.shape.rectangle(cr, width, height)
    end
end

-- Create circle shape
shape.circle = function()
    return function(cr, width, height)
        gears.shape.circle(cr, width, height)
    end
end

return shape
