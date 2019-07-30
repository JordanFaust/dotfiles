-- @module nordic.core.color
local color = {}

local function tohex(num)
    local n = string.format("%x", num) -- this is actually the right number.
    -- but we can't have a hex color code without two digits for each color
    if #n < 2 then
        n = '0' .. n
    end
    return n
end

local function todec(str)
    return tonumber(str, 16)
end

local function standardize(base)
    -- let's transform whatever we get into an 8-length hex value
    local _color = string.sub(base, 2) -- let's remove the hash sign
    assert(#_color == 3 or #_color == 4 or #_color == 6 or #_color == 8,
        "invalid color. must be a hex value of length either 3, 4, 6 or 8")
    local col = ""
    if #_color == 3 then
        for i=1, 3 do
            col = col .. string.rep(string.sub(_color, i, i), 2)
        end
        col = col .. "ff"
    elseif #_color == 4 then
        for i=1, 4 do
            col = col .. string.rep(string.sub(_color, i, i), 2)
        end
    elseif #_color == 6 then
        col = _color .. "ff"
    elseif #color == 8 then
        col = _color
    end

    return col
end

local function split(base)
    local _split = {}
    for i=1, 8 do
        if i % 2 == 0 then
            table.insert(_split, string.sub(base, i-1, i))
        end
    end
    return _split
end

function color.darken(base, val)

    local scolor = split(standardize(base))
    for i=1, 3 do
        local v = scolor[i]
        v = todec(v)
        v = v - val
        if v < 0 then v = 0 end
        scolor[i] = tohex(v)
    end

    return "#" .. table.concat(scolor)

end

function color.lighten(base, val)
    local scolor = split(standardize(base))
    for i=1, 3 do
        local v = scolor[i]
        v = todec(v)
        v = v + val
        if v > 255 then v = 255 end
        scolor[i] = tohex(v)
    end
    return "#" .. table.concat(scolor)
end

return color
