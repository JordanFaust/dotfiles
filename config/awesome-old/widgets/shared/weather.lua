-- awesome modules
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local libwidget = require("lib.widget")
local helpers = require("helpers")
local registry = require("widgets.registry")

-- @module weather
local weather = {}

-- Configuration for the openweathermap API
-- @name config
local config = {
    units = "imperial",
    symbol = "°F",
    city_id = 4394870,
    update_interval = 1200
}

local function weather_credentials()
    return "credentials-lookup widget openweathermap api-key"
end

local function command()
    local key = config.key
    local city_id = config.city_id
    local units = config.units
    local symbol = config.symbol
    return [[
        bash -c '
            KEY="]]..key..[["
            CITY="]]..city_id..[["
            UNITS="]]..units..[["
            SYMBOL="]]..symbol..[["

            weather=$(curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=$KEY&id=$CITY&units=$UNITS")

            if [ ! -z "$weather" ]; then
                weather_temp=$(echo "$weather" | jq ".main.temp" | cut -d "." -f 1)
                weather_icon=$(echo "$weather" | jq -r ".weather[].icon" | head -1)
                weather_description=$(echo "$weather" | jq -r ".weather[].description" | head -1)

                echo "$weather_icon" "$weather_description" "$weather_temp$SYMBOL"
            else
                echo "... Info unavailable"
            fi
    ']]
end

local function update(code, details)
    -- Set icon
    local image
    if string.find(code, "01d") then
        image = beautiful.sun_icon
    elseif string.find(code, "01n") then
        image = beautiful.star_icon
    elseif string.find(code, "02d") then
        image = beautiful.dcloud_icon
    elseif string.find(code, "02n") then
        image = beautiful.ncloud_icon
    elseif string.find(code, "03") or string.find(code, "04") then
        image = beautiful.cloud_icon
    elseif string.find(code, "09") or string.find(code, "10") then
        image = beautiful.rain_icon
    elseif string.find(code, "11") then
        image = beautiful.storm_icon
    elseif string.find(code, "13") then
        image = beautiful.snow_icon
    elseif string.find(code, "50") or string.find(code, "40") then
        image = beautiful.mist_icon
    else
        image = beautiful.whatever_icon
    end

    local icon = registry.get("sidebar::weather::icon")
    local textbox = registry.get("sidebar::weather::textbox")

    icon.image = image
    -- Replace -0 with 0 degrees
    details = string.gsub(details, '%-0', '0')
    -- Capitalize first letter of the description
    details = details:sub(1,1):upper()..details:sub(2)
    textbox.markup = details
end

local function start_watch()
    local function watch()
        awful.widget.watch(command(), config.update_interval, function(_, stdout)
            local icon_code = string.sub(stdout, 1, 3)
            local weather_details = string.sub(stdout, 5)
            weather_details = string.gsub(weather_details, '^%s*(.-)%s*$', '%1')
            update(icon_code, weather_details)
        end)
    end

    awful.spawn.with_line_callback(weather_credentials(), {
        stdout = function(line)
            config.key = line
            watch()
        end,
        stderr = function(_)
            helpers.debug("failed to get credentials")
        end
    })
end

function weather.sidebar()
    local icon = libwidget.icon({
        icon = beautiful.whatever_icon,
        forced_width = 40,
        forced_height = 40
    })
    local textbox = libwidget.textbox({ font = "sans 14" }, "centered")
    textbox.text = "Loading..."

    local widget = wibox.widget{
        nil,
        {
            icon,
            textbox,
            layout = wibox.layout.fixed.horizontal
        },
        nil,
        expand = "none",
        layout = wibox.layout.align.horizontal
    }

    registry.add("sidebar::weather::icon", icon)
    registry.add("sidebar::weather::textbox", textbox)
    registry.add("sidebar::weather", widget)

    start_watch()

    return widget
end

return weather
