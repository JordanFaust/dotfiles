-- awesome modules
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- custom modules
local nordic = {
    widget = require("nordic.widget"),
    core = require("nordic.core")
}

-- @module nordic.components.agenda.monitoring
local monitoring = { mt = {} }

local cpu_command = [[
    bash -c "
    vmstat 1 2 | tail -1 | awk '{printf \"%d\", $15}'
    "]]

local ram_command = [[
    bash -c "
    free -m | grep 'Mem:' | awk '{printf \"%d@@%d@\", $7, $2}'
    "]]

local disk_command = [[
    bash -c "
    df -k -h /dev/nvme1n1p4 | tail -1 | awk '{print $5}'
    "]]

local function cpu(wrapper, textbox)
    local color = beautiful.frost_2
    local widget = nordic.widget.arc {
        default = 50,
        color = color,
        wrapper = wrapper
    }

    awful.widget.watch(cpu_command, 5, function(_, stdout)
        local raw = stdout
        local unused = string.gsub(raw, "^%s*(.-)%s*$", "%1")
        local value = 100 - tonumber(unused)
        textbox.markup = nordic.core.util.colorize_text("CPU: " .. value .. "%", color)
        widget.value = value
    end)

    return widget
end

local function ram(wrapper, textbox)
    local color = beautiful.xcolor4
    local widget = nordic.widget.arc {
        default = 50,
        color = color,
        wrapper = wrapper
    }

    awful.widget.watch(ram_command, 5, function(_, stdout)
        local available = stdout:match('(.*)@@')
        local total = stdout:match('@@(.*)@')
        local value = (total - available) / total * 100
        textbox.markup = nordic.core.util.colorize_text("RAM: " .. math.floor(value) .. "%", color)
        widget.value = value
    end)

    return widget
end

local function disk(wrapper, textbox)
    local color = beautiful.snow_storm_3
    local widget = nordic.widget.arc {
        default = 50,
        color = color,
        wrapper = wrapper
    }

    awful.widget.watch(disk_command, 5, function(_, stdout)
        local disk_space = stdout
        -- Remove trailing white space
        disk_space = string.gsub(disk_space, '%%', '')
        local value = string.gsub(disk_space, '^%s*(.-)%s*$', '%1')
        widget.value = value
        textbox.markup = nordic.core.util.colorize_text("Disk: " .. value .. "%", color)
    end)

    return widget
end

function monitoring.new(args)
    local cpu_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.core.util.colorize_text("CPU: 50%", beautiful.xcolor6),
        align = "center"
    }
    local ram_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.core.util.colorize_text("RAM: 50%", beautiful.xcolor4),
        align = "center"
    }
    local disk_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.core.util.colorize_text("Disk: 50%", beautiful.xcolor7),
        align = "center"
    }

    local text = {
        {
            {

                {
                    layout = wibox.layout.fixed.vertical,
                    cpu_textbox,
                    ram_textbox,
                    disk_textbox
                },
                reflection = { horizontal = true, vertical = false },
                widget = wibox.container.mirror,
            },
            layout = wibox.layout.fixed.vertical,
        },
        top = dpi(50),
        widget = wibox.container.margin,
    }

    local disk_arcchart = disk(text, disk_textbox)
    local ram_arcchart = ram(disk_arcchart, ram_textbox)
    local cpu_arcchart = cpu(ram_arcchart, cpu_textbox)

    return cpu_arcchart
end

function monitoring.mt:__call(...) --luacheck: no unused args
    return monitoring.new(...)
end

return setmetatable(monitoring, monitoring.mt)
