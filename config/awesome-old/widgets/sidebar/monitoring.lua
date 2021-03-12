-- awesome modules
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
-- custom modules
local registry = require("widgets.registry")
local nordic = require("nordic")

local monitoring = {}

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
    local widget = nordic.widget.arc {
        default = 50,
        color = beautiful.xcolor6,
        wrapper = wrapper
    }

    registry.add("sidebar::monitoring::cpu", widget)

    -- widget:connect_signal("update::value", function(arcchart, conf)
    --     arcchart.value = conf.value
    --     textbox.markup = nordic.util.colorize_text("CPU: " .. conf.value .. "%", beautiful.xcolor6)
    -- end)

    awful.widget.watch(cpu_command, 5, function(_, stdout)
        local raw = stdout
        local unused = string.gsub(raw, "^%s*(.-)%s*$", "%1")
        local value = 100 - tonumber(unused)
        textbox.markup = nordic.util.colorize_text("CPU: " .. value .. "%", beautiful.xcolor6)
        widget.value = value
        -- registry.emit("sidebar::monitoring::cpu", "update::value", { value = tonumber(value) })
    end)

    return widget
end

local function ram(wrapper, textbox)
    local widget = nordic.widget.arc {
        default = 50,
        color = beautiful.xcolor4,
        wrapper = wrapper
    }

    registry.add("sidebar::monitoring::ram", widget)

    widget:connect_signal("update::value", function(arcchart, conf)
        arcchart.value = conf.value
        textbox.markup = nordic.util.colorize_text("RAM: " .. conf.value .. "%", beautiful.xcolor6)
    end)

    awful.widget.watch(ram_command, 5, function(_, stdout)
        local available = stdout:match('(.*)@@')
        local total = stdout:match('@@(.*)@')
        local value = (total - available) / total * 100
        textbox.markup = nordic.util.colorize_text("RAM: " .. math.floor(value) .. "%", beautiful.xcolor4)
        widget.value = value
        -- registry.emit("sidebar::monitoring::ram", "update::value", { value = value })
    end)

    return widget
end

local function disk(wrapper, textbox)
    local widget = nordic.widget.arc {
        default = 50,
        color = beautiful.xcolor7,
        wrapper = wrapper
    }

    registry.add("sidebar::monitoring::disk", widget)

    -- widget:connect_signal("update::value", function(arcchart, conf)
    --     arcchart.value = conf.value
    --     textbox.markup = nordic.util.colorize_text("Disk: " .. conf.value .. "%", beautiful.xcolor6)
    -- end)

    awful.widget.watch(disk_command, 5, function(_, stdout)
        local disk_space = stdout
        -- Remove trailing white space
        disk_space = string.gsub(disk_space, '%%', '')
        local value = string.gsub(disk_space, '^%s*(.-)%s*$', '%1')
        widget.value = value
        textbox.markup = nordic.util.colorize_text("Disk: " .. value .. "%", beautiful.xcolor7)
        -- registry.emit("sidebar::monitoring::disk", "update::value", { value = value })
    end)

    return widget
end

function monitoring.new()

    local cpu_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.util.colorize_text("CPU: 50%", beautiful.xcolor6),
        align = "center"
    }
    local ram_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.util.colorize_text("RAM: 50%", beautiful.xcolor4),
        align = "center"
    }
    local disk_textbox = wibox.widget {
        widget = wibox.widget.textbox,
        font = "sans bold 12",
        markup = nordic.util.colorize_text("Disk: 50%", beautiful.xcolor7),
        align = "center"
    }

    local text = {
        widget = wibox.container.margin,
        top = 50,
        {
            layout = wibox.layout.fixed.vertical,
            {

                widget = wibox.container.mirror,
                reflection = { horizontal = true, vertical = false },
                {
                    layout = wibox.layout.fixed.vertical,
                    cpu_textbox,
                    ram_textbox,
                    disk_textbox
                }
            }
        }
    }

    local disk_arcchart = disk(text, disk_textbox)
    local ram_arcchart = ram(disk_arcchart, ram_textbox)
    local cpu_arcchart = cpu(ram_arcchart, cpu_textbox)

    return cpu_arcchart
end

return monitoring
