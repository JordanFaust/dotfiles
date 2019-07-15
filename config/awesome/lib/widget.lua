-- awesome modules
local wibox = require("wibox")
local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
-- local naughty = require("naughty")
-- custom modules
local helpers = require("helpers")

-- The widget modules is a builder for common widgets. It provides base defaults
-- for consistency and abstracts common patterns.
-- @module widget
local widget = {}

-- Default configuration for various widget components.
--
-- <p>If a widget is created using one of the wrapper functions it will fall
-- back to these values if not passed configuration for the property.</p>
--
-- @name default_config
local default_config = {
    ["textbox"] = {
        ["default"] = {
            font = "sans 14",
            align = "left",
            valign = "center"
        },
        ["centered"] = {
            font = "sans 14",
            align = "center",
            valign = "center"
        },
    },
    ["icon"] = {
        ["default"] = {
            resize = true,
            forced_height = dpi(36), -- consider specifying in theme
            forced_width = dpi(36)
        }
    }
}

-- Get the component config from the passed config or the default config.
--
-- <p> Get the property config for the specified component. Priority is given
-- the passed config. If the property is missing check the default config for
-- the specified widget type.</p>
--
-- @param config the config table passed when creating the component, has priority
-- @param component the type of component
-- @param property the property of the component to apply
local function get_component_property(config, component, kind, property)
    if config[property] then
        return config[property]
    end

    local default = default_config
    if default[component] and default[component][kind] and default[component][kind][property] then
        return default_config[component][kind][property]
    end

    return nil
end

local function centered_textbox(config)
    local textbox = wibox.widget {
        widget = wibox.widget.textbox
    }

    local text = get_component_property(config, "textbox", "centered", "text")
    if text then
        textbox.text = text
    end

    local font = get_component_property(config, "textbox", "centered", "font")
    if font then
        textbox.font = font
    end

    local forced_width = get_component_property(config, "textbox", "centered", "forced_width")
    if forced_width then
        textbox.forced_width = forced_width
    end

    local forced_height = get_component_property(config, "textbox", "centered", "forced_height")
    if forced_height then
        textbox.forced_height = forced_height
    end

    local align = get_component_property(config, "textbox", "centered", "align")
    if align then
        textbox.align = align
    end

    local valign = get_component_property(config, "textbox", "centered", "valign")
    if valign then
        textbox.valign = valign
    end

    return textbox
end

local function default_textbox(config)
    local textbox = wibox.widget {
        widget = wibox.widget.textbox
    }

    local text = get_component_property(config, "textbox", "default", "text")
    if text then
        textbox.text = text
    end

    local font = get_component_property(config, "textbox", "default", "font")
    if font then
        textbox.font = font
    end

    local forced_width = get_component_property(config, "textbox", "centered", "forced_width")
    if forced_width then
        textbox.forced_width = forced_width
    end

    local forced_height = get_component_property(config, "textbox", "centered", "forced_height")
    if forced_height then
        textbox.forced_height = forced_height
    end

    local align = get_component_property(config, "textbox", "default", "align")
    if align then
        textbox.align = align
    end

    local valign = get_component_property(config, "textbox", "default", "valign")
    if valign then
        textbox.valign = valign
    end

    return textbox
end

-- Create a wibox imagebox widget of the specified kind and config.
--
-- <p> Creates a wibox imagebox widget. The specified kind determines the default
-- configuration used, with priority given to the provided config.
--
-- Supported types are: default</p>
--
-- @param config the override config to apply
-- @param kind the kind of imagebox to create
-- @return a wibox.widget.imagebox.
function widget.icon(config)
    if config["icon"] == nil then
        helpers.debug("widget:icon: missing required icon")
        return wibox.widget.imagebox(beautiful.lock_icon)
    end

    local icon = wibox.widget.imagebox(config["icon"])

    local resize = get_component_property(config, "icon", "default", "resize")
    if resize then
        icon.resize = resize
    end

    local font = get_component_property(config, "icon", "default", "font")
    if font then
        icon.font = font
    end

    local forced_width = get_component_property(config, "icon", "default", "forced_width")
    if forced_width then
        icon.forced_width = forced_width
    end

    local forced_height = get_component_property(config, "icon", "default", "forced_height")
    if forced_height then
        icon.forced_height = forced_height
    end

    if config["clickable"] then
        helpers.add_clickable_effect(icon)
    end

    return icon
end

-- Create a wibox textbox widget of the specified kind and config.
--
-- <p> Creates a wibox textbox. The specified kind determines the default
-- configuration used, with priority given to the provided config.
--
-- Supported types are: default, centered, left, right</p>
--
-- @param config the override config to apply
-- @param kind the kind of textbox to create
-- @return a wibox.widget.textbox
function widget.textbox(config, kind)
    if kind == "centered" then
        return centered_textbox(config)
    end

    return default_textbox(config)
end

return widget
