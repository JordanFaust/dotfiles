-- awesome
local awful = require("awful")
local gears = require("gears")

-- custom
local registry = require("widgets.registry")
local mod = require("config.keys.mod")

-- @module nordic.components.agenda.config
local config = { }

--- Registers additional configuration for the component
-- @method register
-- @tparam string args.name The name of the registered component
function config.register(args)
    local name = args.name
    assert(name, "args.name is nil")

    local keys = _G.root.keys()

    local new = gears.table.join(
        awful.key({ mod.superkey }, "b",
            function()
                registry.emit(name, "toggle", { search = false })
            end,
            {description = "show agenda", group = "awesome"}
        ),

        -- Sidebar Search
        awful.key({ mod.superkey }, "space",
            function()
                registry.emit(name, "toggle", { search = true })
            end,
            {description = "search applications", group = "launcher"}
        )
    )

    -- Add keys for the component to the root key
    _G.root.keys(gears.table.join(keys, new))
end

return config
