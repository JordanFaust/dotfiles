-- awesome modules

-- custom modules
local helpers = require("helpers")

-- Registry provides lookups and signaling for registered components.
--
-- <p> Creates a registry of widget components. Registered components can
-- have signals emitted allowing interactions between component widgets. </p>
--
-- @module registery
local registry = {}

-- Components is a cache of registered components.
--
-- <p>Components contains a mapping of registered components under a lookup
-- name. The name must be known between components that want to interact. </p>
--
-- @name components
local components = {}

-- Provider is a data source provider.
--
-- <p> Providers are sources of data for components. They contain details on
-- the command to run and the frequency of the command to run. If multiple
-- components create a provider with the same name, the lowest specified execution
-- frequency will be used. A provider tracks the associated components and the
-- signals to emit for each component.
--
-- @name providers = {}

-- Adds the component with the given name.
--
-- @param name the lookup name of the component
-- @param component an awfule.wibar.widget component
function registry.add(name, component)
    if component == nil then
        helpers.debug("can't add nil component for: "..name)
        return
    end
    components[name] = component
end

-- Returns a registered component with the given name.
--
-- @param name the name of the component
-- @return the registered component matching the name
function registry.get(name)
    local component = components[name]
    if component == nil then
        helpers.debug("nil component for key: "..name)
    end

    return component
end

-- Emits the specified signal on the target component.
--
-- <p> Using the lookup name 'target', emits the specified signal for the component.
-- The component the signal emits MUST be configured on the target component. This
-- interaction is a part of the public API of the component. </p>
--
-- @param target the name of the target component
-- @param singal the signal to emit
function registry.emit(target, signal, config)
    local component = components[target]
    if component == nil then
        helpers.debug("attempt to send signal to unregisterd component: "..target)
        return
    end

    if config then
        component:emit_signal(signal, config)
        return
    end

    component:emit_signal(signal)
end

function registry.debug()
    local count = 0
    for key, _ in pairs(components) do
        helpers.debug("key: "..key)
        count = count + 1
    end
    helpers.debug("total: "..count)
end

-- function registry.add_provider(name, timeout, component, signal)
--     local provider = providers[name]
--     if provider == nil then
--         providers[name] =
--     end
-- end

return registry
