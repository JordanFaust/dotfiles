-- @module util
local util = {}

-- Merges two config tables with priority given to the overrides.
--
-- @param config the configuration source, with defaults
-- @param overrides the overrides to apply to the config
-- @return a merged table
function util.merge(config, overrides)
    if type(config) == 'table' and type(overrides) == 'table' then
        for key, value in pairs(overrides) do
            if type(value) == 'table' and type(config[key] or false) == 'table' then
                util.merge(config[key], value)
            else
                config[key]=value
            end
        end
    end
    return config
end

return util
