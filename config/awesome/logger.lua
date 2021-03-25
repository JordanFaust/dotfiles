local gears = require("gears")

local logger = {
    -- The logger level
    level = 1
}

-- Return the configured logger level of the logger. Global config is
-- defined by specifying the global logger_level variable
function logger.level()
  return _G.logger_level or logger.level
end

-- Log to stdout with the formatted output as DEBUG level
function logger.debug(message)
    if logger.level() <= 0  then
        gears.debug.dump(os.date("%Y-%m-%d %T") .. " [DEBUG] " .. message)
    end
end

-- Log to stdout with the formatted output as INFO level
function logger.info(message)
    if logger.level() <= 1 then
        gears.debug.dump(os.date("%Y-%m-%d %T") .. " [INFO] " .. message)
    end
end

-- Log to stdout with the formatted output as WARN level
function logger.warn(message)
    if logger.level() <= 2 then
        gears.debug.dump(os.date("%Y-%m-%d %T") .. " [WARN] " .. message)
    end
end

-- Log to stdout with the formatted output as ERROR level
function logger.error(message)
    if logger.level() <= 3 then
        gears.debug.dump(os.date("%Y-%m-%d %T") .. " [ERROR] " .. message)
    end
end

return logger
