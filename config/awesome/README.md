# Awesome WM


## Layout
rc.lua
nordic/
  # Core libraries used across components and modules
  core/
    registry.lau
    util.lua
    color.lua
    init.lua
  # Components are a collection of tightly coupled widgets that make up a UI component, such as a bar.
  # Components are the smallest UI unit usable. They are created in isolation or provided bindings to other
  # components via a module.
  components/
    bar/
    agenda/
    notifications/
    init.lua
  # Modules are cohesive collection of components/config. They tie components together
  # as necessary, providing the bindings/communication necessary between disjoint components.
  modules/
    config/
      init.lua
    layout/
      init.lua
    init.lua
  # AwesomeWM theme definition
  theme/
    icons/
    titlebar/
    init.lua
