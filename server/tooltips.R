
# From https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip
# To bypass shinyBS bug

# Simulation parameter tab ------------------------------------------------
addTooltip(
  session,
  id = "locality_select",
  title = "Please select the locality you are interested in simulating",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "simulation_period_slider",
  title = "Please select the start and end year of the simulation",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "scope_selector",
  title = "Please choose the type of analysis you are interested in",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "scenarios_number_slider",
  title = "Please choose how many scenarios you want to simulate",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)




