
# From https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip
# To bypass shinyBS bug

# Simulation parameter tab ------------------------------------------------
show_val <- 200 # 4000
hide_val <- 200 # 300

addTooltip(
  session,
  id = "locality_select",
  title = "Please select the locality you are interested in simulating",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "simulation_period_slider",
  title = "Please select the start and end year of the simulation",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "scope_selector",
  title = "Please choose the type of analysis you are interested in",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "scenarios_number_slider",
  title = "Please choose how many scenarios you want to simulate",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "national_qimd_checkbox",
  title = "Please choose to use either the local or national Index of Multideprivation (IMD)",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)



addTooltip(
  session,
  id = "ward_output_checkbox",
  title = "Please choose if you the analysis at Ward level",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
