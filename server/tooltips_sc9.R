# Scenario tabs ------------------------------------------------------
# General panel -----
addTooltip(
  session,
  id = "friendly_name_sc9",
  title = "Please type a name for this scenario. This will be used in model outputs",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "init_year_slider_sc9",
  title = "Please select the first year the scenario is delivered",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "baseline_sc9",
  title = "Please check if this is the baseline scenario that every other scenario is compared with",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "load_sc9",
  title = "This loads an *.rds file which contains a previously saved scenario specification. WARNING: This will overwrite all existing parameters for this scenario",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
session,
id = "save_sc9",
title = "This saves all parameters for this scenario in the default download directory of your browser, for future use",
placement = "bottom",
trigger = "hover",
options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "collapse_panels_button_sc9",
  title = "With every click expands or collapses all panels",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)


# Eligibility panel -----
addTooltip(
  session,
  id = "age_eligibility_slider_sc9",
  title = "Please select the age range of people to be invited",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "frequency_eligibility_slider_sc9",
  title = "Please select the minimum number of years between two concecutive Health Checks",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "invite_known_hypertensives_checkbox_sc9",
  title = "Please select whether known hypertensives should be eligible for a Health Check",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "invite_known_diabetics_checkbox_sc9",
  title = "Please select whether known diabetics should be eligible for a Health Check",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "cancel_program_checkbox_sc9",
  title = "Please select to simulate a no Health Check scenario. It hides all relevant parameters from the user interface",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)


# Coverage panel -----
addTooltip(
  session,
  id = "coverage_detailed_checkbox_sc9",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd0_slider_sc9",
  title = "Please select the percentage of eligible population that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd1_slider_sc9",
  title = "Please select the percentage of eligible population in QIMD 1 (most deprived) areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd2_slider_sc9",
  title = "Please select the percentage of eligible population in QIMD 2 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd3_slider_sc9",
  title = "Please select the percentage of eligible population in QIMD 3 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd4_slider_sc9",
  title = "Please select the percentage of eligible population in QIMD 4 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_qimd5_slider_sc9",
  title = "Please select the percentage of eligible population in QIMD 5 (least deprived) areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd0_sc9",
  title = "Please enter the cost per invitation",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd1_sc9",
  title = "Please enter the cost per invitation in QIMD 1 (most deprived) areas",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd2_sc9",
  title = "Please enter the cost per invitation in QIMD 2 areas",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd3_sc9",
  title = "Please enter the cost per invitation in QIMD 3 areas",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd4_sc9",
  title = "Please enter the cost per invitation in QIMD 4 areas",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "coverage_cost_qimd5_sc9",
  title = "Please enter the cost per invitation in QIMD 5 (least deprived) areas",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)


# Uptake panel -----
addTooltip(
  session,
  id = "uptake_detailed_checkbox_sc9",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "uptake_slider_sc9",
  title = "Please select the percentage of people who complete a Health Check after an invitation",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "uptake_cost_sc9",
  title = "Please enter the cost per completed Health Check",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "uptake_table_sc9",
  title = "In the following table, please enter the number of participants over a period of time by sex, age, QIMD, and QRISK score",
  placement = "top",
  trigger = "hover",
  options = list(delay = list(show = 10000, hide = 300))
)

output$uptake_table_help_sc9 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants over a period of time by sex, age, QIMD, and QRISK score.'
    )
  )
})

# Prescription rate panel ----
addTooltip(
  session,
  id = "px_detailed_checkbox_sc9",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "statin_px_slider_sc9",
  title = "Please select the percentage of people with QRISK score higher than 10 that are prescribed statins",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

addTooltip(
  session,
  id = "antihtn_px_slider_sc9",
  title = "Please select the percentage of people with systolic blood pressure higher than 140 mmHg that are prescribed antihypertensive medication",
  trigger = "hover",
  options = list(delay = list(show = 4000, hide = 300))
)

output$statin_px_table_help_sc9 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants that were prescribed a statin after a Health Check by QIMD, and QRISK score.'
    )
  )
})

output$antihtn_px_table_help_sc9 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants that were prescribed antihypertensive medication after a Health Check by QIMD, and QRISK score.'
    )
  )
})
