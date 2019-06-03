# Scenario tabs ------------------------------------------------------
# General panel -----

show_val <- 200 # 4000
hide_val <- 200 # 300

addTooltip(
  session,
  id = "friendly_name_sc4",
  title = "Please type a name for this scenario. This will be used in model outputs",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "init_year_slider_sc4",
  title = "Please select the first year the scenario is delivered",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "baseline_sc4",
  title = "Please check if this is the baseline scenario which will serve as point of comparison with others",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "load_sc4",
  title = "This loads an *.rds file which contains a previously saved scenario specification. WARNING: This will overwrite all existing parameters for this scenario",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
session,
id = "save_sc4",
title = "This saves all parameters for this scenario in the default download directory of your browser, for future use",
placement = "bottom",
trigger = "hover",
options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "collapse_panels_button_sc4",
  title = "With every click expands or collapses all panels",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)


# Eligibility panel -----
addTooltip(
  session,
  id = "age_eligibility_slider_sc4",
  title = "Please select the age range of people to be invited",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "frequency_eligibility_slider_sc4",
  title = "Please select the minimum number of years between two concecutive Health Checks",
  placement = "bottom",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "invite_known_hypertensives_checkbox_sc4",
  title = "Please select whether known hypertensives should be eligible for a Health Check",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "invite_known_diabetics_checkbox_sc4",
  title = "Please select whether known diabetics should be eligible for a Health Check",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "cancel_program_checkbox_sc4",
  title = "Please select to simulate a no Health Check scenario. It hides all relevant parameters from the user interface",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)


# Coverage panel -----
addTooltip(
  session,
  id = "coverage_detailed_checkbox_sc4",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd0_slider_sc4",
  title = "Please select the percentage of eligible population that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd1_slider_sc4",
  title = "Please select the percentage of eligible population in QIMD 1 (most deprived) areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd2_slider_sc4",
  title = "Please select the percentage of eligible population in QIMD 2 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd3_slider_sc4",
  title = "Please select the percentage of eligible population in QIMD 3 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd4_slider_sc4",
  title = "Please select the percentage of eligible population in QIMD 4 areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_qimd5_slider_sc4",
  title = "Please select the percentage of eligible population in QIMD 5 (least deprived) areas that is invited every year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd0_sc4",
  title = "Please enter the cost per invitation",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd1_sc4",
  title = "Please enter the cost per invitation in QIMD 1 (most deprived) areas",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd2_sc4",
  title = "Please enter the cost per invitation in QIMD 2 areas",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd3_sc4",
  title = "Please enter the cost per invitation in QIMD 3 areas",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd4_sc4",
  title = "Please enter the cost per invitation in QIMD 4 areas",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "coverage_cost_qimd5_sc4",
  title = "Please enter the cost per invitation in QIMD 5 (least deprived) areas",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)


# Uptake panel -----
addTooltip(
  session,
  id = "uptake_detailed_checkbox_sc4",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "uptake_slider_sc4",
  title = "Please select the percentage of people who complete a Health Check after an invitation",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "uptake_cost_sc4",
  title = "Please enter the cost per individual completing a Health Check",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "uptake_equalprob_checkbox_sc4",
  title = "This option will allow you to vary randomly the percentages of people completing a Health Check by sex, age, QIMD and QRISK",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "uptake_table_sc4",
  title = "In the following table, please enter the number of participants over a period of time by sex, age, QIMD and QRISK score",
  placement = "top",
  trigger = "hover",
  options = list(delay = list(show = show_val * 2.5, hide = hide_val))
)

output$uptake_table_help_sc4 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants over a period of time by sex, age, QIMD and QRISK score.'
    )
  )
})

# Prescription rate panel ----
addTooltip(
  session,
  id = "px_detailed_checkbox_sc4",
  title = "Reveals advanced more granular inputs",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "statin_px_slider_sc4",
  title = "Please select the percentage of people with QRISK score higher than 10 that are prescribed statins",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "antihtn_px_slider_sc4",
  title = "Please select the percentage of people with systolic blood pressure higher than 140 mmHg that are prescribed antihypertensive medication",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "statin_px_table_help_sc4",
  title = "'In the following table, please enter the number of participants that were prescribed a statin after a Health Check by QIMD, and QRISK score",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

addTooltip(
  session,
  id = "antihtn_px_table_help_sc4",
  title = "In the following table, please enter the number of participants that were prescribed antihypertensive medication after a Health Check by QIMD, and QRISK score",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

output$statin_px_table_help_sc4 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants that were prescribed a statin after a Health Check by QIMD, and QRISK score.'
    )
  )
})

output$antihtn_px_table_help_sc4 <- renderUI({
  helpText(
    p(
      'In the following table, please enter the number of participants that were prescribed antihypertensive medication after a Health Check by QIMD, and QRISK score.'
    )
  )
})

# Lifestyle panel -----

#smoking-------
addTooltip(
  session,
  id = "smkcess_slider_sc4",
  title = "Please enter the percentage of people quitting smoking in the last year",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "smkcess_cost_sc4",
  title = "Please enter the cost per participant in the smoking cessation program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "smkcess_cost_ovrhd_sc4",
  title = "Please enter the annual fixed cost of the smoking cessation program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))

)
#weight management-------

addTooltip(
  session,
  id = "wghtpct_slider_sc4",
  title = "Please enter the percentage of overweight and obese participants losing weigth",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "wghtreduc_slider_sc4",
  title = "Please enter average percentage reduction in BMI per person in a year, e.g. 5% reduction in BMI",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "wghtloss_cost_sc4",
  title = "Please enter the cost per participant in the weight management program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "wghtloss_cost_ovrhd_sc4",
  title = "Please enter the annual fixed cost of the weight management program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
#physical activityt-------

addTooltip(
  session,
  id = "papct_slider_sc4",
  title = "Please enter the percentage of participants increasing their physical activity levels",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "papincr_slider_sc4",
  title = "Please enter the average increase in days of physical activity, e.g. 2 days",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "pa_cost_sc4",
  title = "Please enter the cost per participant in the the physical activity program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "pa_cost_ovrhd_sc4",
  title = "Please enter the annual fixed cost of the physical activity program",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

# Advance panel -----
#scenario esembles-----
addTooltip(
  session,
  id = "serial_ensemble_checkbox_sc4",
  title = "This allows you to run simultaneously different scenarios for different time spans, e.g scenario 1 from years 1-5 and scenario 2 from years 6-10",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "parallel_ensemble_checkbox_sc4",
  title = "This allows you to run simultaneously different scenarios for different groups of the population, e.g lifestyle interventions for the most deprived and more appoitments offered for the individuals at risk",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "parallel_ensemble_slider_sc4",
  title = "Percentage of the population this scenario applies to if it is part of a parallel or sequential ensemble",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)

#Digital Health Check------
addTooltip(
  session,
  id = "ignore_cholesterol_checkbox_sc4",
  title = "When calculating the Qrisk function this option allows you to exclude cholesterol estimates",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "ignore_sbp_checkbox_sc4",
  title = "When calculating the Qrisk function this option allows you to exclude systolic blood pressure estimates",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "ignore_bmi_checkbox_sc4",
  title = "When calculating the Qrisk function this option allows you to exclude bmi estimates",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)


#Structural Policies/Calibration
addTooltip(
  session,
  id = "structural_smk_slider_sc4",
  title = "Relative change (%) in the prevalence of smoking as result of a structural policy",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "structural_fv_slider_sc4",
  title = "Relative change (%) in the prevalence of fruit and vegetable consumption as result of a structural policy",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "structural_alcohol_slider_sc4",
  title = "Relative change (%) in the population mean of alcohol consumption as result of a structural policy",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "structural_pa_slider_sc4",
  title = "Relative change (%) in the prevalence of physical activity as result of a structural policy",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
addTooltip(
  session,
  id = "structural_bmi_slider_sc4",
  title = "Relative change (%) in the population mean of bmi as result of a structural policy",
  trigger = "hover",
  options = list(delay = list(show = show_val, hide = hide_val))
)
  addTooltip(
    session,
    id = "structural_sbp_slider_sc4",
    title = "Relative change (%) in the population mean of systolic blood pressure as result of a structural policy",
    trigger = "hover",
    options = list(delay = list(show = show_val, hide = hide_val))
)
  addTooltip(
    session,
    id = "structural_chol_slider_sc4",
    title = "Relative change (%) in the population mean of cholesterol as result of a structural policy",
    trigger = "hover",
    options = list(delay = list(show = show_val, hide = hide_val))
)

  #Notes-----
  addTooltip(
    session,
    id = "notes_sc4",
    title = "Add a description of this scenario or any important notes to remember",
    trigger = "hover",
    options = list(delay = list(show = show_val, hide = hide_val))
  )

