conditionalPanel(
  condition = "input.level == 5",
  bsCollapse(
    id = "collapse_panel_sc5",
    multiple = TRUE,
    open = c(
      "General Parameters"
      # "Eligibility Criteria",
      # "Appointments Offered Yearly (%)",
      # "Health Checks received (%)",
      # "Prescription Rate",
      # "Impact on Lifestyle"
    ),


# General panel ----------------------------------------------------------

bsCollapsePanel(
  "General Parameters",
  style = "default",
  wellPanel(fluidRow(
    column(
      4,
      textInput("friendly_name_sc5", "Friendly name", "Scenario 5"),
      uiOutput("init_year_slider_sc5")
    ),
    column(
      4,
      br(),
      switchInput(
        "baseline_sc5",
        "Baseline scenario",
        FALSE,
        onLabel = "Yes",
        offLabel = "No",
        labelWidth = "100%"
      )
    ),
    column(
      4,
      br(),
      downloadButton(
        "save_sc5",
        "Save scenario",
        #icon("save"),
        #class = "btn-info",
        labelWidth = "100%",
        width = "100%"
      ),
        actionButton(
          "collapse_panels_button_sc5",
          "Expand/Collapse panels",
          icon("minus-square"),
          labelWidth = "100%"
        ),
        fileInput(
          "load_sc5",
          "",
          multiple = FALSE,
          accept = ".rds",
          placeholder = "",
          buttonLabel = "Load scenario"
        )
      )
    ))
  ),

# Eligibility panel ----------------------------------------------------------
bsCollapsePanel("Eligibility Criteria",
                style = "success",
                wellPanel(fluidRow(
                  column(
                    4,
                    sliderInput(
                      "age_eligibility_slider_sc5",
                      "Age eligibility",
                      30,
                      84,
                      c(40, 74),
                      1,
                      sep = "",
                      ticks = FALSE
                    ),
                    sliderInput(
                      "frequency_eligibility_slider_sc5",
                      "Minimum allowed years between two Health Checks",
                      1,
                      15,
                      5,
                      1,
                      sep = "",
                      ticks = FALSE
                    )
                  ),
                  column(
                    4,
                    switchInput(
                      "invite_known_hypertensives_checkbox_sc5",
                      "Invite people known to have  hypertension",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    ),
                    br(),
                    br(),
                    switchInput(
                      "invite_known_diabetics_checkbox_sc5",
                      "Invite people known to have diabetes",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                  ),
                  column(
                    4,
                    switchInput(
                      "cancel_program_checkbox_sc5",
                      "Nobody eligible",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                  )
                ))),

# Coverage panel ----------------------------------------------------------
bsCollapsePanel(
  "Appointments Offered Yearly (%)",
  style = "success",
  wellPanel(
    fluidRow(
      column(
        4,
        sliderInput(
          "coverage_qimd0_slider_sc5",
          "Invitations (percentage of eligible population)",
          0,
          100,
          20,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
      ),
      column(
        4,
        numericInput(
          "coverage_cost_qimd0_sc5",
          "Cost per invitation",
          20,
          0,
          500,
          1
        )
      ),
      column(
        4,
        br(),
        switchInput(
          "coverage_detailed_checkbox_sc5",
          "Detailed input",
          value = FALSE,
          onLabel = "Show",
          offLabel = "Hide",
        )
      )
    ),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd1_slider_sc5",
        "Invitations as percentage of eligible population in QIMD 1 (most deprived)",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd1_sc5",
        "Cost per invitation in QIMD 1 (most deprived)",
        20,
        0,
        500,
        1
      )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd2_slider_sc5",
        "Invitations as percentage of eligible population in QIMD 2",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd2_sc5",
        "Cost per invitation in QIMD 2",
        20,
        0,
        500,
        1
      )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd3_slider_sc5",
        "Invitations as percentage of eligible population in QIMD 3",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd3_sc5",
        "Cost per invitation in QIMD 3",
        20,
        0,
        500,
        1
      )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd4_slider_sc5",
        "Invitations as percentage of eligible population in QIMD 4",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd4_sc5",
        "Cost per invitation in QIMD 4",
        20,
        0,
        500,
        1
      )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd5_slider_sc5",
        "Invitations as percentage of eligible population in QIMD 5 (least deprived)",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd5_sc5",
        "Cost per invitation in QIMD 5 (least deprived)",
        20,
        0,
        500,
        1
      )
    ))
  )
),

# Uptake panel ----------------------------------------------------------

bsCollapsePanel("Health Checks Received (%)",
                style = "success",
                wellPanel(
                  fluidRow(
                    column(
                      4,
                      sliderInput(
                        "uptake_slider_sc5",
                        "Proportion of invitees attending a Health Check",
                        0,
                        100,
                        66,
                        0.5,
                        sep = "",
                        ticks = FALSE,
                        post  = " %"
                      )
                    ),
                    column(
                      4,
                      numericInput("uptake_cost_sc5",
                                   "Cost per completed Health Check", 20, 0, 500, 1)
                    ),
                    column(
                      4,
                      br(),
                      switchInput(
                        "uptake_detailed_checkbox_sc5",
                        "Detailed input",
                        value = FALSE,
                        onLabel = "Show",
                        offLabel = "Hide",
                        labelWidth = "100%"
                      )
                    )
                  ),
                  fluidRow(
                    br(),
                    switchInput(
                      "uptake_equalprob_checkbox_sc5",
                      "Assume equal attendance probability",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    ),
                    uiOutput("uptake_table_help_sc5"),
                    tableOutput("uptake_table_sc5")
                  )
                )),


# Prescription rate panel  ------------------------------------------------
bsCollapsePanel(
  "Prescription Rate",
  style = "success",
  wellPanel(
    fluidRow(
      column(
        4,
        sliderInput(
          "statin_px_slider_sc5",
          "Proportion of participants with QRISK 10+ prescribed statins",
          0,
          100,
          60,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
      ),
      column(
        4,
        sliderInput(
          "antihtn_px_slider_sc5",
          "Proportion of participants with high systolic blood pressure prescribed antihypertensives",
          0,
          100,
          60,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
      ),
      column(
        4,
        br(),
        switchInput(
          "px_detailed_checkbox_sc5",
          "Detailed input",
          value = FALSE,
          onLabel = "Show",
          offLabel = "Hide",
          labelWidth = "100%"
        )
      )
    )),

  wellPanel(
    fluidRow(
    uiOutput("statin_px_table_help_sc5"),
    tableOutput("statin_px_table_sc5")
)),

wellPanel(
  fluidRow(
    uiOutput("antihtn_px_table_help_sc5"),
    tableOutput("antihtn_px_table_sc5")
  ))
),

# Lifestyle panel  ------------------------------------------------
bsCollapsePanel(
  "Impact on Lifestyle",
  style = "success",
  wellPanel(helpText(h5('Smoking cessation')),
            fluidRow(
              column(
                4,
                sliderInput(
                  "smkcess_slider_sc5",
                  "Percentage of smoker participants successfully quit smoking for at least a year",
                  0,
                  100,
                  0,
                  0.5,
                  sep = "",
                  ticks = FALSE,
                  post  = " %"
                )
              ),
              column(
                4,
                numericInput(
                  "smkcess_cost_sc5",
                  "Smoking cessation cost per successful quit",
                  0,
                  0,
                  5e6,
                  1
                )
              ),
              column(
                4,
                numericInput(
                  "smkcess_cost_ovrhd_sc5",
                  "Smoking cessation annual overhead costs",
                  0,
                 -5e6,
                  5e6,
                  1
                )
              )
            )),
  wellPanel(
  helpText(h5('Weight management')),
  fluidRow(
    column(
      4,
      sliderInput(
        "wghtpct_slider_sc5",
        "Percentage of overweight and obese participants losing weight",
        0,
        100,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      ),
      sliderInput(
        "wghtreduc_slider_sc5",
        "Average weight loss as percentage weight", # equivalent to percentage of BMI
        0,
        20,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "wghtloss_cost_sc5",
        "Weight management annual cost per participant losing weight",
        0,
        0,
        5e6,
        1
      )
    ),
    column(
      4,
      numericInput(
        "wghtloss_cost_ovrhd_sc5",
        "Weight management annual overhead costs",
        0,
       -5e6,
        5e6,
        1
      )
    )
  )
  ),
  wellPanel(
  helpText(h5('Physical activity')),
  fluidRow(
    column(
      4,
      sliderInput(
        "papct_slider_sc5",
        "Percentage of participants increasing their physical activity",
        0,
        100,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      ),
      sliderInput(
        "papincr_slider_sc5",
        "Average increase in active days per week",
        0,
        7,
        0,
        1,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
    ),
    column(
      4,
      numericInput(
        "pa_cost_sc5",
        "Physical activity annual cost per more active participant",
        0,
        0,
        5e6,
        1
      )
    ),
    column(
      4,
      numericInput(
        "pa_cost_ovrhd_sc5",
        "Physical activity actions annual overhead costs",
        0,
       -5e6,
        5e6,
        1
      )
    )
  ))
),

# Advanced panel  ------------------------------------------------
bsCollapsePanel(
  "Advanced",
  style = "default",
  wellPanel(fluidRow(
    h5("Scenario ensembles"),
    column(6,
  switchInput(
    "serial_ensemble_checkbox_sc5",
    "This scenario is part of a serial ensemble",
    value = FALSE,
    onLabel = "Yes",
    offLabel = "No",
    labelWidth = "100%"
  )
  )),
fluidRow(
    column(6,
           switchInput(
             "parallel_ensemble_checkbox_sc5",
             "This scenario is part of a parallel ensemble",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
    ),
    column(6,
    sliderInput(
      "parallel_ensemble_slider_sc5",
      "Percentage of population this scenario applies to",
      0,
      100,
      0,
      0.5,
      sep = "",
      ticks = FALSE,
      post  = " %"
    )
    ))),
  wellPanel(fluidRow(
    h5("Digital Health Checks"),
    column(4,
           switchInput(
             "ignore_cholesterol_checkbox_sc5",
             "Ignore cholesterol in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
    ),
    column(4,
           switchInput(
             "ignore_sbp_checkbox_sc5",
             "Ignore systolic blood pressure in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
    ),
    column(4,
           switchInput(
             "ignore_bmi_checkbox_sc5",
             "Ignore BMI in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
    )
    )),
  wellPanel(fluidRow(
    h5("Structural Policies / Calibration"),
    column(6,
           sliderInput(
             "structural_smk_slider_sc5",
             "Smoking prevalence relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_fv_slider_sc5",
             "Fruit & veg relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_alcohol_slider_sc5",
             "Alcohol relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_pa_slider_sc5",
             "Physical activity relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_bmi_slider_sc5",
             "BMI relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_sbp_slider_sc5",
             "Systolic blood pressure relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           ),
           sliderInput(
             "structural_chol_slider_sc5",
             "Cholesterol relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )
    ),
    column(3

    ),
    column(3

    )
  ))
),

# Notes  ------------------------------------------------
bsCollapsePanel("Notes",
                style = "default",
                wellPanel(fluidRow(
                  textAreaInput(
                    "notes_sc5",
                    "",
                    "",
                    width = "100%",
                    rows = 6,
                    resize = "both"
                  )
                )))
  ),

conditionalPanel(condition = "input.level <= input.scenarios_number_slider",
                 fluidRow(
                 column(6,
                        actionButton(
                          "previous_sc5",
                          "Go to next scenario",
                          icon = icon("step-backward"),
                          #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                          class = "btn btn-primary",
                          width = "100%"
                        )),
                 column(
                   6,
                   actionButton(
                     "next_sc5",
                     "Go to next scenario",
                     icon = icon("step-forward"),
                     #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     class = "btn btn-primary",
                     width = "100%"
                   )
                 ))),

conditionalPanel(condition = "input.level == input.scenarios_number_slider",
                 fluidRow(
                   br(),
                   actionButton(
                     "run_simulation_sc5",
                     "Run simulation (all scenarios)",
                     icon = icon("paper-plane"),
                     #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     class = "btn-info",
                     width = "100%"
                   )
                 )))



