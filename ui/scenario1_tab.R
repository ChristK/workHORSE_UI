conditionalPanel(
  condition = "input.level == 1",
  bsCollapse(
    id = "collapse_panel_sc1",
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
      textInput("friendly_name_sc1", "Friendly name", "Scenario 1")
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please type a name for this scenario. This will be used in model outputs")
        ),
      uiOutput("init_year_slider_sc1")
    ),
    column(
      4,
      br(),
      switchInput(
        "baseline_sc1",
        "Baseline scenario",
        FALSE,
        onLabel = "Yes",
        offLabel = "No",
        labelWidth = "100%"

      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please check if this is the baseline scenario which will serve as point of comparison with others")
        ),
      
      colourInput(
        inputId = "col_sc1",
        label = "Pick a color for the scenario",
        value = def_col_small[[1]],
        palette = "limited",
        allowedCols = def_col,
        returnName = TRUE
      ),
      uiOutput("panel_col_sc1")
    ),
    column(
      4,
      br(),
      downloadButton(
        "save_sc1",
        "Save scenario",
        #icon("save"),
        #class = "btn-info",
        labelWidth = "100%",
        width = "100%"
      ),
        actionButton(
          "collapse_panels_button_sc1",
          "Expand/Collapse panels",
          icon("minus-square"),
          labelWidth = "100%"
        ),
        fileInput(
          "load_sc1",
          "",
          multiple = FALSE,
          accept = ".rds",
          placeholder = "",
          buttonLabel = "Load scenario"
        )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Load scenario: this loads an *.rds file which contains a previously saved scenario specification. WARNING: This will overwrite")
        )
      )
    
    )
    )),

# Eligibility panel ----------------------------------------------------------
bsCollapsePanel("Eligibility Criteria",
                style = "success",
                wellPanel(fluidRow(
                  column(
                    4,
                    sliderInput(
                      "age_eligibility_slider_sc1",
                      "Age eligibility",
                      30,
                      84,
                      c(40, 74),
                      1,
                      sep = "",
                      ticks = FALSE
                    )
                      %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Please select the age range of people to be invited")
                        )
                    ,
                    sliderInput(
                      "frequency_eligibility_slider_sc1",
                      "Minimum allowed years between two Health Checks",
                      1,
                      15,
                      5,
                      1,
                      sep = "",
                      ticks = FALSE
                    )
                    %>%
                      shinyInput_label_embed(
                        icon("info") %>%
                          bs_embed_tooltip(title = "Please select the minimum number of years between two concecutive Health Checks")
                      )
                  ),
                  column(
                    4,
                    switchInput(
                      "invite_known_hypertensives_checkbox_sc1",
                      "Invite people known to have  hypertension",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                    %>%
                      shinyInput_label_embed(
                        icon("info") %>%
                          bs_embed_tooltip(title = "Please select whether known hypertensives should be eligible for a Health Check")
                    ),
                    br(),
                    br(),
                    switchInput(
                      "invite_known_diabetics_checkbox_sc1",
                      "Invite people known to have diabetes",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                    %>%
                      shinyInput_label_embed(
                        icon("info") %>%
                          bs_embed_tooltip(title ="Please select whether known diabetics should be eligible for a Health Check")
                      )
                  ),
                  column(
                    4,
                    switchInput(
                      "cancel_program_checkbox_sc1",
                      "Nobody eligible",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                    %>%
                      shinyInput_label_embed(
                        icon("info") %>%
                          bs_embed_tooltip(title = "Please select to simulate a no Health Check scenario. It hides all relevant parameters from the user interface")
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
          "coverage_qimd0_slider_sc1",
          "Invitations (percentage of eligible population)",
          0,
          100,
          20,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Please select the percentage of eligible population that is invited every year")
          )
      ),
      column(
        4,
        numericInput(
          "coverage_cost_qimd0_sc1",
          "Cost per invitation",
          20,
          0,
          500,
          1
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Please enter the cost per invitation")
          )
      ),
      column(
        4,
        br(),
        switchInput(
          "coverage_detailed_checkbox_sc1",
          "Detailed input",
          value = FALSE,
          onLabel = "Show",
          offLabel = "Hide",
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Reveals more granular inputs")
          )
      )
    ),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd1_slider_sc1",
        "Invitations as percentage of eligible population in QIMD 1 (most deprived)",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the percentage of eligible population in QIMD 1 (most deprived) areas that is invited every year")
        )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd1_sc1",
        "Cost per invitation in QIMD 1 (most deprived)",
        20,
        0,
        500,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per invitation in QIMD 1 (most deprived) areas")
        )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd2_slider_sc1",
        "Invitations as percentage of eligible population in QIMD 2",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the percentage of eligible population in QIMD 2 areas that is invited every year")
        )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd2_sc1",
        "Cost per invitation in QIMD 2",
        20,
        0,
        500,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per invitation in QIMD 2 areas")
        )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd3_slider_sc1",
        "Invitations as percentage of eligible population in QIMD 3",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the percentage of eligible population in QIMD 3 areas that is invited every year")
        )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd3_sc1",
        "Cost per invitation in QIMD 3",
        20,
        0,
        500,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per invitation in QIMD 3 areas")
        )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd4_slider_sc1",
        "Invitations as percentage of eligible population in QIMD 4",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the percentage of eligible population in QIMD 4 areas that is invited every year")
        )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd4_sc1",
        "Cost per invitation in QIMD 4",
        20,
        0,
        500,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per invitation in QIMD 4 areas")
        )
    )),
    fluidRow(column(
      4,
      sliderInput(
        "coverage_qimd5_slider_sc1",
        "Invitations as percentage of eligible population in QIMD 5 (least deprived)",
        0,
        100,
        20,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the percentage of eligible population in QIMD 5 (least deprived) areas that is invited every year")
        )
    ),
    column(
      4,
      numericInput(
        "coverage_cost_qimd5_sc1",
        "Cost per invitation in QIMD 5 (least deprived)",
        20,
        0,
        500,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per invitation in QIMD 5 (least deprived) areas")
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
                        "uptake_slider_sc1",
                        "Proportion of invitees attending a Health Check",
                        0,
                        100,
                        66,
                        0.5,
                        sep = "",
                        ticks = FALSE,
                        post  = " %"
                      )
                      %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Please select the percentage of people who complete a Health Check after an invitation")
                        )
                    ),
                    column(
                      4,
                      numericInput("uptake_cost_sc1",
                                   "Cost per completed Health Check", 20, 0, 500, 1)
                      %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Please enter the cost per individual completing a Health Check")
                        )
                    ),
                    column(
                      4,
                      br(),
                      switchInput(
                        "uptake_detailed_checkbox_sc1",
                        "Detailed input",
                        value = FALSE,
                        onLabel = "Show",
                        offLabel = "Hide",
                        labelWidth = "100%"
                      )
                      %>%
                        shinyInput_label_embed(
                          icon("info") %>%
                            bs_embed_tooltip(title = "Reveal more granular inputs")
                        )
                    )
                  ),
                  fluidRow(
                    br(),
                    switchInput(
                      "uptake_equalprob_checkbox_sc1",
                      "Assume equal attendance probability",
                      value = FALSE,
                      onLabel = "Yes",
                      offLabel = "No",
                      labelWidth = "100%"
                    )
                    %>%
                      shinyInput_label_embed(
                        icon("info") %>%
                          bs_embed_tooltip(title = "This option will allow you to vary randomly the percentages of people completing a Health Check by sex, age, QIMD and QRISK")
                    ),
                    uiOutput("uptake_table_help_sc1"),
                    tableOutput("uptake_table_sc1")
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
          "statin_px_slider_sc1",
          "Proportion of participants with QRISK 10+ prescribed statins",
          0,
          100,
          60,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Please select the percentage of people with QRISK score higher than 10 that are prescribed statins")
          )
      ),
      column(
        4,
        sliderInput(
          "antihtn_px_slider_sc1",
          "Proportion of participants with high systolic blood pressure prescribed antihypertensives",
          0,
          100,
          60,
          0.5,
          sep = "",
          ticks = FALSE,
          post  = " %"
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Please select the percentage of people with systolic blood pressure higher than 140 mmHg that are prescribed antihypertensive medication")
          )
      ),
      column(
        4,
        br(),
        switchInput(
          "px_detailed_checkbox_sc1",
          "Detailed input",
          value = FALSE,
          onLabel = "Show",
          offLabel = "Hide",
          labelWidth = "100%"
        )
        %>%
          shinyInput_label_embed(
            icon("info") %>%
              bs_embed_tooltip(title = "Reveal more granular options")
          )
      )
    )),

  wellPanel(
    fluidRow(
    uiOutput("statin_px_table_help_sc1"),
    tableOutput("statin_px_table_sc1")
)),

wellPanel(
  fluidRow(
    uiOutput("antihtn_px_table_help_sc1"),
    tableOutput("antihtn_px_table_sc1")
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
                  "smkcess_slider_sc1",
                  "Percentage of smoker participants successfully quit smoking for at least a year",
                  0,
                  100,
                  0,
                  0.5,
                  sep = "",
                  ticks = FALSE,
                  post  = " %"
                )
                  %>%
                    shinyInput_label_embed(
                      icon("info") %>%
                        bs_embed_tooltip(title = "Please enter the percentage of people quitting smoking in the last year")
                    )
              ),
              column(
                4,
                numericInput(
                  "smkcess_cost_sc1",
                  "Smoking cessation cost per successful quit",
                  0,
                  0,
                  5e6,
                  1
                )
                %>%
                  shinyInput_label_embed(
                    icon("info") %>%
                      bs_embed_tooltip(title = "Please enter the cost per participant in the smoking cessation program")
                  )
              ),
              column(
                4,
                numericInput(
                  "smkcess_cost_ovrhd_sc1",
                  "Smoking cessation annual overhead costs",
                  0,
                 -5e6,
                  5e6,
                  1
                )
                %>%
                  shinyInput_label_embed(
                    icon("info") %>%
                      bs_embed_tooltip(title = "Please enter the annual fixed cost of the smoking cessation program")
                  )
              )
            )),
  wellPanel(
  helpText(h5('Weight management')),
  fluidRow(
    column(
      4,
      sliderInput(
        "wghtpct_slider_sc1",
        "Percentage of overweight and obese participants losing weight",
        0,
        100,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )  %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the percentage of overweight and obese participants losing weigth")
      ),
      sliderInput(
        "wghtreduc_slider_sc1",
        "Average weight loss as percentage weight", # equivalent to percentage of BMI
        0,
        20,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter average percentage reduction in BMI per person in a year, e.g. 5% reduction in BMI")
        )
    ),
    column(
      4,
      numericInput(
        "wghtloss_cost_sc1",
        "Weight management annual cost per participant losing weight",
        0,
        0,
        5e6,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per participant in the weight management program")
        )
    ),
    column(
      4,
      numericInput(
        "wghtloss_cost_ovrhd_sc1",
        "Weight management annual overhead costs",
        0,
       -5e6,
        5e6,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the annual fixed cost of the weight management program")
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
        "papct_slider_sc1",
        "Percentage of participants increasing their physical activity",
        0,
        100,
        0,
        0.5,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the percentage of participants increasing their physical activity levels")
      ),
      sliderInput(
        "papincr_slider_sc1",
        "Average increase in active days per week",
        0,
        7,
        0,
        1,
        sep = "",
        ticks = FALSE,
        post  = " %"
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the average increase in days of physical activity, e.g. 2 days")
        )
    ),
    column(
      4,
      numericInput(
        "pa_cost_sc1",
        "Physical activity annual cost per more active participant",
        0,
        0,
        5e6,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the cost per participant in the the physical activity program")
        )
    ),
    column(
      4,
      numericInput(
        "pa_cost_ovrhd_sc1",
        "Physical activity actions annual overhead costs",
        0,
       -5e6,
        5e6,
        1
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please enter the annual fixed cost of the physical activity program")
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
    h6("In this section you can specify the above scenario to be part of a serial or parallel ensemble."),
    column(6,
  switchInput(
    "serial_ensemble_checkbox_sc1",
    "This scenario is part of a serial ensemble",
    value = FALSE,
    onLabel = "Yes",
    offLabel = "No",
    labelWidth = "100%"
  )
  %>%
    shinyInput_label_embed(
      icon("info") %>%
        bs_embed_tooltip(title = "This allows you to run simultaneously different scenarios for different time spans, e.g scenario 1 from years 1-5 and scenario 2 from years 6-10")
    )
  )),
fluidRow(
    column(6,
           switchInput(
             "parallel_ensemble_checkbox_sc1",
             "This scenario is part of a parallel ensemble",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
           %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "This allows you to run simultaneously different scenarios for different groups of the population, e.g lifestyle interventions for the most deprived and more appoitments offered for the individuals at risk")
             )
    ),
    column(6,
    sliderInput(
      "parallel_ensemble_slider_sc1",
      "Percentage of population this scenario applies to",
      0,
      100,
      0,
      0.5,
      sep = "",
      ticks = FALSE,
      post  = " %"
    )
    %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Percentage of the population this scenario applies to if it is part of a parallel or sequential ensemble")
      )
    ))),
  wellPanel(fluidRow(
    h5("Digital Health Checks"),
    column(4,
           switchInput(
             "ignore_cholesterol_checkbox_sc1",
             "Ignore cholesterol in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
           %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "When calculating the Qrisk function this option allows you to exclude cholesterol estimates")
             )
    ),
    column(4,
           switchInput(
             "ignore_sbp_checkbox_sc1",
             "Ignore systolic blood pressure in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
           %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "When calculating the Qrisk function this option allows you to exclude systolic blood pressure estimates")
             )
    ),
    column(4,
           switchInput(
             "ignore_bmi_checkbox_sc1",
             "Ignore BMI in QRISK calculation",
             value = FALSE,
             onLabel = "Yes",
             offLabel = "No",
             labelWidth = "100%"
           )
           %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "When calculating the Qrisk function this option allows you to exclude BMI estimates")
             )
    )
    )),
  wellPanel(fluidRow(
    h5("Structural Policies / Calibration"),
    h6("In this section the effect of implementing structural/population level strategies in addition",
               "to the health checks programm specified above can be modelled. ",
      "Alternatively, this section can be used to adjust the model risk factors population model",
     "to match local estimates if data is available. "),
    column(6,
           sliderInput(
             "structural_smk_slider_sc1",
             "Smoking prevalence relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )
           %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the prevalence of smoking as result of a structural policy")
           ),
           sliderInput(
             "structural_fv_slider_sc1",
             "Fruit & veg relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the prevalence of fruit and vegetable consumption as result of a structural policy")
           ),
           sliderInput(
             "structural_alcohol_slider_sc1",
             "Alcohol relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the population mean of alcohol consumption as result of a structural policy")
           ),
           sliderInput(
             "structural_pa_slider_sc1",
             "Physical activity relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the prevalence of physical activity as result of a structural policy")
           ),
           sliderInput(
             "structural_bmi_slider_sc1",
             "BMI relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the population mean of BMI as result of a structural policy")
           ),
           sliderInput(
             "structural_sbp_slider_sc1",
             "Systolic blood pressure relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the population mean of systolic blood pressure as result of a structural policy")
           ),
           sliderInput(
             "structural_chol_slider_sc1",
             "Cholesterol relative percentage change across population",
             -20,
             20,
             c(0, 0),
             0.1,
             sep = "",
             ticks = FALSE,
             post  = " %"
           )  %>%
             shinyInput_label_embed(
               icon("info") %>%
                 bs_embed_tooltip(title = "Relative change (%) in the population mean of cholesterol as result of a structural policy")
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
                    "notes_sc1",
                    "",
                    "",
                    width = "100%",
                    rows = 6,
                    resize = "both"
                  )
                  %>%
                    shinyInput_label_embed(
                      icon("info") %>%
                        bs_embed_tooltip(title = "Add a description of this scenario or any important notes to remember")
                    )
                )))
  ),

conditionalPanel(condition = "input.level <= input.scenarios_number_slider",
                 fluidRow(
                 column(6,
                        actionButton(
                          "previous_sc1",
                          "Go to previous scenario",
                          icon = icon("step-backward"),
                          #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                          class = "btn btn-primary",
                          width = "100%"
                        )),
                 column(
                   6,
                   actionButton(
                     "next_sc1",
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
                     "run_simulation_sc1",
                     "Run simulation (all scenarios)",
                     icon = icon("paper-plane"),
                     #style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
                     class = "btn-info",
                     width = "100%"
                   )
                 )))



