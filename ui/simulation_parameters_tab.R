tabPanel(
  "Simulation parameters",
  wellPanel(fluidRow(
    column(
      6,
      pickerInput(inputId = "locality_select",
                  label = "Area to simulate",
                  choices = list(
                        "National" = list("England"),
                        "Strategic Health Authority/Region" = sort(as.character((localities_indx[, unique(SHA11NM)]))),
                        "Local Authority" = sort(as.character((localities_indx[, unique(LAD17NM)])))
                      ),
                    options = list(`actions-box` = TRUE, `live-search` = TRUE),
                  multiple = TRUE)
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the locality you are interested in simulating")
        )
    ),
    column(
      6,
      sliderInput(
        "simulation_period_slider",
        "Period to simulate",
        2011,
        2045,
        c(2017, 2035),
        1,
        sep = "",
        ticks = FALSE
      ) %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please select the start and end year of the simulation")
    ))
  )),

  wellPanel(fluidRow(
    column(
      6,
      sliderInput(
        "scenarios_number_slider",
        "Number of scenarios to be simulated",
        1,
        9,
        2,
        1,
        sep = "",
        ticks = FALSE
      )
      %>%
        shinyInput_label_embed(
          icon("info") %>%
            bs_embed_tooltip(title = "Please choose how many scenarios you want to simulate")
        )
    ),
    column(
      6,
      checkboxGroupInput(
        "scope_selector",
        "Simulation scope",
        choices = c("Effectiveness",
                    "Cost-effectiveness",
                    "Equity"),
        selected = c("Effectiveness",
                     "Cost-effectiveness",
                     "Equity")
      )
    )
  )),

  wellPanel(fluidRow(column(
    6,
    switchInput(
      "national_qimd_checkbox",
      # "Produce results by local IMD",
      value = TRUE,
      onLabel = "National IMD",
      offLabel = "Local IMD",
      offStatus = "danger",
      width = "100%"
    )
    %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please choose to use either the local or the national Index of Multideprivation (IMD)")
      ))
  ,
  column(
    6,
    switchInput(
      "ward_output_checkbox",
      "Produce results by Ward",
      value = FALSE,
      onLabel = "Yes",
      offLabel = "No",
      labelWidth = "100%"
    )
    %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please choose if you would like the results to be presented at the Ward level")
      ))))
)
