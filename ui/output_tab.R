tabPanel(
  title = "Output",
  value = "output_panel",
  dashboardPage(
    dashboardHeader(disable = TRUE), # Misbehaves when framed in a tabPanel
    dashboardSidebar(
      sidebarMenu(
        id = "out_tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Cost-effectiveness", icon = icon("pound-sign"), tabName = "cost_effectiveness"
        ),
        menuItem("Health Inequalities", icon = icon("balance-scale"), tabName = "equity"
        ),
        menuItem("Effectiveness", icon = icon("heart"), tabName = "effectiveness"
        ),
        menuItem("Detailed Outputs", icon = icon("table"),
                 menuSubItem("Sub-item 1", tabName = "subitem1"),
                 menuSubItem("Sub-item 2", tabName = "subitem2")
        ),
        menuItem("Filters",
                 uiOutput("out_year_slider"),
                 uiOutput("out_scenario_select"),
                 pickerInput(inputId = "out_characteristics_select",
                             label = "Sub-population",
                             choices = list(
                               "Age groups" = c("30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                               "Sex"        = c("Men", "Women"),
                               "QIMD"       = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
                               "Ethnicity"  = c(
                                 "white",
                                 "indian",
                                 "pakistani",
                                 "bangladeshi",
                                 "other asian",
                                 "black caribbean",
                                 "black african",
                                 "chinese",
                                 "other"
                               )
                             ),
                             selected = c(
                               "30-39", "40-49", "50-59", "60-69", "70-79", "80+",
                               "Men", "Women",
                               "1 (most deprived)", "2", "3", "4", "5 (least deprived)",
                               "white",
                               "indian",
                               "pakistani",
                               "bangladeshi",
                               "other asian",
                               "black caribbean",
                               "black african",
                               "chinese",
                               "other"
                             ),
                             options = list(`actions-box` = TRUE, `live-search` = TRUE),
                             multiple = TRUE)  %>%
                   shinyInput_label_embed(
                     icon("info") %>%
                       bs_embed_tooltip(title = "Please select the parameters you want to apply to the scenarios and see the differences in the tab.")
                   ),
                 icon = icon("filter")),
        menuItem("Health Economics",
                 sliderInput(
                   "out_discount_cost_slider",
                   "Annual discount rate for costs",
                   0,
                   10,
                   3.5,
                   0.5,
                   sep = "",
                   ticks = FALSE,
                   post  = " %"
                 ),
                 sliderInput(
                   "out_discount_qalys_slider",
                   "Annual discount rate for QALYs",
                   0,
                   10,
                   1.5,
                   0.5,
                   sep = "",
                   ticks = FALSE,
                   post  = " %"
                 ),
                 numericInput("out_wtp_box",
                              "Willingness to pay (Cost per QALY)", 2e4L, 0, 1e5L, 1e3L),
                 icon = icon("toolbox")
        ),
        actionButton(
          "produce_report",
          "Produce report",
          icon = icon("file-contract"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
          #class = "btn-info",
          width = "auto"
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          "dashboard", fluidPage(
          fluidRow(
            infoBoxOutput("most_cost_effective_box"),
            infoBoxOutput("most_equitable_box"),
            infoBoxOutput("most_effective_box")
          ),
          fluidRow(
            box(
              title = "Cost-effectiveness plane",
              solidHeader = TRUE,
              collapsible = FALSE,
              plotlyOutput("cep1")
            ),
            box(
              title = "Equity plane",
              solidHeader = TRUE,
              collapsible = FALSE,
              plotlyOutput("equ1")
            )
          ),
          fluidRow(
            box(
              title = "Explications",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = 12,
              textOutput("automated_text_descr")
            )
          )
        )),
        tabItem("cost_effectiveness",
                fluidRow(
                  box(
                    title = "Cost-effectiveness plane",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    plotlyOutput("cep1_1")
                  ),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    p("Placeholder for dynamic notes"),
                    p("Please do not use these results for any real-life application")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "Probability of",
                    side = "right",
                    selected = "cost-effective policy",
                    id = "out_cep_p",
                    tabPanel("cost saving policy", plotlyOutput("cep_p_cs")),
                    tabPanel("cost-effective policy", plotlyOutput("cep_p_ce"))
                  ),
                  box(
                    title = "Cost-effectiveness over time",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    plotlyOutput("cep_anim")
                  )
                )),
        tabItem("equity",
                fluidRow(
                  tabBox(
                    title = "Equity planes",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_plane",
                    tabPanel("relative health inequalities", plotlyOutput("equ_rel")),
                    tabPanel("absolute health inequalities", plotlyOutput("equ1_1"))
                  ),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    p("Placeholder for dynamic notes"),
                    p("Please do not use these results for any real-life application")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "Probability of equitable policy",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_p",
                    tabPanel("relative health inequalities", plotlyOutput("equ_p_rel")),
                    tabPanel("absolute health inequalities", plotlyOutput("equ_p_abs"))
                  ),
                  tabBox(
                    title = "Equity over time",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_anim",
                    tabPanel("relative health inequalities", plotlyOutput("equ_anim_rel")),
                    tabPanel("absolute health inequalities", plotlyOutput("equ_anim_abs"))
                  )
                )
        ),
        tabItem("effectiveness",
                div(p("Dashboard tab content"))
        ),
        tabItem("subitem1",
                box(
                  title = "Detailed results", solidHeader = TRUE,
                  collapsible = TRUE,
                  DTOutput('tbl')
                )
        ),
        tabItem("subitem2",
                "Sub-item 2 tab content"
        )
      )
      )
  )
)
