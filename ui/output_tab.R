tabPanel(
  title = "Output",
  value = "output_panel",
  dashboardPage(
    dashboardHeader(disable = TRUE), # Misbehaves when framed in a tabPanel
    dashboardSidebar(
      tags$head(
        tags$style(HTML("right-side,
                        .sidebar-toggle {height: 100vh; overflow: hidden;}
                      " )
        )),
      sidebarMenu(
        id = "out_tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Cost-effectiveness", icon = icon("pound-sign"), tabName = "cost_effectiveness"
        ),
        menuItem("Health Inequalities", icon = icon("balance-scale"), tabName = "equity"
        ),
        menuItem("Effectiveness", icon = icon("heart"), tabName = "effectiveness"
        ),
        menuItem("Wider Social Benefits", icon = icon("users"), tabName = "wider_social_benef"
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
            tags$head(tags$style(
              "html, body {overflow: visible; }")
            ),
            box(

              title = "Cost-effectiveness plane",
              solidHeader = TRUE,
              collapsible = FALSE,
              fluidRow(
              column(1,
              bs_modal(id = "modal_cep", title = "Explanation of this cost-effectiveness plane", body = textOutput("info_ce_plane")),
              shiny_iconlink() %>%
              bs_attach_modal(id_modal = "modal_cep")),
              #var_out_proc_use_ui("plot1"),
              #var_tt_creation_ui("plot1_tt"),

              column(10, offset = 1,
              plotlyOutput("cep1")),
              column(1, offset = 10,
                     dropdown(
                       "Result display",
                       switchInput(
                         "res_display_cep1",
                         "Mean values",
                         FALSE,
                         onLabel = "Yes",
                         offLabel = "No"),

                       style = "unite", status = "primary", size = "sm",
                       icon = icon("gear", class = "fa-xs"), width = "200px"
                       #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                     ))
            )) ,
            box(
              title = "Equity plane",
              solidHeader = TRUE,
              collapsible = FALSE,
              fluidRow(
              column(1,
              bs_modal(id = "modal_equ", title = "Explications of this equity plane", body = textOutput("info_equ_plane")),
              shiny_iconlink() %>%
              bs_attach_modal(id_modal = "modal_equ")),
              column(10, offset = 1,
              plotlyOutput("equ1")),
              column(1, offset = 10,
                     dropdown(
                       "Result display",
                       switchInput(
                         "res_display_equ1",
                         "Mean values",
                         FALSE,
                         onLabel = "Yes",
                         offLabel = "No"),
                       style = "unite", status = "primary", size = "sm", right = TRUE,
                       icon = icon("gear", class = "fa-xs"), width = "200px" 
                       #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                     ))
            )
          ),
          fluidRow(
            box(
              title = "Explications",
              solidHeader = TRUE,
              collapsible = FALSE,
              width = 12,
              uiOutput("automated_text_descr")
            )
          )
        ))),
        tabItem("cost_effectiveness",
                fluidRow(
                  box(
                    title = "Cost-effectiveness plane",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    fluidRow(
                    column(1,
                    bs_modal(id = "modal_cep1_1", title = "Explications of this Cost-effectiveness plane", body = textOutput("info_ce1_plane")),
                    shiny_iconlink() %>%
                    bs_attach_modal(id_modal = "modal_cep1_1")),
                    column(10, offset = 1,
                    plotlyOutput("cep1_1")),
                    column(1, offset = 10,
                           dropdown(
                             "Result display",
                             switchInput(
                               "res_display_cep1_1",
                               "Mean values",
                               FALSE,
                               onLabel = "Yes",
                               offLabel = "No"),
                             
                             style = "unite", status = "primary", size = "sm",
                             icon = icon("gear", class = "fa-xs"), width = "200px"
                             #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                           ))
                  )),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    height = 505,
                    uiOutput("note_cost_eff")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "Probability of",
                    side = "right",
                    selected = "cost-effective policy",
                    id = "out_cep_p",
                    tabPanel("cost saving policy", bs_modal(id = "modal_cep_p_cs", title = "Explications of this Cost saving policy plane", body = textOutput("info_cepcs_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cep_p_cs"),
                               plotlyOutput("cep_p_cs")
                             ),
                    tabPanel("cost-effective policy", bs_modal(id = "modal_cep_p_ce", title = "Explications of this Cost-effective policy plane", body = textOutput("info_cepce_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cep_p_ce"), plotlyOutput("cep_p_ce"))
                  ),
                  box(
                    title = "Cost-effectiveness over time",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    bs_modal(id = "modal_cep_anim", title = "Explications of this Cost-effectiveness over time plane", body = textOutput("info_cep_anim_plane")),
                    shiny_iconlink() %>%
                    bs_attach_modal(id_modal = "modal_cep_anim"),
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
                    tabPanel("relative health inequalities",
                             fluidRow(
                               column(1,
                                      bs_modal(id = "modal_equ_rel", title = "Explications of this relative health inequalities plane concerning equity", body = textOutput("info_equ_rel_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ_rel")),
                             column(10, offset = 1,
                             plotlyOutput("equ_rel")),
                             column(1, offset = 10,
                                    dropdown(
                                      "Result display",
                                      switchInput(
                                        "res_display_equ_rel",
                                        "Mean values",
                                        FALSE,
                                        onLabel = "Yes",
                                        offLabel = "No"),
                                      style = "unite", status = "primary", size = "sm",
                                      icon = icon("gear", class = "fa-xs"), width = "200px"
                                      #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                                    )))),
                    tabPanel("absolute health inequalities",
                             fluidRow(
                               column(1,
                                      bs_modal(id = "modal_equ1_1", title = "Explications of this absolute health inequalities plane concerning equity", body = textOutput("info_equ1_1_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ1_1")),
                             column(10, offset = 1,
                             plotlyOutput("equ1_1")),
                             column(1, offset = 10,
                                    dropdown(
                                      "Result display",
                                      switchInput(
                                        "res_display_equ1_1",
                                        "Mean values",
                                        FALSE,
                                        onLabel = "Yes",
                                        offLabel = "No"),
                                      style = "unite", status = "primary", size = "sm",
                                      icon = icon("gear", class = "fa-xs"), width = "200px"
                                      #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                                    )))
                  )),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    height = 540,
                    uiOutput("note_health_ineq")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "Probability of equitable policy",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_p",
                    tabPanel("relative health inequalities", bs_modal(id = "modal_equ_p_rel", title = "Explications of this relative health inequalities plane concerning equitable policy", body = textOutput("info_equ_p_rel_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ_p_rel"), plotlyOutput("equ_p_rel")),
                    tabPanel("absolute health inequalities", bs_modal(id = "modal_equ_p_abs", title = "Explications of this absolute health inequalities plane concerning equitable policy", body = textOutput("info_equ_p_abs_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ_p_abs"), plotlyOutput("equ_p_abs"))
                  ),
                  tabBox(
                    title = "Equity over time",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_anim",
                    tabPanel("relative health inequalities", bs_modal(id = "modal_equ_anim_rel", title = "Explications of this relative health inequalities plane concerning equity over time", body = textOutput("info_equ_anim_rel_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ_anim_rel"), plotlyOutput("equ_anim_rel")),
                    tabPanel("absolute health inequalities", bs_modal(id = "modal_equ_anim_abs", title = "Explications of this absolute health inequalities plane concerning equity over time", body = textOutput("info_equ_anim_abs_plane")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_equ_anim_abs"), plotlyOutput("equ_anim_abs"))
                  )
                )
        ),
        tabItem("effectiveness",
                fluidRow(
                  tabBox(
                    title = "Prevented or Postponed",
                    side = "right",
                    selected = "Case-years",
                    id = "out_pp_plane",
                    tabPanel("Deaths",
                     bs_modal(id = "modal_dpp_1", title = "Explications", body = textOutput("info_dpp_1_chart")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_dpp_1"), plotlyOutput("dpp_1")),
                    tabPanel("Case-years"
                    , bs_modal(id = "modal_cppy_1", title = "Explications", body = textOutput("info_cppy_1_chart")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cppy_1"), plotlyOutput("cppy_1")),
                    tabPanel("Cases"
                             , bs_modal(id = "modal_cpp_1", title = "Explications", body = textOutput("info_cpp_1_chart")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cpp_1"), plotlyOutput("cpp_1"))
                  ),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    height = 525,
                    uiOutput("note_effvnss")
                    #p("Please do not use these results for any real-life application")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "Prevented or Postponed per year",
                    side = "right",
                    selected = "Case-years",
                    id = "out_ppy_plane",
                    tabPanel("Deaths",
                             
                                      bs_modal(id = "modal_dppy_spline", title = "Explications", body = textOutput("info_dppy_spline")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_dppy_spline"), plotlyOutput("dppy_spline")
                    ),
                    tabPanel("Case-years",
                             fluidRow(
                               column(1,
                                      bs_modal(id = "modal_cppy_spline", title = "Explications", body = textOutput("info_cppy_spline")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cppy_spline")),
                             column(10, offset = 1,
                             plotlyOutput("cppy_spline")
                             ),
                             column(1, offset = 10,
                                    dropdown(checkboxGroupButtons(inputId = "out_diseases_select",
                                                         label = "Diseases",
                                                         choices = list("Coronary heart disease" = "cpp_chd_cml",
                                                                        "Stroke" = "cpp_stroke_cml",
                                                                        "Diabetes" = "cpp_t2dm_cml",
                                                                        "Lung cancer" = "cpp_lc_cml",
                                                                        "Atrial fibrillation" = "cpp_af_cml"),
                                                         selected = list("cpp_chd_cml",
                                                                         "cpp_stroke_cml",
                                                                         "cpp_t2dm_cml",
                                                                         "cpp_lc_cml",
                                                                         "cpp_af_cml"),
                                                         size = "xs", direction = "vertical", width = "100px")  
                                             
                                             ,
                                      style = "unite", status = "primary", size = "sm", up = TRUE,
                                      icon = icon("gear", class = "fa-xs"), width = "200px"
                                      #tooltip = tooltipOptions(title = "Click to chose the way you want to see the results !")
                                    )))),
                    tabPanel("Cases", bs_modal(id = "modal_cpp_spline", title = "Explications", body = textOutput("info_cpp_spline")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_cpp_spline"), plotlyOutput("cpp_spline_1"))
                  ),
                  tabBox(
                    title = "Prevented or Postponed per year",
                    side = "right",
                    selected = "Case-years",
                    id = "case_years_staked_graph",
                    tabPanel("Case-years",
                             fluidRow(
                               column(1,
                                      bs_modal(id = "modal_case_years", title = "Explications", body = textOutput("info_case_years_stack")),
                             shiny_iconlink() %>%
                               bs_attach_modal(id_modal = "modal_case_year")),
                             column(10, offset = 1,
                             plotlyOutput("cpp_stacked_area")),
                             column(1, offset = 10,
                                    dropdown(uiOutput("out_scenario_disease_select"),
                                             style = "unite", status = "primary", size = "sm", right = TRUE, up = TRUE,
                                             icon = icon("gear", class = "fa-xs"), width = "200px"
                                    )))
                             
                  ),
                  tabPanel("Cases")
                           # , bs_modal(id = "modal_cpp_1", title = "Explications", body = textOutput("info_cpp_1_chart")),
                           # shiny_iconlink() %>%
                           #   bs_attach_modal(id_modal = "modal_cpp_1"), plotlyOutput("cpp_1"))
                  
                )
        )),
        tabItem("wider_social_benef",
                fluidRow(
                  tabBox(
                    title = "1st plane",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_plane",
                    tabPanel("A 1st tab"),
                    # , bs_modal(id = "modal_equ_rel", title = "Explications of this relative health inequalities plane concerning equity", body = textOutput("info_equ_rel_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ_rel"), plotlyOutput("equ_rel")),
                    tabPanel("A 2nd tab")
                    # , bs_modal(id = "modal_equ1_1", title = "Explications of this absolute health inequalities plane concerning equity", body = textOutput("info_equ1_1_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ1_1"), plotlyOutput("equ1_1"))
                  ),
                  box(
                    title = "Notes",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    uiOutput("note_social_benef")
                    #p("Please do not use these results for any real-life application")
                  )
                ),
                fluidRow(
                  tabBox(
                    title = "2nd plane",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_p",
                    tabPanel("A 1st tab"),
                    # , bs_modal(id = "modal_equ_p_rel", title = "Explications of this relative health inequalities plane concerning equitable policy", body = textOutput("info_equ_p_rel_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ_p_rel"), plotlyOutput("equ_p_rel")),
                    tabPanel("A 2nd tab")
                    # , bs_modal(id = "modal_equ_p_abs", title = "Explications of this absolute health inequalities plane concerning equitable policy", body = textOutput("info_equ_p_abs_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ_p_abs"), plotlyOutput("equ_p_abs"))
                  ),
                  tabBox(
                    title = "3rd plane",
                    side = "right",
                    selected = "absolute health inequalities",
                    id = "out_equ_anim",
                    tabPanel("A 1st tab"),
                    # , bs_modal(id = "modal_equ_anim_rel", title = "Explications of this relative health inequalities plane concerning equity over time", body = textOutput("info_equ_anim_rel_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ_anim_rel"), plotlyOutput("equ_anim_rel")),
                    tabPanel("A 2nd tab")
                    # , bs_modal(id = "modal_equ_anim_abs", title = "Explications of this absolute health inequalities plane concerning equity over time", body = textOutput("info_equ_anim_abs_plane")),
                    #          shiny_iconlink() %>%
                    #            bs_attach_modal(id_modal = "modal_equ_anim_abs"), plotlyOutput("equ_anim_abs"))
                  )
                )
                #div(p("Dashboard tab content"))
        ),
        tabItem("subitem1",
                  box(
                    title = "Detailed results", solidHeader = TRUE,
                    collapsible = TRUE,
                    column(6, offset = 6, uiOutput("out_columns_select")),
                   column(12,
                    DT::dataTableOutput('tbl')
                    ))

        ),
        tabItem("subitem2",
                "Sub-item 2 tab content"
        )
      )
)
))

