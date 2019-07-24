server = function(input, output, session) {
# Dynamic scenario tab addition -------------------------------------------
  output$scenario_tabs <- renderUI({
    Tabs <- as.list(rep(0, input$scenarios_number_slider))
    for (i in 0:length(Tabs)) {
      Tabs[i] <- lapply(paste0("Scenario ", i), tabPanel, value = i)
    }

    #Tabs <- lapply(paste("Layer",0:input$subClust,sep=" "), tabPanel)
    do.call(tabsetPanel, c(Tabs, id = "level"))
  })


# Reactive fucntion to choose scenario colours and shapes, and use their value according to their name

  colours <- reactive({
      setnames(transpose(as.data.table(lapply(seq_len(input$scenarios_number_slider), function(i) {
        c(input[[paste0("col_sc", i)]],
          input[[paste0("friendly_name_sc", i)]],
          input[[paste0("symbol_sc", i)]])
      }))), c("colour", "names", "symbol"))
  })


 


  # firstFrame <- vapplyseq_len(input$scenarios_number_slider), function(i) isTRUE(tr[["frame"]] %in% frameNames[[1]]), logical(1))
  # input$scenarios_number_slider[firstFrame] <- p$x$frames[[1]]$data

  #p$x$data <- c(c(p$x$data, colours), NULL)

  # callModule(module = createPlot, id = "cep1")




  observeEvent(input$scenarios_number_slider, {
# Render uptake/Px tables -------------------------------------------------
    for (i in seq_len(input$scenarios_number_slider)) {
      source(exprs = parse(text = gsub("_sc1",  paste0("_sc", i),
                                      readLines(file.path("server", "render_scenario_tables.R")))), local = TRUE
             )$value
    }

    # Alter ,min/max scenario init year -----------------------------------------
    for (i in seq_len(input$scenarios_number_slider)) {
      source(exprs = parse(text = gsub("_sc1",  paste0("_sc", i),
                                      readLines(file.path("server", "dynamic_scenario_minmax_year.R")))), local = TRUE
             )$value
    }

    # Load/save scenario  -----------------------------------------------------
    for (i in seq_len(input$scenarios_number_slider)) {
      source(exprs = parse(text = gsub("_sc1",  paste0("_sc", i),
                                      readLines(file.path("server", "save_load_scenarios.R")))), local = TRUE
             )$value
    }

    # Load tooltips ----
    # for (i in seq_len(input$scenarios_number_slider)) {
    #   source(file.path("server", paste0("tooltips_sc", i, ".R")),  local = TRUE)$value
    # }

    # Conditionally disable inputs -------
    for (i in seq_len(input$scenarios_number_slider)) {
      source(exprs = parse(text = gsub("_sc1",  paste0("_sc", i),
                                       readLines(file.path("server", "show_hide_logic.R")))), local = TRUE
      )$value
    }

    # Prev/Next buttons -------
    for (i in seq_len(input$scenarios_number_slider)) {
      source(exprs = parse(text = gsub("_sc1",  paste0("_sc", i),
                                       readLines(file.path("server", "prev_next_buttons.R")))), local = TRUE
      )$value
    }

    #Logic for color picker in scenarios
    #for (i in seq_len(input$scenarios_number_slider)) {
     # n <- input$scenarios_number_slider
      #def_col <- viridis(9, option = "D")
      #def_col_small <- def_col[seq(1, length(def_col))]
     # tt <- readLines(file.path("ui", "scenario1_tab.R"))
     # tt2 <- readLines(file.path("ui", "scenario2_tab.R"))

     # if(input$scenarios_number_slider == 2) {
       # observeEvent (input$col_sc1 ,{
         # output$colour_sc1 <- renderUI({
          #  input$col_sc1 <- first(def_col)
         # })
         # })
       # gsub("def_col_small[[1]]", def_col, tt)
      #  gsub("def_col_small[[2]]", last(def_col), tt2)
        #observeEvent (input$col_sc2 ,{
          #output$colour_sc2 <- renderUI({
           # input$col_sc2 <- last(def_col)
         # })
      #  })
    #  }
   # }
  },
  ignoreNULL = FALSE, ignoreInit = FALSE)

# Run simulation ----
  out <- eventReactive(input[[paste0("run_simulation_sc", input$scenarios_number_slider)]], {
    # Create a Progress object
    # progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(progress$close())
    # progress$set(message = "Running metamodel", value = 0)

    parameters <- lapply(reactiveValuesToList(input), unclass)
    parameters <- data.table("input_names" = names(parameters), "value" = parameters, key = "input_names")
    parameters[, scenario := tstrsplit(input_names, "_sc", fixed = TRUE, keep = 2L)]
    parameters[!is.na(scenario), scenario := paste0("sc", scenario)]
    parameters[is.na(scenario), scenario := "global"]
    parameters[, input_names := gsub("_sc\\d", "", input_names)]
    setkey(parameters, input_names, scenario)
    setcolorder(parameters)
    parameters <- parameters[lapply(lapply(value, `[`), length) > 0, ]
    tt <- parameters["scenarios_number_slider", value[[1]]]
    parameters <- parameters[scenario %in% c("global", paste0("sc", seq_len(tt))), ]
    parameters <- parameters[!input_names %like% "shinyjs-delay-"]

    # saveRDS(parameters, file.path("output", "input_parameters.rds"))
    # parameters["simulation_period_slider",]$global[[1]][[1]]

    # progress$inc(1/n, detail = paste("Doing part", i))
    withProgress(message = 'Running metamodel',
                 detail = 'This may take a while...', value = 0, {
    predict_metamodel(parameters)
                 })
              })
  # change to output panel
observeEvent(input[[paste0("run_simulation_sc", input$scenarios_number_slider)]], {
  updateTabsetPanel(session, "inTabset",
                    selected = "output_panel")
})








# Output tab --------------------------------------------------------
  output$most_cost_effective_box <- renderInfoBox({
    infoBox(
      "Most Cost-Effective", most_cost_effective(out_proc()), icon = icon("pound-sign"),
      color = "aqua", fill = FALSE
    )

  })


  output$most_equitable_box <- renderInfoBox({
    infoBox(
      "Most Equitable", most_equitable(out_proc()), icon = icon("balance-scale"),
      color = "purple"
    )
  })

  output$most_effective_box <- renderInfoBox({
    infoBox(
      "Most Effective", most_effective(out_proc()), icon = icon("heart"),
      color = "yellow"
    )
  })


  diff_year <- function() {
    diff_year <- input$inout_year_slider - input$simulation_period_slider[1]
  }

  
# Automated graph explanation text  ------------------------------------------------------
  
   output$automated_text_descr <- renderUI({
     HTML(paste0("With time horizon of ", diff_year()," year/s, starting in year ", input$simulation_period_slider[1], ".", br(), br(),
     "The most cost effective scenario was ", most_benefit_cost_ratio(out_proc()), " scenario",
            " which had a benefit:cost ratio (including the value of health gains) of £", benefit_cost_ratio(out_proc())," for every £1 spent.", br(), br(),
     "This scenario had a societal incremental cost effectiveness ratio of ", incr_cost_effect_ratio(),
            " per QALY when compared to the next best scenario. " , br(), br(), "Ranking the scenarios, the most cost effective was ", most_cost_effective(out_proc()), " scenario while the least cost effective was ", last(rank_cost_effective(out_proc())), " scenario."))
   })


   output$note_cost_eff <- renderUI({
     HTML(paste0("With time horizon of ", diff_year()," year/s, starting in year ", input$simulation_period_slider[1], ": and based on valuing QALYs at ", input$out_wtp_box,
                 ", the scenario with the greatest societal benefit:cost ratio was ", most_equitable(out_proc()), " scenario which had a benefit: cost ratio of ", benefit_cost_ratio(out_proc()),
                 " for every £1 spent and a total net monetary benefit of £", tot_net_monet_benef(out_proc()), " from a societal perspective.", br(), br(),
                 "This scenario had a societal incremental cost effectiveness ratio of ", incr_cost_effect_ratio(), " per QALY when compared to the next best scenario ", rank_cost_effective(out_proc())[2], " scenario.", br(), br(),
                 "The most cost effective scenario from a healthcare cost perspective was ", most_cost_effective(out_proc()), " scenario which had an incremental cost effectiveness ratio of ", incr_cost_effect_ratio(),
                 " per QALY gained.", br(), "Ranking the scenarios from most to least cost effective, the order was:", br(), br(),
                 paste(head(rank_cost_effective(out_proc()), -1), collapse =", "), last_rank()
     ))
   })

   #TODO: update with new functions for absolute and relative equitable

   output$note_health_ineq <- renderUI({
     HTML(paste0(most_equitable_rel(out_proc()), " scenario had the biggest impact in terms of reducing relative inequalities, reducing the relative index of inequalities by ", reduce_rel_index_ineq(), ".", br(), br(),
                 last(most_equitable_abs(out_proc())), " scenario did the least harm to absolute inequalities, increasing the absolute index of inequalities by ", increase_abs_index_ineq(), "."))

   })


   output$note_effvnss <- renderUI({
     HTML(paste0("The most effective scenario in terms of disease cases prevented and postponed was ", most_effective(out_proc()), " scenario which prevented '...' total disease cases,
                 made up of '...' cases of CVD, and '...' cases of other diseases"))
   })


   output$note_social_benef <- renderUI({
     HTML(paste0("As well as healthcare benefits, ", most_equitable(out_proc())," scenario would produce additional social care cost savings of ",
                 social_care_cost_sav(), ", productivity benefits of ", prod_benef(), " (including earnings
and the value of household productivity) and informal care cost savings of", inform_care_cost_sav(), br()))
   })

 output$info_ce_plane <- renderText({
   "Needs to be implemented"
 })

 output$info_equ_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_ce1_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_cepcs_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_cepce_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_cep_anim_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_equ_rel_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_equ1_1_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_equ_p_rel_plane <- renderText({
   "The future explications for this specific graph"
 })

 output$info_equ_p_abs_plane <- renderText({
   "The future explications for this specific graph"
 })

output$info_equ_anim_rel_plane <- renderText({
  "The future explications for this specific graph"
})

output$info_equ_anim_abs_plane <- renderText({
  "The future explications for this specific graph"
})

 # user_inputs <- reactive(reactiveValuesToList(input))


  source(file.path("server", "output_inputs.R"),  local = TRUE)$value


# Tooltips ----------------------------------------------------------------
  # source(file.path("server", "tooltips.R"),  local = TRUE)$value


  # outputOptions(output, "nrows", suspendWhenHidden = FALSE)


last_rank <- function(){
  lr <- rank_cost_effective(out_proc())[length(rank_cost_effective(out_proc()))]
  lr <- paste0(" and ", last(rank_cost_effective(out_proc())))
}

}
