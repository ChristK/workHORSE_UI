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


# The choice of scenario's colours
  colours <- reactive({
      setnames(transpose(as.data.table(lapply(seq_len(input$scenarios_number_slider), function(i) {
        c(input[[paste0("col_sc", i)]],
          input[[paste0("friendly_name_sc", i)]])
      }))), c("colour", "names"))
  })

  
  
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
      color = "aqua"
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



 # user_inputs <- reactive(reactiveValuesToList(input))

  
  source(file.path("server", "output_inputs.R"),  local = TRUE)$value


# Tooltips ----------------------------------------------------------------
  # source(file.path("server", "tooltips.R"),  local = TRUE)$value


  # outputOptions(output, "nrows", suspendWhenHidden = FALSE)

}