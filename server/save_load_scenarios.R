
# From https://stackoverflow.com/questions/32922190/saving-state-of-shiny-app-to-be-restored-later
output$save_sc1 <- downloadHandler(


  filename = function()
    paste0(input$friendly_name_sc1, ".rds"),

  content = function(file) {
    updateCollapse(session, "collapse_panel_sc1",
    open = c("Eligibility Criteria",
             "Appointments Offered Yearly (%)",
             "Health Checks Received (%)",
             "Prescription Rate",
             "Impact on Lifestyle",
             "Advanced",
             "Notes"))
    Sys.sleep(2)
   tt_sc1 <- lapply(reactiveValuesToList(input), unclass)

    # tt_sc1 <- reactiveValuesToList(input)
    tt_sc1 <- tt_sc1[grep("^.*_sc1$", names(tt_sc1))]
    tt_sc1$save_sc1 <- NULL
      tt_sc1$load_sc1 <- NULL
      tt_sc1$collapse_panels_button_sc1 <- NULL
      tt_sc1$collapse_panel_sc1 <- NULL
      tt_sc1$run_simulation_sc1 <- NULL

    tt_sc1 <- setNames(tt_sc1, gsub("_sc1$", "", names(tt_sc1)))
    saveRDS(tt_sc1, file = file)
  }
)


  observeEvent(input$load_sc1, {
    updateCollapse(session, "collapse_panel_sc1",
                   open = c("Eligibility Criteria",
                            "Appointments Offered Yearly (%)",
                            "Health Checks Received (%)",
                            "Prescription Rate",
                            "Impact on Lifestyle",
                            "Advanced",
                            "Notes"))

    inFile <- input$load_sc1

    if (is.null(inFile)) return(NULL)
    savedInputs   <- readRDS(inFile$datapath)
    names(savedInputs)      <- paste0(names(savedInputs), "_sc1")

    if (savedInputs[["uptake_detailed_checkbox_sc1"]]) showElement(id = "uptake_table_sc1")
 if (savedInputs[["px_detailed_checkbox_sc1"]]) {
   showElement(id = "statin_px_table_sc1")
   showElement(id = "antihtn_px_table_sc1")
 }

# delay necessary so all elements are expanded on screen before load
    delay(2000, lapply(names(savedInputs),
           function(x) session$sendInputMessage(x, list(value = savedInputs[[x]]))
    ))
  })

# Ensure uptake & px is open and close once, otherwise the tables are not produced and they cannot be saved or load
observeEvent(input$collapse_panels_button_sc1, ({
  if (length(input$collapse_panel_sc1) > 1) {
    updateCollapse(session, "collapse_panel_sc1",
                   close = setdiff(input$collapse_panel_sc1, "General Parameters"))
  } else {
    updateCollapse(session, "collapse_panel_sc1",
                   open = c("General Parameters",
                            "Eligibility Criteria",
                            "Appointments Offered Yearly (%)",
                            "Health Checks Received (%)",
                            "Prescription Rate",
                            "Impact on Lifestyle"#,
                           # "Advanced",
                           # "Notes"
                            ))
  }
}))
