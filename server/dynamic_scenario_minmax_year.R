output$init_year_slider_sc1 <- renderUI({
  tagList(
    sliderInput(
      "ininit_year_slider_sc1",
      "First year of implementation",
      input$simulation_period_slider[[1]], input$simulation_period_slider[[2]], 2014, 1,
      sep = "",
      ticks = FALSE
    ) %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please select the first year of implementation for this scenario.
                             If this scenario is part of serial scenarion ensemble,
                             then the last year of implementation for this scenario
                             will up to the first year of implementation for the next scenario in the ensemble.")
      )
  )
})
