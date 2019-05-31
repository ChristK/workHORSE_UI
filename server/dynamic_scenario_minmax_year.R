output$init_year_slider_sc1 <- renderUI({
  tagList(
    sliderInput(
      "ininit_year_slider_sc1",
      "First year of implementation",
      input$simulation_period_slider[[1]], input$simulation_period_slider[[2]], 2014, 1,
      sep = "",
      ticks = FALSE
    )
  )
})
