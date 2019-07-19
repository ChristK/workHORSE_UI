
output$out_year_slider <- renderUI({
  tagList(
    sliderInput(
      "inout_year_slider",
      "Year",
      input$simulation_period_slider[[1]], input$simulation_period_slider[[2]],
      input$simulation_period_slider[[2]], 1,
      sep = "",
      ticks = FALSE
    ) %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please drag the slider to change the end date of the scenarios and see the differences.")
      )
  )
})

observe({
      hlp_frienly_names <- list()
      for (i in seq_len(input$scenarios_number_slider)) {

        if (!input[[paste0("baseline_sc", i)]] && !input[[paste0("parallel_ensemble_checkbox_sc", i)]]) {

         # hlp_frienly_names[link()$n] <- link()$n
           hlp_frienly_names[[input[[paste0("friendly_name_sc", i)]]]] <-  input[[paste0("friendly_name_sc", i)]]

        } else if (!input[[paste0("baseline_sc", i)]] && input[[paste0("parallel_ensemble_checkbox_sc", i)]]) {

          hlp_frienly_names[[paste0(input[[paste0("friendly_name_sc", i)]], "_ens")]] <-
            paste0(input[[paste0("friendly_name_sc", i)]], "_ens")
          }
      }

       updatePickerInput(session, "inout_scenario_select", choices = hlp_frienly_names, selected = hlp_frienly_names)

    })


output$out_scenario_select <- renderUI({
  tagList(
    pickerInput(inputId = "inout_scenario_select",
                label = "Scenarios",
                choices = c(paste0("Scenario ", seq_len(input$scenarios_number_slider))),
                selected = c(paste0("Scenario ", seq_len(input$scenarios_number_slider))),
                options = list(`actions-box` = TRUE, `live-search` = FALSE),
                multiple = TRUE)    %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please select the scenario(s) you want to remove from the graphs.")
      )
    )
})


out_proc <- reactive({
  # if (!exists("out")) out <- read_fst("./metamodel/out.fst", as.data.table = TRUE)
  # out <- read_fst(file.path("output", "out.fst"), as.data.table = TRUE)
  out()[year <= input$inout_year_slider &
        friendly_name %in% input$inout_scenario_select,][, `:=` (
          net_utility = net_utility * ((100 - input$out_discount_qalys_slider) / 100) ^ (year - 2017),
          net_cost = net_cost * ((100 - input$out_discount_cost_slider) /
                                   100) ^ (year - 2017),
          intervention_cost = (annual_coverage_cost + annual_uptake_cost) * ((100 - input$out_discount_cost_slider) /
                                                                               100) ^ (year - 2017)
        )][,
           `:=` (
             cpp_chd_cml           = round(cumsum(cpp_chd)),
             cpp_stroke_cml        = round(cumsum(cpp_stroke)),
             cpp_t2dm_cml          = round(cumsum(cpp_t2dm)),
             cpp_af_cml            = round(cumsum(cpp_af)),
             cpp_lc_cml            = round(cumsum(cpp_lc)),
             dpp_chd_cml           = round(cumsum(dpp_chd)),
             dpp_stroke_cml        = round(cumsum(dpp_stroke)),
             dpp_other_cml         = round(cumsum(dpp_other)),
             net_utility_cml       = cumsum(net_utility),
             net_cost_cml          = cumsum(net_cost),
             intervention_cost_cml = cumsum(intervention_cost)
           ),
           by = .(.id, friendly_name, qimd)][, `:=` (nmb_cml = net_utility_cml * input$out_wtp_box - net_cost_cml)]
  })



# CE plane
output$cep1_1 <- renderPlotly({
  
  tt <-       out_proc()[year == max(year), ][, .(
    net_utility_cml = sum(net_utility_cml),
    net_cost_cml    = sum(net_cost_cml)
  ),
  by = .(.id, friendly_name)]
  
  max_x <- tt[, max(abs(net_utility_cml))] * 1.2
  wtp_thres <- reactive(max_x * input$out_wtp_box)
  max_y <-  max(tt[, max(abs(net_cost_cml))] * 1.2, wtp_thres())
  trng_path <- paste0("M 0 0 L ",  max_x, " ", wtp_thres(), " L ", max_x, " 0 Z")
  
  
  # TODO separate this code from this specific graph because it is universal. Move it somewhere it is obviously universal
  # TODO synchronise colours and graphs with the scenario selection on the left side bar
  
  if (input$res_display_cep1_1) tt <- tt[, lapply(.SD, mean), keyby = .(friendly_name)]
  
  p <-
    plot_ly(
      tt, 
      x = ~ net_utility_cml,
      y = ~ net_cost_cml,
      color = ~ friendly_name,
      colors =  colours()[names %in% input$inout_scenario_select, colour],
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  
  p <-
    layout(
      p,
      yaxis = list(title = "Incremental cumulative cost (£)"),
      xaxis = list(title = "Incremental cumulative effects (QALYS)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y"),
        list(type = "path",
             fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
             layer = "below",
             path = trng_path),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.2,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = max_y, y1 = -max_y, yref = "y")
      ))
  
  # p <- animation_opts(p, frame = 1000, redraw = FALSE)
  # p <- animation_slider(p,
  #                       currentvalue = list(prefix = "Year: ",
  #                                           font = list(color = "red")))
  
})
  
  output$cep1 <- renderPlotly({

  tt <-       out_proc()[year == max(year), ][, .(
    net_utility_cml = sum(net_utility_cml),
    net_cost_cml    = sum(net_cost_cml)
  ),
  by = .(.id, friendly_name)]

  max_x <- tt[, max(abs(net_utility_cml))] * 1.2
  wtp_thres <- reactive(max_x * input$out_wtp_box)
  max_y <-  max(tt[, max(abs(net_cost_cml))] * 1.2, wtp_thres())
  trng_path <- paste0("M 0 0 L ",  max_x, " ", wtp_thres(), " L ", max_x, " 0 Z")


  # TODO separate this code from this specific graph because it is universal. Move it somewhere it is obviously universal
 # TODO synchronise colours and graphs with the scenario selection on the left side bar
  if (input$res_display_cep1) tt <- tt[, lapply(.SD, mean), keyby = .(friendly_name)]
  
  
    p <-
      plot_ly(
        tt, 
        x = ~ net_utility_cml,
        y = ~ net_cost_cml,
        color = ~ friendly_name,
        colors =  colours()[names %in% input$inout_scenario_select, colour],
        # frame = ~ year,
        type = "scatter",
        mode = "markers",
        symbol = ~ friendly_name,
        symbols = colours()[names %in% input$inout_scenario_select, symbol],
        showlegend = TRUE
      )
 
  p <-
    layout(
      p,
      yaxis = list(title = "Incremental cumulative cost (£)"),
      xaxis = list(title = "Incremental cumulative effects (QALYS)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y"),
        list(type = "path",
             fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
             layer = "below",
             path = trng_path),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.2,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = max_y, y1 = -max_y, yref = "y")
        ))

  # p <- animation_opts(p, frame = 1000, redraw = FALSE)
  # p <- animation_slider(p,
  #                       currentvalue = list(prefix = "Year: ",
  #                                           font = list(color = "red")))

  })

output$cep_anim <- renderPlotly({
  tt <-       out_proc()[, .(
    net_utility_cml = sum(net_utility_cml),
    net_cost_cml    = sum(net_cost_cml)
  ),
  by = .(.id, friendly_name, year)]

  max_x <- tt[, max(abs(net_utility_cml))] * 1.2
  wtp_thres <- reactive(max_x * input$out_wtp_box)
  max_y <-  max(tt[, max(abs(net_cost_cml))] * 1.2, wtp_thres())
  trng_path <- paste0("M 0 0 L ",  max_x, " ", wtp_thres(), " L ", max_x, " 0 Z")

  
  
  p <-
    plot_ly(
      tt,
      x = ~ net_utility_cml,
      y = ~ net_cost_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Incremental cumulative cost (£)"),
      xaxis = list(title = "Incremental cumulative effects (QALYS)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y"),
        list(type = "path",
             fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
             layer = "below",
             path = trng_path),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.2,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = max_y, y1 = -max_y, yref = "y")
      ))

# TODO the warning message concerns the colours and symbols that are not added on
# every frames of the animation as they should be, they are only added on the first.
# This needs to be done 3 times, for the 3 graphs with animations
  p <- animation_opts(p, frame = 1000, redraw = FALSE)
  p <- animation_slider(p,
                        currentvalue = list(prefix = "Year: ",
                                            font = list(color = "red")))
})

output$cep_p_ce <- renderPlotly({
  tt <- out_proc()[, sum(nmb_cml), by = .(.id, friendly_name, year)
            ][, .(prop_if(V1 > 0)), by = .(friendly_name, year)
              ][, V2 := predict(loess(V1 ~ year)), by = friendly_name]


  plot_ly(tt,
             x = ~year, y = ~V2, type = "scatter", mode = "lines+markers", color = ~ friendly_name, colors = colours()[names %in% input$inout_scenario_select, colour],
          symbol = ~ friendly_name, symbols = colours()[names %in% input$inout_scenario_select, symbol],
          line = list(shape = "spline", smoothing = 1.3)) %>%

    add_lines(x = ~year, y = 0.8, name = "Decision aid", color = NULL, symbol = NULL,
              line = list(color = "black", dash = "dot")) %>%
  layout(
    yaxis = list(title = "Probability of cost-effective policy", range = c(-0.05, 1.05),
                 tickformat = ",.0%"),
    xaxis = list(title = "Year")
  )
})

output$cep_p_cs <- renderPlotly({
  tt <- out_proc()[, sum(net_cost_cml), by = .(.id, friendly_name, year)
                   ][, .(prop_if(V1 <= 0)), by = .(friendly_name, year)
                     ][, V2 := predict(loess(V1 ~ year)), by = friendly_name]

  plot_ly(tt,
          x = ~year, y = ~V2, type = "scatter", mode = "lines+markers", color = ~ friendly_name, colors = colours()[names %in% input$inout_scenario_select, colour],
           symbol = ~ friendly_name, symbols = colours()[names %in% input$inout_scenario_select, symbol], line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(x = ~year, y = 0.8, name = "Decision aid", color = NULL, symbol = NULL,
              line = list(color = "black", dash = "dot")) %>%
    layout(
      yaxis = list(title = "Probability of cost-effective policy", range = c(-0.05, 1.05),
                   tickformat = ",.0%"),
      xaxis = list(title = "Year")
    )
})

# EQU plane
output$equ1_1 <- renderPlotly({
  tt <- out_proc()[year == max(year), ][, .(
    nmb_cml = sum(nmb_cml),
    sei    = mean(sei)
  ),
  by = .(.id, friendly_name)]
  max_x <- tt[, max(abs(sei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2
  
  if (input$res_display_equ1_1) tt <- tt[, lapply(.SD, mean), keyby = .(friendly_name)]
  
  p <-
    plot_ly(
      tt,
      x = ~ sei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Net monetary benefit (£)"),
      xaxis = list(title = "Absolute inequality reduction (SII)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = max_y, yref = "y"),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y")
      )
    )
  
  
  # p <- animation_opts(p, frame = 1000, redraw = FALSE)
  # p <- animation_slider(p,
  #                       currentvalue = list(prefix = "Year: ",
  #                                           font = list(color = "red")))
})
  
  output$equ1 <- renderPlotly({
  tt <- out_proc()[year == max(year), ][, .(
    nmb_cml = sum(nmb_cml),
    sei    = mean(sei)
  ),
  by = .(.id, friendly_name)]
  max_x <- tt[, max(abs(sei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2

  if (input$res_display_equ1) tt <- tt[, lapply(.SD, mean), keyby = .(friendly_name)]
  
  p <-
    plot_ly(
      tt,
      x = ~ sei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Net monetary benefit (£)"),
      xaxis = list(title = "Absolute inequality reduction (SII)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = max_y, yref = "y"),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y")
        )
      )


  # p <- animation_opts(p, frame = 1000, redraw = FALSE)
  # p <- animation_slider(p,
  #                       currentvalue = list(prefix = "Year: ",
  #                                           font = list(color = "red")))
})

output$equ_rel <- renderPlotly({
  tt <- out_proc()[year == max(year), ][, .(
    nmb_cml = sum(nmb_cml),
    rei    = mean(rei)
  ),
  by = .(.id, friendly_name)]
  max_x <- tt[, max(abs(rei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2

  
  if (input$res_display_equ_rel) tt <- tt[, lapply(.SD, mean), keyby = .(friendly_name)]
  
  p <-
    plot_ly(
      tt,
      x = ~ rei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Net monetary benefit (£)"),
      xaxis = list(title = "Relative inequality reduction (RII)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = max_y, yref = "y"),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y")
      )
    )


  # p <- animation_opts(p, frame = 1000, redraw = FALSE)
  # p <- animation_slider(p,
  #                       currentvalue = list(prefix = "Year: ",
  #                                           font = list(color = "red")))
})

output$equ_anim_abs <- renderPlotly({
  tt <- out_proc()[, .(
    nmb_cml = sum(nmb_cml),
    sei    = mean(sei)
  ),
  by = .(.id, friendly_name, year)]

  max_x <- tt[, max(abs(sei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2

  p <-
    plot_ly(
      tt,
      x = ~ sei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Net monetary benefit (£)"),
      xaxis = list(title = "Absolute inequality reduction (SII)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = max_y, yref = "y"),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y")
       ))


  p <- animation_opts(p, frame = 1000, redraw = FALSE)
  p <- animation_slider(p,
                        currentvalue = list(prefix = "Year: ",
                                            font = list(color = "red")))
})

output$equ_anim_rel <- renderPlotly({
  tt <- out_proc()[, .(
    nmb_cml = sum(nmb_cml),
    rei    = mean(rei)
  ),
  by = .(.id, friendly_name, year)]

  max_x <- tt[, max(abs(rei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2

  p <-
    plot_ly(
      tt,
      x = ~ rei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      colors = colours()[names %in% input$inout_scenario_select, colour],
      frame = ~ year,
      type = "scatter",
      mode = "markers",
      symbol = ~ friendly_name,
      symbols = colours()[names %in% input$inout_scenario_select, symbol],
      showlegend = TRUE
    )
  p <-
    layout(
      p,
      yaxis = list(title = "Net monetary benefit (£)"),
      xaxis = list(title = "Relative inequality reduction (SII)"),
      shapes = list(
        list(type = "rect",
             fillcolor = "green", line = list(color = "green"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = max_x, xref = "x",
             y0 = 0, y1 = max_y, yref = "y"),
        list(type = "rect",
             fillcolor = "red", line = list(color = "red"), opacity = 0.3,
             layer = "below",
             x0 = 0, x1 = -max_x, xref = "x",
             y0 = 0, y1 = -max_y, yref = "y")
      ))


  p <- animation_opts(p, frame = 1000, redraw = FALSE)
  p <- animation_slider(p,
                        currentvalue = list(prefix = "Year: ",
                                            font = list(color = "red")))
})

output$equ_p_abs <- renderPlotly({
  tt <- out_proc()[, mean(sei), by = .(.id, friendly_name, year)
                   ][, .(prop_if(V1 > 0)), by = .(friendly_name, year)
                     ][, V2 := predict(loess(V1 ~ year)), by = friendly_name]


  plot_ly(tt,
          x = ~year, y = ~V2, type = "scatter", mode = "lines+markers", color = ~ friendly_name, colors = colours()[names %in% input$inout_scenario_select, colour],
           symbol = ~ friendly_name, symbols = colours()[names %in% input$inout_scenario_select, symbol], line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(x = ~year, y = 0.8, name = "Decision aid", color = NULL, symbol = NULL,
              line = list(color = "black", dash = "dot")) %>%
    layout(
      yaxis = list(title = "Probability of equitable policy", range = c(-0.05, 1.05),
                   tickformat = ",.0%"),
      xaxis = list(title = "Year")
    )

})

output$equ_p_rel <- renderPlotly({
  tt <- out_proc()[, mean(rei), by = .(.id, friendly_name, year)
                   ][, .(prop_if(V1 <= 0)), by = .(friendly_name, year)
                     ][, V2 := predict(loess(V1 ~ year)), by = friendly_name]


  plot_ly(tt,
          x = ~year, y = ~V2, type = "scatter", mode = "lines+markers", color = ~ friendly_name, colors = colours()[names %in% input$inout_scenario_select, colour],
           symbol = ~ friendly_name, symbols = colours()[names %in% input$inout_scenario_select, symbol], line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(x = ~year, y = 0.8, name = "Decision aid", color = NULL, symbol = NULL,
                 line = list(color = "black", dash = "dot")) %>%
    layout(
      yaxis = list(title = "Probability of equitable policy", range = c(-0.05, 1.05),
                   tickformat = ",.0%"),
      xaxis = list(title = "Year")
    )
})


 myModal <- function() {
   div(id = "test",
       modalDialog(downloadButton("download","Download the tab as csv"),
                   br(),
                   br(),
                   easyClose = TRUE, title = "Download Table")
   )
 }


output$tbl <- DT::renderDataTable(
  if(!is.null(input$inout_columns_select)) {
   DT::datatable(
     out_proc()[, .SD, .SDcols = input$inout_columns_select],
    style = "bootstrap",
    extensions = "Buttons",
    options = list(scrollX = '400px', scrollY = "350px", searching = FALSE,
                   dom = 'Bfrtip',
                   pageLength = 100, buttons = list(
                     "pdf",
                     list(
                       extend = "collection",
                       text = 'download entire dataset',
                       action = DT::JS("function ( e, dt, node, config ) {
                                    Shiny.setInputValue('test', true, {priority: 'event'});
                                    }")
                     )
                   )),
    filter = list(position = "top"),
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: left;',
      'Table 4: ',
      htmltools::em('Model outputs'))
   )} else { #brand needed for when no columns are selected
  datatable(
    out_proc(),
            style = "bootstrap",
            extensions = "Buttons",
            options = list(scrollX = '400px', scrollY = "350px", searching = FALSE,
                           dom = 'Bfrtip',
                           pageLength = 100, buttons = list("pdf")),
            filter = list(position = "top"),
            caption = htmltools::tags$caption(
              style = 'caption-side: top; text-align: left;',
              'Table 4: ',
              htmltools::em('Model outputs'))
  )
   }
)

observeEvent(input$test, {
  #print("hello")
  showModal(myModal())
})

output$download <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(out_proc()[, .SD, .SDcols = input$inout_columns_select], file)
  }
)

output$out_columns_select <- renderUI({
  tagList(
    pickerInput(inputId = "inout_columns_select",
                label = "Columns",
                choices = colnames(out_proc()),
                selected = colnames(out_proc()),
                options = list(`actions-box` = TRUE, `live-search` = FALSE),
                multiple = TRUE)    %>%
      shinyInput_label_embed(
        icon("info") %>%
          bs_embed_tooltip(title = "Please select the parameters you want to see in the tab.")
      )
  )
})



outputOptions(output, "out_scenario_select", suspendWhenHidden = FALSE)
outputOptions(output, "out_year_slider", suspendWhenHidden = FALSE)
# outputOptions(input, "out_discount_slider", suspendWhenHidden = FALSE)

