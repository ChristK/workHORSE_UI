output$out_year_slider <- renderUI({
  tagList(
    sliderInput(
      "inout_year_slider",
      "Year",
      input$simulation_period_slider[[1]], input$simulation_period_slider[[2]],
      input$simulation_period_slider[[2]], 1,
      sep = "",
      ticks = FALSE
    )
  )
})

observe({
      hlp_frienly_names <- list()
      for (i in seq_len(input$scenarios_number_slider)) {
        if (!input[[paste0("baseline_sc", i)]] && !input[[paste0("parallel_ensemble_checkbox_sc", i)]]) {
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
                multiple = TRUE)
  )
})

out_proc <- reactive({
  # if (!exists("out")) out <- read_fst("./metamodel/out.fst", as.data.table = TRUE)
  # out <- read_fst(file.path("output", "out.fst"), as.data.table = TRUE)
  out()[year <= input$inout_year_slider &
        friendly_name %in% input$inout_scenario_select,][, `:=` (
          net_utility = net_utility * ((100 - input$out_discount_qalys_slider) / 100) ^ (year - 2017),
          net_cost = net_cost * ((100 - input$out_discount_cost_slider) /
                                   100) ^ (year - 2017)
        )][,
           `:=` (
             cpp_chd_cml     = round(cumsum(cpp_chd)),
             cpp_stroke_cml  = round(cumsum(cpp_stroke)),
             cpp_t2dm_cml    = round(cumsum(cpp_t2dm)),
             cpp_af_cml      = round(cumsum(cpp_af)),
             cpp_lc_cml      = round(cumsum(cpp_lc)),
             dpp_chd_cml     = round(cumsum(dpp_chd)),
             dpp_stroke_cml  = round(cumsum(dpp_stroke)),
             dpp_other_cml   = round(cumsum(dpp_other)),
             net_utility_cml = cumsum(net_utility),
             net_cost_cml    = cumsum(net_cost)
           ),
           by = .(.id, friendly_name, qimd)][, `:=` (nmb_cml = net_utility_cml * input$out_wtp_box - net_cost_cml)]
})

# CE plane
output$cep1_1 <- output$cep1 <- renderPlotly({
  tt <-       out_proc()[year == max(year), ][, .(
    net_utility_cml = sum(net_utility_cml),
    net_cost_cml    = sum(net_cost_cml)
  ),
  by = .(.id, friendly_name)]

  max_x <- tt[, max(abs(net_utility_cml))] * 1.2
  wtp_thres <- reactive(max_x * input$out_wtp_box)
  max_y <-  max(tt[, max(abs(net_cost_cml))] * 1.2, wtp_thres())
  trng_path <- paste0("M 0 0 L ",  max_x, " ", wtp_thres(), " L ", max_x, " 0 Z")
  # AMANDINE pal <- input$color_select
  mypal <- brewer.pal(9, "Greens")
  
  p <-
    plot_ly(
      tt,
      x = ~ net_utility_cml,
      y = ~ net_cost_cml,
      color = ~ friendly_name,
      # colorscale='Viridis',
      # reversescale =T,
      colors = mypal,
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
      showlegend = TRUE
    )
  suppressWarnings(print(p))
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
      frame = ~ year,
      type = "scatter",
      mode = "markers",
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
             x = ~year, y = ~V2, color = ~ friendly_name, type = 'scatter',
             mode = 'lines', line = list(shape = "spline", smoothing = 1.3)) %>%
  add_lines(y = 0.8, name = "Decision aid", color = NULL,
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
          x = ~year, y = ~V2, color = ~ friendly_name, type = 'scatter',
          mode = 'lines', line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(y = 0.8, name = "Decision aid", color = NULL,
              line = list(color = "black", dash = "dot")) %>%
    layout(
      yaxis = list(title = "Probability of cost-effective policy", range = c(-0.05, 1.05),
                   tickformat = ",.0%"),
      xaxis = list(title = "Year")
    )
})

# EQU plane
output$equ1_1 <- output$equ1 <- renderPlotly({
  tt <- out_proc()[year == max(year), ][, .(
    nmb_cml = sum(nmb_cml),
    sei    = mean(sei)
  ),
  by = .(.id, friendly_name)]
  max_x <- tt[, max(abs(sei))] * 1.2
  max_y <- tt[, max(abs(nmb_cml))] * 1.2

  p <-
    plot_ly(
      tt,
      x = ~ sei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
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

  p <-
    plot_ly(
      tt,
      x = ~ rei,
      y = ~ nmb_cml,
      color = ~ friendly_name,
      # frame = ~ year,
      type = "scatter",
      mode = "markers",
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
      frame = ~ year,
      type = "scatter",
      mode = "markers",
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
      frame = ~ year,
      type = "scatter",
      mode = "markers",
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
          x = ~year, y = ~V2, color = ~ friendly_name, type = 'scatter',
          mode = 'lines', line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(y = 0.8, name = "Decision aid", color = NULL,
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
          x = ~year, y = ~V2, color = ~ friendly_name, type = 'scatter',
          mode = 'lines', line = list(shape = "spline", smoothing = 1.3)) %>%
    add_lines(y = 0.8, name = "Decision aid", color = NULL,
              line = list(color = "black", dash = "dot")) %>%
    layout(
      yaxis = list(title = "Probability of equitable policy", range = c(-0.05, 1.05),
                   tickformat = ",.0%"),
      xaxis = list(title = "Year")
    )
})

output$tbl <- renderDT(
  out_proc(),
  style = "bootstrap",
  options = list(scrollX = '400px', scrollY = "350px",
                 dom = 'lrt'
  ),
  filter = list(position = "top"),
  caption = htmltools::tags$caption(
    style = 'caption-side: top; text-align: left;',
    'Table 4: ',
    htmltools::em('Model outputs')
  )
)
outputOptions(output, "out_scenario_select", suspendWhenHidden = FALSE)
outputOptions(output, "out_year_slider", suspendWhenHidden = FALSE)
# outputOptions(input, "out_discount_slider", suspendWhenHidden = FALSE)

