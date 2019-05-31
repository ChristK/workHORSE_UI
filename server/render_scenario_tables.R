

observeEvent(input$age_eligibility_slider_sc1, {
  # Dynamic age groups ----
  if (input$age_eligibility_slider_sc1[[2]] - input$age_eligibility_slider_sc1[[1]] < 11) {
    agegrp_nam <- paste0(input$age_eligibility_slider_sc1[[1]], "-", input$age_eligibility_slider_sc1[[2]])
  } else {
  agegrp_nam <- agegrp_name(as.integer(input$age_eligibility_slider_sc1[[1]]/10) * 10L,
                            as.integer(input$age_eligibility_slider_sc1[[2]]/10) * 10L, 10L)
  }
  l_agegrp_nam <- length(agegrp_nam)
  # Prepare uptake table ----------------------------------------------------
  output$uptake_table_sc1 <- renderTable({
    num.inputs.col1 <- paste0("<input id='uptake_qrisk_low_",  seq_len(l_agegrp_nam * 10), "_sc1", "' class='shiny-bound-input' type='number' value=`0.007`>")
    num.inputs.col2 <- paste0("<input id='uptake_qrisk_mid_",  seq_len(l_agegrp_nam * 10), "_sc1", "' class='shiny-bound-input' type='number' value=`0.007`>")
    num.inputs.col3 <- paste0("<input id='uptake_qrisk_high_", seq_len(l_agegrp_nam * 10), "_sc1", "' class='shiny-bound-input' type='number' value=`0.007`>")
    CJ(QIMD = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)"),
       Sex = c("men", "women"),
       Ages = agegrp_nam)[,
                          c("Low risk (QRISK <10)", "Mid risk (QRISK 10-20)", "High risk (QRISK 20+)") := .(num.inputs.col1, num.inputs.col2, num.inputs.col3)
                          ]
  }, spacing = "xs",
  sanitize.text.function = function(x) x)
})




# Prepare statins Px table -------------------------------
  output$statin_px_table_sc1 <- renderTable({
    num.inputs.col1 <- paste0("<input id='statin_px_qrisk_low_",  1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    num.inputs.col2 <- paste0("<input id='statin_px_qrisk_mid_",  1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    num.inputs.col3 <- paste0("<input id='statin_px_qrisk_high_", 1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    setnames(data.frame(QIMD =
                          c("1 (most deprived)", 2, 3, 4, "5 (least deprived)"),
                        num.inputs.col1, num.inputs.col2, num.inputs.col3),
             c("num.inputs.col1", "num.inputs.col2", "num.inputs.col3"),
             c("Low risk (QRISK <10)", "Mid risk (QRISK 10-20)", "High risk (QRISK 20+)")
    )
  }, spacing = "xs",
  sanitize.text.function = function(x) x)

# Prepare antihtn Px table -------------------------------
  output$antihtn_px_table_sc1 <- renderTable({
    num.inputs.col1 <- paste0("<input id='antihtn_px_qrisk_low_",  1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    num.inputs.col2 <- paste0("<input id='antihtn_px_qrisk_mid_",  1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    num.inputs.col3 <- paste0("<input id='antihtn_px_qrisk_high_", 1:5, "_sc1", "' class='shiny-bound-input' type='number' value=`0`>")
    setnames(data.frame(QIMD =
                          c("1 (most deprived)", 2, 3, 4, "5 (least deprived)"),
                        num.inputs.col1, num.inputs.col2, num.inputs.col3),
             c("num.inputs.col1", "num.inputs.col2", "num.inputs.col3"),
             c("Low risk (QRISK <10)", "Mid risk (QRISK 10-20)", "High risk (QRISK 20+)")
    )
  }, spacing = "xs",
  sanitize.text.function = function(x) x)

