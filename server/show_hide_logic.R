observeEvent(input$cancel_program_checkbox_sc1, {
  if (input$cancel_program_checkbox_sc1) {
    lapply(
      c(
        "frequency_eligibility_slider_sc1",
        "age_eligibility_slider_sc1",
        "invite_known_hypertensives_checkbox_sc1",
        "invite_known_diabetics_checkbox_sc1",
        "coverage_detailed_checkbox_sc1",
        "coverage_qimd0_slider_sc1",
        "coverage_qimd1_slider_sc1",
        "coverage_qimd2_slider_sc1",
        "coverage_qimd3_slider_sc1",
        "coverage_qimd4_slider_sc1",
        "coverage_qimd5_slider_sc1",
        "coverage_cost_qimd0_sc1",
        "coverage_cost_qimd1_sc1",
        "coverage_cost_qimd2_sc1",
        "coverage_cost_qimd3_sc1",
        "coverage_cost_qimd4_sc1",
        "coverage_cost_qimd5_sc1",
        "uptake_slider_sc1",
        "uptake_cost_sc1",
        "uptake_detailed_checkbox_sc1",
        "uptake_equalprob_checkbox_sc1",
        "uptake_table_help_sc1",
        "uptake_table_sc1",
        "statin_px_slider_sc1",
        "antihtn_px_slider_sc1",
        "px_detailed_checkbox_sc1",
        "statin_px_table_help_sc1",
        "statin_px_table_sc1",
        "antihtn_px_table_help_sc1",
        "antihtn_px_table_sc1",
        "smkcess_slider_sc1",
        "smkcess_cost_sc1",
        "smkcess_cost_ovrhd_sc1",
        "wghtpct_slider_sc1",
        "wghtreduc_slider_sc1",
        "wghtloss_cost_sc1",
        "wghtloss_cost_ovrhd_sc1",
        "papct_slider_sc1",
        "papincr_slider_sc1",
        "pa_cost_sc1",
        "pa_cost_ovrhd_sc1",
        "ignore_cholesterol_checkbox_sc1",
        "ignore_sbp_checkbox_sc1",
        "ignore_bmi_checkbox_sc1"
      ),
      hideElement,
      anim = TRUE
    )
  } else {
    lapply(
      c(
        "frequency_eligibility_slider_sc1",
        "age_eligibility_slider_sc1",
        "invite_known_hypertensives_checkbox_sc1",
        "invite_known_diabetics_checkbox_sc1",
        "coverage_detailed_checkbox_sc1",
        "smkcess_slider_sc1",
        "smkcess_cost_sc1",
        "smkcess_cost_ovrhd_sc1",
        "wghtpct_slider_sc1",
        "wghtreduc_slider_sc1",
        "wghtloss_cost_sc1",
        "wghtloss_cost_ovrhd_sc1",
        "papct_slider_sc1",
        "papincr_slider_sc1",
        "pa_cost_sc1",
        "pa_cost_ovrhd_sc1",
        "uptake_slider_sc1",
        "uptake_cost_sc1",
        "uptake_detailed_checkbox_sc1",
        "px_detailed_checkbox_sc1",
        "ignore_cholesterol_checkbox_sc1",
        "ignore_sbp_checkbox_sc1",
        "ignore_bmi_checkbox_sc1"
      ),
      showElement,
      anim = TRUE
    )
    if (!input$coverage_detailed_checkbox_sc1) {
      lapply(
        c(
          "coverage_qimd0_slider_sc1",
          "coverage_cost_qimd0_sc1"
        ),
        showElement,
        anim = TRUE
      )
    } else {
      lapply(
        c(
          "coverage_qimd1_slider_sc1",
          "coverage_qimd2_slider_sc1",
          "coverage_qimd3_slider_sc1",
          "coverage_qimd4_slider_sc1",
          "coverage_qimd5_slider_sc1",
          "coverage_cost_qimd1_sc1",
          "coverage_cost_qimd2_sc1",
          "coverage_cost_qimd3_sc1",
          "coverage_cost_qimd4_sc1",
          "coverage_cost_qimd5_sc1"
        ),
        showElement,
        anim = TRUE
      )
    }
    if (input$uptake_detailed_checkbox_sc1) {
      lapply(
        c(
          "uptake_table_sc1",
          "uptake_table_help_sc1"
        ),
        showElement,
        anim = TRUE
      )
    } else {
      showElement(id = "uptake_equalprob_checkbox_sc1",
                  anim = TRUE)
    }

    if (!input$px_detailed_checkbox_sc1) {
      lapply(c("statin_px_slider_sc1", "antihtn_px_slider_sc1"),
             showElement,
             anim = TRUE)
    } else {
      lapply(
        c(
          "statin_px_table_help_sc1",
          "statin_px_table_sc1",
          "antihtn_px_table_help_sc1",
          "antihtn_px_table_sc1"
        ),
        showElement,
        anim = TRUE
      )
    }
  }
})

observeEvent(input$coverage_detailed_checkbox_sc1, {
  toggleElement(id = "coverage_qimd0_slider_sc1",
                condition = !input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd0_sc1",
                condition = !input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_qimd1_slider_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd1_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_qimd2_slider_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd2_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_qimd3_slider_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd3_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_qimd4_slider_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd4_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_qimd5_slider_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "coverage_cost_qimd5_sc1",
                condition = input$coverage_detailed_checkbox_sc1, anim = TRUE)
})

observeEvent(input$uptake_detailed_checkbox_sc1, {
  toggleElement(id = "uptake_table_sc1",
                condition = input$uptake_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "uptake_table_help_sc1",
                condition = input$uptake_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "uptake_equalprob_checkbox_sc1",
                condition = !input$uptake_detailed_checkbox_sc1, anim = TRUE)

})

observeEvent(input$px_detailed_checkbox_sc1, {
  toggleElement(id = "statin_px_slider_sc1",
                condition = !input$px_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "antihtn_px_slider_sc1",
                condition = !input$px_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "statin_px_table_help_sc1",
                condition = input$px_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "statin_px_table_sc1",
                condition = input$px_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "antihtn_px_table_help_sc1",
                condition = input$px_detailed_checkbox_sc1, anim = TRUE)
  toggleElement(id = "antihtn_px_table_sc1",
                condition = input$px_detailed_checkbox_sc1, anim = TRUE)
})
