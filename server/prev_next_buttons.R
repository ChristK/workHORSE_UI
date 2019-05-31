

observeEvent(input$next_sc1, {
  updateTabsetPanel(session, "level",
                    selected = as.character(as.integer(gsub("_sc", "", "_sc1")) + 1L)
  )
})

observeEvent(input$previous_sc1, {
  updateTabsetPanel(session, "level",
                    selected = as.character(as.integer(gsub("_sc", "", "_sc1")) - 1L)
  )
})
