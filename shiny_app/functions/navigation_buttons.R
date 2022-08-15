####################### Navigation buttons #######################

# Jump to main tabs
observeEvent(input$jump_to_landing_page, {updateTabsetPanel(session, "intabset", selected = "overview")})
observeEvent(input$jump_to_specialties, {updateTabsetPanel(session, "intabset", selected = "specialties")})
observeEvent(input$jump_to_hbt, {updateTabsetPanel(session, "intabset", selected = "hbt")})
observeEvent(input$jump_to_data, {updateTabsetPanel(session, "intabset", selected = "data")})
observeEvent(input$jump_to_notes, {updateTabsetPanel(session, "intabset", selected = "notes")})

# Jump to note panels
# To jump to commentary tab and ensures correct panel is expanded
# action buttons must have unique ID
observeEvent(input$jump_to_notes_overview, {updateTabsetPanel(session, "intabset", selected = "Notes")
  updateCollapse(session, "collapse_notes", open = "Overview")})
observeEvent(input$jump_to_notes_cp, {updateTabsetPanel(session, "intabset", selected = "Notes")
  updateCollapse(session, "collapse_notes", open = "Clinical Prioritisation")})
observeEvent(input$jump_to_notes_generic, {updateTabsetPanel(session, "intabset", selected = "Notes")
  updateCollapse(session, "collapse_notes", open = "Additional information")})
observeEvent(input$jump_to_notes_specialty, {updateTabsetPanel(session, "intabset", selected = "Notes")
  updateCollapse(session, "collapse_notes", open = "Specialties")})

# Jump from intro page to different tabs
observeEvent(input$intro_jump_to_landing_page, {updateTabsetPanel(session, "intabset", selected = "overview")})
observeEvent(input$intro_jump_to_specialties, {updateTabsetPanel(session, "intabset", selected = "specialties")})
observeEvent(input$intro_jump_to_hbt, {updateTabsetPanel(session, "intabset", selected = "hbt")})
observeEvent(input$intro_jump_to_data, {updateTabsetPanel(session, "intabset", selected = "data")})
observeEvent(input$intro_jump_to_notes, {updateTabsetPanel(session, "intabset", selected = "notes")})
