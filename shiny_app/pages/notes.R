####################### Notes #######################

output$notes_ui <- renderUI({

  wellPanel(
    column(12,
         p("Select a topic to see background information"),
         bsCollapse(id = "collapse_notes", open = "Panel 1",
                    bsCollapsePanel("Overview", uiOutput("overview_notes")),
                    bsCollapsePanel("Clinical Prioritisation", uiOutput("cp_notes")),
                    bsCollapsePanel("Additional information", uiOutput("timeline_notes")),
                    bsCollapsePanel("Specialties", uiOutput("specialty_notes"))
         ) # bscollapse
      ) # column
  ) # wellpanel

  }) # render UI close bracket


output$overview_notes <- renderUI({

  tagList(
    h4("Overview"),

    bsButton("jump_to_landing_page", label = "Go to overview")

    )
}) # render UI close bracket


output$cp_notes <- renderUI({

  tagList(

    )
}) # render UI close bracket


output$generic_notes <- renderUI({

  tagList(

    )
}) # render UI close bracket

output$specialty_notes <- renderUI({

  tagList(
  h4("Specialties"),

  bsButton("jump_to_specialties", label = "Go to specialties")
    )
}) # render UI close bracket



