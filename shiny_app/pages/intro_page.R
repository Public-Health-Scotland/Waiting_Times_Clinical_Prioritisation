####################### Intro Page #######################

output$intro_page_ui <-  renderUI({

  div(
    # 1st row of boxes
    fluidRow(
      h3("Use the buttons below or the tabs in the top bar to navigate around the dashboard"),
      br(),
      # Overview
      column(6, class="intro-page-column",
                intro_main_box(button_name = 'intro_jump_to_landing_page',
                            title_box = "Overview",
                            description = 'Summary statistics')),
      # Specialties
      column(6, class="intro-page-column",
                intro_main_box(button_name = 'intro_jump_to_specialties',
                            title_box = "Specialties",
                            description = 'Breakdown by specialties'))
    ), # fluid row
    fluidRow(
      br(),
      # HBT
      column(6, class="intro-page-column",
         intro_main_box(button_name = 'intro_jump_to_hbt',
                        title_box = "Health Board of Treatment",
                        description = 'Breakdown by Health Board of Treatment')),
     # Data download
      column(6, class="intro-page-column",
            intro_main_box(button_name = 'intro_jump_to_data',
                           title_box = "Download data",
                           description = 'Interactive page to dowload selected data'))
    ), # fluid row
   # End of first row

   wellPanel(
     column(12,
            linebreaks(3),
            h3("Select a topic to see background information"),
            bsCollapse(id = "collapse_notes", open = "Panel 1",
                       bsCollapsePanel("Overview", uiOutput("overview_notes")),
                       bsCollapsePanel("Clinical Prioritisation", uiOutput("cp_notes")),
                       bsCollapsePanel("Additional information", uiOutput("timeline_notes")),
                       bsCollapsePanel("Specialties", uiOutput("specialty_notes"))
            ) # bscollapse
     ) # column
   ) # wellpanel


  ) # div

})



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




