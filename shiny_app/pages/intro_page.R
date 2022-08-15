####################### Intro Page #######################

output$intro_page_ui <-  renderUI({

  div(
    # 1st row of boxes
    fluidRow(
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
                            description = 'Breakdown by specialties')),
      # HBT
      column(6, class="intro-page-column",
         intro_main_box(button_name = 'intro_jump_to_hbt', title_box = "Health Board",
                     description = 'Breakdown by Health Board of Treatment')),
     # Data download
     column(6, class="intro-page-column",
            intro_main_box(button_name = 'intro_jump_to_data', title_box = "Download data",
                        description = 'Interactive page to dowload selected data'))
    ) # fluid row close
   # End of first row

  ) # div

})
