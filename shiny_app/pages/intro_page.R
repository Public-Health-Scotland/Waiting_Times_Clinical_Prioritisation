####################### Intro Page #######################

output$intro_page_ui <-  renderUI({
  # getDependency('sparkline') #load dependencies for sparkline plot

  div(
    wellPanel(
      column(12,
             h3("Select a topic to see background information"),
             bsCollapse(id = "collapse_notes", open = "Panel 1",
                        bsCollapsePanel("About Clinical Prioritisation", uiOutput("about_cp_notes")),
                        bsCollapsePanel("Data quality", uiOutput("data_quality_notes")),
                        bsCollapsePanel("Using the dashboard", uiOutput("using_dashboard_notes")),
                        bsCollapsePanel("Further information", uiOutput("further_info_notes")),
                        bsCollapsePanel("Accessibility", uiOutput("accessibility_notes"))
             ) # bscollapse
      ) # column
    ), # wellpanel

    # 1st row of boxes
    fluidRow(
      h3("Use the buttons below or the tabs in the top bar to navigate around the dashboard"),
      br(),
      # Overview
      column(6, class="intro-page-column",
                intro_main_box(button_name = 'intro_jump_to_landing_page',
                            title_box = "Overview of waiting list activity",
                            description = 'Summary statistics')),
      # Specialties
      column(6, class="intro-page-column",
                intro_main_box(button_name = 'intro_jump_to_specialties',
                            title_box = "Variation by specialty",
                            description = 'Breakdown by specialties'))
    ), # fluid row
    fluidRow(
      br(),
      # HBT
      column(6, class="intro-page-column",
         intro_main_box(button_name = 'intro_jump_to_hbt',
                        title_box = "Variation by Health Board of Treatment",
                        description = 'Breakdown by Health Board of Treatment')),
     # Data download
      column(6, class="intro-page-column",
            intro_main_box(button_name = 'intro_jump_to_data',
                           title_box = "Download data",
                           description = 'Interactive page to dowload selected data'))
    ), # fluid row
   # End of first row

   fluidRow(width=12, height="100px", br())


  ) # div

})



output$about_cp_notes <- renderUI({

  tagList(
    h4("About Clinical Prioritisation")

  )
}) # render UI close bracket


output$data_quality_notes <- renderUI({

  tagList(
    h4("Data quality summary"),

    numbers[["dq_summary"]],
    br(),
    column(6,
           pickerInput("hbt_dq_filter",
                       "1. Select Health Board of Treatment ",
                       choices = c("NHS Scotland",
                                   "NHS Ayrshire & Arran",
                                   "NHS Borders",
                                   "NHS Dumfries & Galloway",
                                   "NHS Fife",
                                   "NHS Forth Valley",
                                   "NHS Grampian",
                                   "NHS Greater Glasgow & Clyde",
                                   "NHS Highland",
                                   "NHS Lanarkshire",
                                   "NHS Lothian",
                                   "NHS Orkney",
                                   "NHS Shetland",
                                   "NHS Tayside",
                                   "NHS Western Isles",
                                   "Golden Jubilee National Hospital"),
                       selected = "NHS Scotland",
                       multiple = FALSE)
    ),
    column(6,
           pickerInput("month_dq_filter",
                       "2. Select month",
                       choices = get_month(unique(app_data[["total_comp_jun"]]$date)),
                       selected = "June 2022")
    ),
    br(),
    numbers[["dq_table"]]

  )
}) # render UI close bracket


output$using_dashboard_notes <- renderUI({

  tagList(
    h4("Using the dashboard")

  )
}) # render UI close bracket

output$further_info_notes <- renderUI({

  tagList(
    h4("Further information")#,

    #bsButton("jump_to_specialties", label = "Go to specialties")
  )
}) # render UI close bracket


output$accessibility_notes <- renderUI({

  tagList(
    h4("Accessibility")#,

  #  bsButton("jump_to_specialties", label = "Go to specialties")
  )
}) # render UI close bracket


numbers$dq_table <- DT::renderDataTable({

  sparkline_table(dq_table(app_data[["total_comp_jun"]], input$hbt_dq_filter, input$month_dq_filter))

})

numbers$dq_summary <- renderUI({
  select_text <- app_data[["dq_summaries"]] %>% 
    filter(nhs_board_of_treatment == input$hbt_dq_filter) %>% 
    select(summary)

  paste(select_text)
})


