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
                            description = 'Number of additions to list, patients admitted for treatment
                            and patients waiting for treatment, alongside distribution of waiting times')),
      # Specialties
      column(6, class="intro-page-column",
                intro_main_box(button_name = 'intro_jump_to_specialties',
                            title_box = "Variation by specialty",
                            description = 'Variation of waiting list activity and distribution of
                            wait times by specialty'))
    ), # fluid row
    fluidRow(
      br(),
      # HBT
      column(6, class="intro-page-column",
         intro_main_box(button_name = 'intro_jump_to_hbt',
                        title_box = "Variation by Health Board of Treatment",
                        description = 'Variation of waiting list activity and distribution of
                            wait times by Health Board')),
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
    h4("About Clinical Prioritisation"),
    br(),
    includeHTML("about_cp.html")

  )
}) # render UI close bracket


output$data_quality_notes <- renderUI({

  tagList(
    h4("Data quality summary"),
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
    numbers[["dq_summary"]],
    br(),
    numbers[["dq_table"]]

  )
}) # render UI close bracket


output$using_dashboard_notes <- renderUI({

  tagList(
    h4("Using the dashboard"),
    tags$li("There are tabs across the top of the dashboard for each topic area, and also buttons at the bottom
      of this page to help you navigate around the dashboard."),
    tags$li("You can find an interactive overview of the data on the 'Overview of waiting list activity' tab, and more in-depth
            breakdowns by specialty and NHS Health Board of Treatment on the corresponding additional tabs."),
    tags$li("If you would like to download data, please do this using the 'Download data' tab, which will
            allow you to select custom data for download."),
    br(),
    h4("Interacting with the dashboard"),
    tags$li("On each tab there are dropdown boxes which allow the user to filter the data they wish to
            explore, for example by NHS Health Board of Treatment."),
    tags$li("Move the mouse over the charts to see information about the underlying data"),
    tags$li("Clicking on a category in the legend removes it from the chart, and double clicking will isolate
            only that category. This is useful to reduce the number of lines on the chart, making them
            easier to see. Clicking again on a removed category will add it back into the chart."),
    tags$li("You can download an image of a chart in a given state by clicking the camera icon in the
            top right hand corner of the chart."),
    tags$li("Some panels have tabs on the upper right hand side which allow you to toggle between different
            views, such as a graph and data view, or two different graph views.")

  )
}) # render UI close bracket

output$further_info_notes <- renderUI({

  tagList(
    h4("Further information"),
    tags$li("The code used to produce this tool will soon be published on",
       tags$a(href="https://github.com/Public-Health-Scotland", "GitHub (external website)")),
    tags$li("Please contact the PHS Waiting Times team with any questions or comments on",
            tags$a(href="mailto:phs.waitingtimes@phs.scot", "phs.waitingtimes@phs.scot"))

    #bsButton("jump_to_specialties", label = "Go to specialties")
  )
}) # render UI close bracket


output$accessibility_notes <- renderUI({

  tagList(
    h4("Accessibility"),
    tags$li("This website is run by", tags$a(href="https://www.publichealthscotland.scot/",
                                             "Public Health Scotland,"), "Scotland's national
            organisation for public health. As a new organisation formed on 1 April 2020,
            Public Health Scotland is currently reviewing its web estate. Public Health Scotland
            is committed to making its website accessible, in accordance with the Public Sector
            Bodies (Websites and Mobile Applications)(No. 2) Accessibility Regulations 2018.
            This accessibility statement applies to this dashboard."),
    tags$li(tags$a(href="https://mcmw.abilitynet.org.uk/", "AbilityNet (external website)"),
            "has advice on making your device easier to use if you have a disability"),
    br(),
    p(strong("Compliance status")),
    p("This site has not yet been evaluated against Web Content Accessibility Guidelines version 2.1
      level AA standard."),
    br(),
    p(strong("Reporting any accessibility problems with this website")),
    p("If you with to contact us about any accessibility issues you encounter on this site, please email",
      tags$a(href="mailto:phs.qualityindicators@phs.scot", "phs.qualityindicators@phs.scot")),
    br(),
    p(strong("Enforcement procedure")),
    p("The Equality and Human Rights Commission (EHRC) is responsible for enforcing the Public Sector
      Bodies (Websites and Mobile Applications)(No.2) Accessibility Regulations 2018 (the 'accessibility
      regulations'). If you are not happy with how we respond to your complaint,",
      tags$a(href="https://www.equalityadvisoryservice.com/",
             "contact the Equality Advisory and Support Service (EASS) (external website).")),
    br(),
    p(strong("Preparation of this accessibility statement")),
    p("This statement was prepared on 24 August 2022. It was last reviewed on 24 August 2022.")

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


