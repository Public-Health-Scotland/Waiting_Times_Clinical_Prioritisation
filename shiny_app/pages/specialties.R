####################### Specialties #######################

output$specialties_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
                                 column(width=4,
                                        pickerInput("hbt_filter_spec",
                                                    "1. Select Health Board ",
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
                                                    options = pickerOptions(liveSearch = TRUE, showTick=TRUE),
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("quarter_end_spec",
                                                     "2. Select quarter end date ",
                                                     choices = c("September 2021", "December 2021", "June 2022"),
                                                     selected = "June 2022",
                                                     multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("specialty_filter",
                                                    "3. Select up to six specialties ",
                                                    choices = unique(app_data[["hb_plotdata_jun"]]$specialty),
                                                    selected = c("Orthopaedics", "General Surgery",
                                                                 "Opthalmology", "Urology",
                                                                 "Ear, Nose & Throat", "Gynaecology"),
                                                    options = pickerOptions(
                                                      liveSearch=TRUE,
                                                      maxOptions = 6,
                                                      maxOptionsText="Choose up to six options"),

                                                    multiple = TRUE)
                                 ) # column
             ) # box
    ), # fluidrow

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="2000px", side="right",
                                     tabPanel("Activity",
                                              tagList(
                                                h3("Activity"),
                                                br(),
                                                plots[["activity_facet_plot_spec"]],
                                                linebreaks(10),
                                                materialSwitch(inputId = "show_data_activity",
                                                               label = "Show data",
                                                               right = FALSE,
                                                               value = FALSE,
                                                               status = "primary"),
                                                conditionalPanel(
                                                  # Condition is in javascript
                                                  condition = "input.show_data_activity == true",
                                                    numbers[["spec_activity_table_output"]]
                                                  )
                                              ) # taglist
                                     ),
                                     tabPanel("Distribution of waits",
                                              tagList(
                                                h3("Distribution of waits"),
                                                br(),
                                                fluidRow(
                                                  column(6,
                                                         plots[["waits_spec_plot_admitted"]]
                                                         ), #column
                                                  column(6,
                                                         plots[["waits_spec_plot_ongoing"]]
                                                         ) #column
                                                ), #fluidrow
                                                linebreaks(35),
                                                materialSwitch(inputId = "show_data_waits",
                                                               label = "Show data",
                                                               right = FALSE,
                                                               value = FALSE,
                                                               status = "primary"),
                                                conditionalPanel(
                                                  # Condition is in javascript
                                                  condition = "input.show_data_waits == true",
                                                  numbers[["spec_waits_table_output"]]
                                                )
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow


    fluidRow(width=12, height="50px", br())

  ) # div


})


# This makes sure that specialty_filter default options update based off HBT and quarter
observe({
  if( (!is.null(input$quarter_end_spec) & (!is.null(input$hbt_filter_spec))) ) {
    updateSelectInput(session, "specialty_filter",
                      # topsix_specs is defined in specialties_plot_functions
                      selected = topsix_specs(input$quarter_end_spec,
                                              input$hbt_filter_spec) )
  }
})



## Plots

plots$activity_facet_plot_spec <- renderPlotly({

  withProgress(message="Loading plots ... please wait", {

                                              p1 <- activity_specs(input_data=app_data[["hb_plotdata_jun"]],
                                                                     waiting_status = "additions",
                                                                     qend=input$quarter_end_spec,
                                                                     hbt=input$hbt_filter_spec,
                                                                     specialties=input$specialty_filter)

                                              p2 <- activity_specs(input_data=app_data[["hb_plotdata_jun"]],
                                                                   waiting_status = "admitted",
                                                                   qend=input$quarter_end_spec,
                                                                   hbt=input$hbt_filter_spec,
                                                                   specialties=input$specialty_filter)

                                              p3 <- activity_specs(input_data=app_data[["hb_plotdata_jun"]],
                                                                   waiting_status = "waiting",
                                                                   qend=input$quarter_end_spec,
                                                                   hbt=input$hbt_filter_spec,
                                                                   specialties=input$specialty_filter)

                                              #make facets
                                              subplot(style(p1, showlegend = FALSE), # keep one legend for all plots
                                                      plotly_empty(),
                                                      style(p2, showlegend = FALSE),
                                                      plotly_empty(),
                                                      p3, nrows = 5, heights = c(0.26, 0.1, 0.26, 0.1, 0.26), shareX = FALSE, # share axis between plots
                                                      titleY = TRUE)
  })
})

#Dow ongoing waits facetted by specialty
plots$waits_spec_plot_ongoing <- renderPlotly({

  withProgress(message="Loading plots ... please wait", {

  make_dow_spec_suplots(data = app_data[["dow_4wk_qtr_pub_jun"]],
                        plotdata=app_data[["hb_plotdata_jun"]],
                   specialties = input$specialty_filter,
                   n_specs = length(input$specialty_filter),
                   waiting_status = "waiting",
                   qend = input$quarter_end_spec,
                   hbt = input$hbt_filter_spec)
  })

})

#Dow admissions facetted by specialty
plots$waits_spec_plot_admitted <- renderPlotly({

  withProgress(message="Loading plots ... please wait", {

  make_dow_spec_suplots(data = app_data[["dow_4wk_qtr_pub_jun"]],
                        plotdata=app_data[["hb_plotdata_jun"]],
                   specialties = input$specialty_filter,
                   n_specs = length(input$specialty_filter),
                   waiting_status = "admitted",
                   qend = input$quarter_end_spec,
                   hbt = input$hbt_filter_spec)
  })

})



## Data

numbers$spec_activity_table_output <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(spec_activity_table(input_data=app_data[["hb_plotdata_jun"]],
                         qend=input$quarter_end_spec,
                         hbt=input$hbt_filter_spec,
                         specialties=input$specialty_filter),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 22)
})
})


numbers$spec_waits_table_output <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(spec_waits_table(input_data=app_data[["dow_4wk_qtr_pub_jun"]],
                         qend=input$quarter_end_spec,
                         hbt=input$hbt_filter_spec,
                         specialties=input$specialty_filter),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 10)

  })

})
