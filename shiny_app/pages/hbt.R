####################### Health Board of Treatment #######################

# UI ----
output$hbt_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
                                 column(width=4,
                                        pickerInput("spec_filter_hbt",
                                                    "1. Select specialty ",
                                                    choices = sort(unique(app_data[["hb_plotdata_jun"]]$specialty)),
                                                    selected = "All Specialties",
                                                    options = pickerOptions(liveSearch = TRUE, showTick=TRUE),
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("quarter_end_spec_hbt",
                                                    "2. Select quarter end date ",
                                                    choices = quarter_end_dates,
                                                    selected = "June 2022",
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("hbt_filter_hbt",
                                                    "3. Select up to six health boards ",
                                                    choices = hb_ordered,
                                                    # Selected those with most patients waiting
                                                    selected = hb_most_waiting,
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
                                                h3("Waiting list activity"),
                                                br(),
                                                plots[["activity_facet_plot_hbt"]],
                                                linebreaks(10),
                                                materialSwitch(inputId = "show_data_activity_hbt",
                                                               label = "Show data",
                                                               right = FALSE,
                                                               value = FALSE,
                                                               status = "primary"),
                                                conditionalPanel(
                                                  # Condition is in javascript
                                                  condition = "input.show_data_activity_hbt == true",
                                                  numbers[["spec_activity_table_output_hbt"]]
                                                )
                                              ) # taglist
                                     ),
                                     tabPanel("Distribution of waits",
                                              tagList(
                                                h3("Distribution of waits"),
                                                br(),
                                                fluidRow(
                                                  column(6,
                                                         plots[["waits_hbt_plot_admitted"]]
                                                         ),#colum

                                                  column(6,
                                                         plots[["waits_hbt_plot_ongoing"]]
                                                         )#column
                                                ),#fluidrow
                                                # plots[["waits_facet_plot_hbt"]],
                                                linebreaks(35),
                                                materialSwitch(inputId = "show_data_waits_hbt",
                                                               label = "Show data",
                                                               right = FALSE,
                                                               value = FALSE,
                                                               status = "primary"),
                                                conditionalPanel(
                                                  # Condition is in javascript
                                                  condition = "input.show_data_waits_hbt == true",
                                                  numbers[["spec_waits_table_output_hbt"]]
                                                )
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow


    fluidRow(width=12, height="50px", br())

  ) # div


})



## Plots ----

plots$activity_facet_plot_hbt <- renderPlotly({
  withProgress(message="Loading plots ... please wait", {
                              p1 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_jun"]],
                                    waiting_status="additions",
                                    qend=input$quarter_end_spec_hbt,
                                    hbts=input$hbt_filter_hbt,
                                    specialty_choice=input$spec_filter_hbt)

                              p2 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_jun"]],
                                                       waiting_status="admitted",
                                                       qend=input$quarter_end_spec_hbt,
                                                       hbts=input$hbt_filter_hbt,
                                                       specialty_choice=input$spec_filter_hbt)

                              p3 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_jun"]],
                                                       waiting_status="waiting",
                                                       qend=input$quarter_end_spec_hbt,
                                                       hbts=input$hbt_filter_hbt,
                                                       specialty_choice=input$spec_filter_hbt)

                              # make facets
                              subplot(style(p1, showlegend = FALSE), # keep one legend for all plots
                                      style(p2, showlegend = FALSE),
                                      p3, nrows=1, shareY = TRUE, # share axis between plots
                                      widths = c(0.3, 0.3, 0.3),
                                      titleY = TRUE)
  })
})


# Dow ongoing waits facetted by hbt
plots$waits_hbt_plot_ongoing <- renderPlotly({

  withProgress(message="Loading plots ... please wait", {

  make_dow_hbt_suplots(data = app_data[["dow_4wk_qtr_pub_jun"]],
                        healthboards = input$hbt_filter_hbt,
                        n_hbts = length(input$hbt_filter_hbt),
                        waiting_status = "waiting",
                        qend = input$quarter_end_spec_hbt,
                        spec = input$spec_filter_hbt)

  })

})

# Dow admissions facetted by hbt
plots$waits_hbt_plot_admitted <- renderPlotly({

  withProgress(message="Loading plot ... please wait", {

  make_dow_hbt_suplots(data = app_data[["dow_4wk_qtr_pub_jun"]],
                        healthboards = input$hbt_filter_hbt,
                        n_hbts = length(input$hbt_filter_hbt),
                        waiting_status = "admitted",
                        qend = input$quarter_end_spec_hbt,
                        spec = input$spec_filter_hbt)

  })

})


## Data ----

numbers$spec_activity_table_output_hbt <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(spec_activity_table_hbt(input_data=app_data[["hb_plotdata_jun"]],
                                 qend=input$quarter_end_spec_hbt,
                                 hbts=input$hbt_filter_hbt,
                                 specialty_choice=input$spec_filter_hbt),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 22)

  })

})


numbers$spec_waits_table_output_hbt <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(spec_waits_table_hbt(input_data=app_data[["dow_4wk_qtr_pub_jun"]],
                                  qend=input$quarter_end_spec_hbt,
                                  hbts=input$hbt_filter_hbt,
                                  specialty_choice=input$spec_filter_hbt),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 10)

  })

})
