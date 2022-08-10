####################### Health Board of Treatment #######################

output$hbt_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
                                 column(width=4,
                                        pickerInput("spec_filter_hbt",
                                                    "1. Select specialty ",
                                                    choices = unique(app_data[["hb_plotdata_mar"]]$specialty),
                                                    selected = "All Specialties",
                                                    options = pickerOptions(liveSearch = TRUE, showTick=TRUE),
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("quarter_end_spec_hbt",
                                                    "2. Select quarter end date ",
                                                    choices = c("September 2021", "December 2021", "March 2022"),
                                                    selected = "March 2022",
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("hbt_filter_hbt",
                                                    "3. Select up to six health boards ",
                                                    choices = unique(app_data[["dow_4wk_qtr_pub_mar"]]$nhs_board_of_treatment),
                                                    selected = c("Golden Jubilee National Hospital",
                                                                 "NHS Ayrshire & Arran",
                                                                 "NHS Borders",
                                                                 "NHS Dumfries & Galloway",
                                                                 "NHS Fife",
                                                                 "NHS Forth Valley"),
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
                                                plots[["waits_facet_plot_hbt"]],
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



## Plots

plots$activity_facet_plot_hbt <- renderPlotly({
                              p1 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_mar"]],
                                    waiting_status="additions",
                                    qend=input$quarter_end_spec_hbt,
                                    hbts=input$hbt_filter_hbt,
                                    specialty_choice=input$spec_filter_hbt)
                              
                              p2 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_mar"]],
                                                       waiting_status="admitted",
                                                       qend=input$quarter_end_spec_hbt,
                                                       hbts=input$hbt_filter_hbt,
                                                       specialty_choice=input$spec_filter_hbt)
                              
                              p3 <- activity_specs_hbt(input_data=app_data[["hb_plotdata_mar"]],
                                                       waiting_status="waiting",
                                                       qend=input$quarter_end_spec_hbt,
                                                       hbts=input$hbt_filter_hbt,
                                                       specialty_choice=input$spec_filter_hbt)
                              
                              # make facets
                              subplot(style(p1, showlegend = FALSE), # keep one legend for all plots
                                      style(p2, showlegend = FALSE),
                                      p3, nrows = 3, shareX = TRUE, # share axis between plots
                                      # heights = c(0.3, 0.3, 0.3),
                                      titleY = TRUE)
                                })


plots$waits_facet_plot_hbt <- renderPlotly({waits_specs_hbt(input_data=app_data[["dow_4wk_qtr_pub_mar"]],
                                                           qend=input$quarter_end_spec_hbt,
                                                           hbts=input$hbt_filter_hbt,
                                                           specialty_choice=input$spec_filter_hbt)})



## Data

numbers$spec_activity_table_output_hbt <- DT::renderDataTable({

  make_table(spec_activity_table_hbt(input_data=app_data[["hb_plotdata_mar"]],
                                 qend=input$quarter_end_spec_hbt,
                                 hbts=input$hbt_filter_hbt,
                                 specialty_choice=input$spec_filter_hbt),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 22)

})


numbers$spec_waits_table_output_hbt <- DT::renderDataTable({

  make_table(spec_waits_table_hbt(input_data=app_data[["dow_4wk_qtr_pub_mar"]],
                                  qend=input$quarter_end_spec_hbt,
                                  hbts=input$hbt_filter_hbt,
                                  specialty_choice=input$spec_filter_hbt),
             # These columns have thousand separator added
             add_separator_cols = c(6),
             rows_to_display = 10)

})
