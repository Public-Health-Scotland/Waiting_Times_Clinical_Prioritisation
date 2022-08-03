####################### Landing Page (Overview) #######################

output$landing_page_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
              column(width=6,
                     pickerInput("hbt_filter",
                                 "1. Select Health Board of Treatment ",
                                 choices = unique(app_data[["perf_qtr_split_mar"]]$nhs_board_of_treatment),
                                 selected = "NHS Scotland",
                                # pickerOptions = list(liveSearch = TRUE, showTick=TRUE),
                                 multiple = FALSE)
                  ), # column
              column(width=6,
                     radioButtons("timescale_choice",
                                  "2. Select timescale ",
                                 choices = c("monthly", "quarterly"),
                                 selected = "monthly",
                                 inline=TRUE)
                  ) # column
             ) # box
    ),


    fluidRow(
             shinydashboard::tabBox( width=NULL, type="pills", height="1000px", side="right",

                                     tabPanel("Charts",
                                              tagList(
                                                h3("Number of patients waiting, admitted and seen"),
                                                br(),
                                                plots[["activity_stacked"]])
                                     ),

                                     tabPanel("Data",
                                              tagList(
                                                h3("Number of patients waiting, admitted and seen"),
                                                br(),
                                                numbers[["activity_table_output"]])
                                     )


             ) # box

    ), # fluidRow

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12,
             shinydashboard::box( width=NULL, type="pills", height="800px", side="right",
                                    column(12,
                                           tagList(
                                             h3("Distribution of patients waiting and admitted"),
                                             pickerInput("timescale_filter_waits_f", "3. Select month",
                                                         choices = get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                                                         selected = "March 2022")
                                           ),
                                    br(),
                                    column(7,
                                           plots[["waits_breakdown_facets"]] # facetted DoW plot
                                    ), #column
                                    column(5,
                                           # shunting table down to make it more centred
                                           br(), br(),
                                           actionButton("btn_modal_median",
                                                        "What is the median?",
                                                        icon = icon('question-circle')),
                                           br(), br(), br(),
                                           numbers[["median_table_output"]]
                                    ) # column
                                  ) #taglist

             ) # box
    ), # fluidRow

    fluidRow(width=12, height="50px", br())

  ) # div


})

timescale_choices <- list("monthly" = get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                          "quarterly" = get_month(unique(app_data[["perf_qtr_split_mar"]]$date)))

# This makes sure that timescale filters on bottom box update dependent on whether
# monthly or quarterly is selected in timescale_choice
observeEvent(

  eventExpr=input$timescale_choice,

  handlerExpr={

  if( !is.null(input$timescale_choice) ) {

        updatePickerInput(session, inputId="timescale_filter_waits_f",
                      label = case_when(input$timescale_choice=="monthly" ~ "3. Select month",
                                        input$timescale_choice=="quarterly" ~ "3. Select quarter"),
                      selected = "March 2022",
                      choices = timescale_choices[[input$timescale_choice]]
    )

  }

  }
)





## Activity plots
plots$activity_stacked <- renderPlotly({

  # plot patients waiting
  p1 <- activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                   monthly=app_data[["add_perf_mon_mar"]]),
                              waiting_status = "waiting",
                              hbt=input$hbt_filter,
                              timescale=input$timescale_choice)
  # plot patients admitted
  p2 <- activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                   monthly=app_data[["add_perf_mon_mar"]]),
                              waiting_status = "admitted",
                              hbt=input$hbt_filter,
                              timescale=input$timescale_choice)
  # plot additions to the list
  p3 <- activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                   monthly=app_data[["add_perf_mon_mar"]]),
                              waiting_status = "additions",
                              hbt=input$hbt_filter,
                              timescale=input$timescale_choice)

  # make facets
  subplot(style(p1, showlegend = FALSE), # keep one legend for all plots
          style(p2, showlegend = FALSE),
          p3, nrows = 3, shareX = TRUE, # share axis between plots
          titleY = TRUE) # keep subplot titles


})

## Distribution of waits plots

plots$waits_breakdown_facets <- renderPlotly({

  # DoW plot patients waiting
  p4 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_mar"]],
                                      monthly=app_data[["dow_4wk_mon_mar"]]),
                                 waiting_status="waiting",
                                 timescale=input$timescale_choice,
                                 time_chunk_end=input$timescale_filter_waits_f,
                                 hbt=input$hbt_filter)

  # DoW plot patients admitted
  p5 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_mar"]],
                                     monthly=app_data[["dow_4wk_mon_mar"]]),
                                waiting_status="admitted",
                                timescale=input$timescale_choice,
                                time_chunk_end=input$timescale_filter_waits_f,
                                hbt=input$hbt_filter)

  # make facets
  subplot(style(p4, showlegend = FALSE), p5,
          nrows = 2, titleY = TRUE, shareX = TRUE)

})

## Activity numbers
numbers$activity_table_output <- DT::renderDataTable({

  make_table(activity_table(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                 monthly=app_data[["add_perf_mon_mar"]]),
                            hbt=input$hbt_filter,
                            timescale=input$timescale_choice),
             # These columns have thousand separator added
             add_separator_cols = c(4,5))

})


## Distribution of waits numbers

numbers$median_table_output <- DT::renderDataTable({

    info_table(median_byurgency_table(list(quarterly=app_data[["perf_qtr_split_mar"]],
                          monthly=app_data[["perf_mon_split_mar"]]),
                     timescale=input$timescale_choice,
                     time_chunk_end=input$timescale_filter_waits_f,
                     hbt=input$hbt_filter)
                 )


})