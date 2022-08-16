####################### Landing Page (Overview) #######################

output$landing_page_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
              column(width=4,
                     pickerInput("hbt_filter",
                                 "1. Select Health Board of Treatment ",
                                 choices = unique(app_data[["ongoing_completed_for_MI_all_specs"]]$nhs_board_of_treatment),
                                 selected = "NHS Scotland",
                                 multiple = FALSE)
                  ), # column
              column(width=4,
                     pickerInput("specialty_filter",
                                 "2. Select specialty ",
                                 choices = unique(app_data[["ongoing_completed_for_MI_all_specs"]]$specialty),
                                 selected = "All Specialties",
                                 multiple = FALSE)
              ), # column
              column(width=4,
                     radioButtons("timescale_choice",
                                  "3. Select timescale ",
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
                                                h3("Number of TTG patients added to the list, admitted and waiting"),
                                                br(),
                                                column(width = 12,
                                                       plots[["activity_stacked"]])
                                                )

                                     ),

                                     tabPanel("Data",
                                              tagList(
                                                h3("Number of TTG patients added to the list, admitted and waiting"),
                                                br(),
                                                numbers[["activity_table_output"]])
                                     )


             ) # box

    ), # fluidRow

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12,
             shinydashboard::box( width=NULL, height="100px",
                                  tagList(
                                    h3("Distribution of waits for patients admitted and waiting"),
                                    pickerInput("timescale_filter_waits_f", "4. Select month",
                                                choices = get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                                                selected = "March 2022")
                                              ) # pickerInput
             ) # box
    ),

    fluidRow(width=12, height="50px", shinydashboard::box(width=NULL, height="50px", br())),

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="800px", side="right",

                                     tabPanel("Charts",
                                              tagList(
                                                column(7,
                                                       br(), br(),
                                                       plots[["waits_breakdown_facets"]] # facetted DoW plot
                                              ), #column
                                              column(5,
                                                     # shunting table down to make it more centred
                                                     numbers[["median_table_output"]],
                                                     br(), br(),
                                                     actionButton("btn_modal_median",
                                                                  "What is the median?",
                                                                  icon = icon('question-circle')),
                                                     actionButton("btn_modal_90th",
                                                                  "What is the 90th percentile?",
                                                                  icon = icon('question-circle'))
                                              ) # column
                                              ) #taglist

                                      ), # tabpanel

                                     tabPanel("Data",
                                              tagList(
                                                numbers[["waits_table_output"]]
                                              ) #taglist

                                     ) # tabpanel


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
                              chosen_specialty = input$specialty_filter,
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
  subplot(style(p3, showlegend = FALSE), # keep one legend for all plots
          style(p2, showlegend = FALSE),
          p1, nrows = 3, shareX = TRUE, # share axis between plots
          heights = c(0.3, 0.3, 0.3),
          titleY = TRUE)
})



## Distribution of waits plots

plots$waits_breakdown_facets <- renderPlotly({

  # DoW plot patients waiting
  p4 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_mar"]],
                                      monthly=app_data[["dow_4wk_mon_mar"]]),
                                 waiting_status="waiting",
                                 timescale=input$timescale_choice,
                                 chosen_specialty = input$specialty_filter,
                                 time_chunk_end=input$timescale_filter_waits_f,
                                 hbt=input$hbt_filter)

  # DoW plot patients admitted
  p5 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_mar"]],
                                     monthly=app_data[["dow_4wk_mon_mar"]]),
                                waiting_status="admitted",
                                timescale=input$timescale_choice,
                                chosen_specialty = input$specialty_filter,
                                time_chunk_end=input$timescale_filter_waits_f,
                                hbt=input$hbt_filter)

  # make facets
  subplot(style(p5, showlegend = FALSE), p4,
          nrows = 2, titleY = TRUE, shareX = TRUE)

})

## Activity numbers
numbers$activity_table_output <- DT::renderDataTable({

  make_table(activity_table(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                 monthly=app_data[["add_perf_mon_mar"]]),
                            hbt=input$hbt_filter,
                            chosen_specialty = input$specialty_filter,
                            timescale=input$timescale_choice),
             # These columns have thousand separator added
             add_separator_cols = c(4,5))

})

numbers$activity_ban_P1 <- activity_ban(value = "1",
                                            color = waiting_times_palette[1],
                                            subtitle = "P1A-1B")
numbers$activity_ban_P2 <- activity_ban(value = "1",
                                        color = waiting_times_palette[2],
                                        subtitle = "P2")
numbers$activity_ban_P3 <- activity_ban(value = "1",
                                        color = waiting_times_palette[3],
                                        subtitle = "P3")
numbers$activity_ban_P4 <- activity_ban(value = "1",
                                        color = waiting_times_palette[4],
                                        subtitle = "P4")
numbers$activity_ban_Other <- activity_ban(value = "1",
                                        color = waiting_times_palette[5],
                                        subtitle = "Other")

## Distribution of waits numbers

# Median and 90th percentile
numbers$median_table_output <- DT::renderDataTable({

    info_table(median_byurgency_table(list(quarterly=app_data[["perf_qtr_split_mar"]],
                          monthly=app_data[["perf_mon_split_mar"]]),
                     timescale=input$timescale_choice,
                     chosen_specialty = input$specialty_filter,
                     time_chunk_end=input$timescale_filter_waits_f,
                     hbt=input$hbt_filter)
                 )


})



# Raw data table
numbers$waits_table_output <- DT::renderDataTable({

  make_table(waits_table(list(quarterly=app_data[["dow_4wk_qtr_pub_mar"]],
                              monthly=app_data[["dow_4wk_mon_mar"]]),
                            hbt=input$hbt_filter,
                            time_chunk_end=input$timescale_filter_waits_f,
                            chosen_specialty = input$specialty_filter,
                            timescale=input$timescale_choice),
             # These columns have thousand separator added
             add_separator_cols = c(3),
             rows_to_display = 10)

})
