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

    # BANs
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="200px",
                                 tagList(

                                   h3("BANs"),
                                   p("Some numbers go here")

                                 ) # taglist
                                 ) # box
             ), # fluidrow

    fluidRow(
             shinydashboard::tabBox( width=NULL, type="pills", height="500px", side="right",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Number of patients waiting"),
                                                br(),
                                                plots[["activity_waiting"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Number of patients admitted"),
                                                br(),
                                                plots[["activity_admitted"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Additions",
                                              tagList(
                                                h3("Number of additions to list"),
                                                br(),
                                                plots[["activity_additions"]]
                                              ) # taglist
                                     )
             ) # tabbox

    ), # fluidRow

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="600px", side="right",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Distribution of waits"),
                                                pickerInput("timescale_filter_waits_w", "3. Select month",
                                                            choices = get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                                                            selected = "March 2022"),
                                                column(8,
                                                       p()
                                            #           plots[["waits_breakdown_waiting"]]
                                                ), # column
                                                column(4,
                                                       p("Total figs")
                                                       )
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Distribution of admitted patients"),
                                                pickerInput("timescale_filter_waits_a", "3. Select month",
                                                            choices = NULL,
                                                            selected = NULL),
                                                column(8,
                                                       p()
                                             #          plots[["waits_breakdown_admitted"]]
                                                ), # column
                                                column(4,
                                                       p("Total figs")
                                                )
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow

    fluidRow(width=12, height="50px", br())

  ) # div


})


# This makes sure that timescale filters on bottom box update dependent on whether
# monthly or quarterly is selected in timescale_choice
observeEvent(

  eventExpr=input$timescale_choice,

  handlerExpr={

  if( !is.null(input$timescale_choice) ) {
    updateSelectInput(session, inputId="timescale_filter_waits_w",
                      label = case_when(input$timescale_choice=="monthly" ~ "3. Select month",
                                        input$timescale_choice=="quarterly" ~ "3. Select quarter"),
                      selected = "March 2022",
                      choices = case_when(input$timescale_choice=="monthly" ~ get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                                          input$timescale_choice=="quarterly" ~ get_month(unique(app_data[["perf_qtr_split_mar"]]$date)))
                      )

    updatePickerInput(session, inputId="timescale_filter_waits_a",
                      label = case_when(input$timescale_choice=="monthly" ~ "3. Select month",
                                        input$timescale_choice=="quarterly" ~ "3. Select quarter"),
                      selected = "March 2022",
                      choices = case_when(input$timescale_choice=="monthly" ~ get_month(unique(app_data[["perf_mon_split_mar"]]$date)),
                                          input$timescale_choice=="quarterly" ~ get_month(unique(app_data[["perf_qtr_split_mar"]]$date)))
    )
  }

  }
)





## Activity plots
plots$activity_waiting <- renderPlotly({activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                                             monthly=app_data[["add_perf_mon_mar"]]),
                                                           waiting_status = "waiting",
                                                           hbt=input$hbt_filter,
                                                           timescale=input$timescale_choice)})
plots$activity_admitted <- renderPlotly({activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                                                 monthly=app_data[["add_perf_mon_mar"]]),
                                                            waiting_status = "admitted",
                                                            hbt=input$hbt_filter,
                                                            timescale=input$timescale_choice)})
plots$activity_additions <- renderPlotly({activity_trendplot(list(quarterly=app_data[["add_perf_qtr_mar"]],
                                                                  monthly=app_data[["add_perf_mon_mar"]]),
                                                             waiting_status = "additions",
                                                             hbt=input$hbt_filter,
                                                             timescale=input$timescale_choice)})


## Distribution of waits plots

# plots$waits_breakdown_waiting <- renderPlotly({
#   waits_distribution_plot(list(quarterly=app_data[["perf_qtr_split_mar"]],
#                                monthly=app_data[["perf_mon_split_mar"]]),
#                           waiting_status="waiting",
#                           timescale=input$timescale_choice,
#                           quarter_ending=input$quarter_filter_waits_w,
#                           hbt=input$hbt_filter)})
#
# plots$waits_breakdown_admitted <- renderPlotly({
#   waits_distribution_plot(list(quarterly=app_data[["perf_qtr_split_mar"]],
#                                monthly=app_data[["perf_mon_split_mar"]]),
#                           waiting_status="admitted",
#                           timescale=input$timescale_choice,
#                           quarter_ending=input$quarter_filter_waits_a,
#                           hbt=input$hbt_filter)})
#
#
