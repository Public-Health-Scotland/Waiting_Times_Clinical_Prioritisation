####################### Landing Page (Overview) #######################

output$landing_page_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
              column(width=6,
                     pickerInput("hbt_filter", "1. Select Health Board of Treatment ",
                                 choices = unique(app_data[["dow_4wk_qtr_pub"]]$nhs_board_of_treatment),
                                 selected = "NHS Scotland",
                                # pickerOptions = list(liveSearch = TRUE, showTick=TRUE),
                                 multiple = FALSE)
                  ), # column
              column(width=6,
                     radioButtons("timescale_choice", "2. Select timescale ",
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
                                                p("All boards and all specialties"),
                                                plots[["activity_waiting"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Number of patients admitted"),
                                                p("All boards and all specialties"),
                                                plots[["activity_admitted"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Additions",
                                              tagList(
                                                h3("Number of additions to list"),
                                                p("All boards and all specialties"),
                                                plots[["activity_additions"]]
                                              ) # taglist
                                     )
             ) # tabbox

    ), # fluidRow

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="500px", side="right",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Distribution of waits"),
                                                pickerInput("quarter_filter_waits_w", "3. Select quarter",
                                                            choices = c("September 2021", "December 2021", "March 2022"),
                                                            selected = "March 2022",
                                                            multiple = FALSE),
                                                plots[["waits_breakdown_waiting"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Distribution of admitted patients"),
                                                pickerInput("quarter_filter_waits_a", "3. Select quarter",
                                                            choices = c("September 2021", "December 2021", "March 2022"),
                                                            selected = "March 2022",
                                                            multiple = FALSE),
                                                plots[["waits_breakdown_admitted"]]
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow

    fluidRow(width=12, height="50px", br())

  ) # div


})


## Activity plots
plots$activity_waiting <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]],
                                                           waiting_status = "waiting",
                                                           hbt="NHS Scotland",
                                                           timescale="monthly")})
plots$activity_admitted <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]],
                                                            waiting_status = "admitted",
                                                            hbt="NHS Scotland",
                                                            timescale="monthly")})
plots$activity_additions <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]],
                                                             waiting_status = "additions",
                                                             hbt="NHS Scotland",
                                                             timescale="monthly")})


## Distribution of waits plots

plots$waits_breakdown_waiting <- renderPlotly({
  waits_distribution_plot(app_data[["dow_4wk_qtr_pub_mar"]],
                          waiting_status="waiting",
                          quarter_ending=input$quarter_filter_waits_w,
                          hbt=input$hbt_filter)})

plots$waits_breakdown_admitted <- renderPlotly({
  waits_distribution_plot(app_data[["dow_4wk_qtr_pub_mar"]],
                          waiting_status="admitted",
                          quarter_ending=input$quarter_filter_waits_a,
                          hbt=input$hbt_filter)})


