####################### Landing Page (Overview) #######################

## UI ----
output$landing_page_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
              column(width=4,
                     pickerInput("hbt_filter",
                                 "1. Select Health Board of Treatment ",
                                 choices = hb_ordered,
                                 selected = "NHS Scotland",
                                 multiple = FALSE)
                  ), # column
              column(width=4,
                     pickerInput("specialty_filter_lp",
                                 "2. Select specialty ",
                                 choices = sort(unique(app_data[["add_perf_qtr_specs_jun"]]$specialty)),
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
      shinydashboard::tabBox( width=NULL, type="pills", height="1700px", side="right",

                              tabPanel("Charts",
                                       tagList(
                                         h3("Number of TTG patients added to the waiting list"),
                                         br(),
                                         # Additions

                                         # BANs
                                         # NB: The colours below are proxy colours for PHS colours
                                         # because only certain colours are accepted for this function.
                                         # Have restyled them as PHS colours in the css file.
                                         # green: phs-green; purple: phs-purple; blue: phs-blue;
                                         # fuchsia: phs-magenta; olive: phs-graphite;
                                         tags$div(class = "bans",
                                                  h4(ban_header[["additions"]]),
                                                  shinydashboard::valueBox(value="P1A-1B",
                                                                           subtitle= numbers[["ban_additions_p1"]], width=2,
                                                                           color="green"),
                                                  shinydashboard::valueBox(value="P2",
                                                                           subtitle=numbers[["ban_additions_p2"]], width=2,
                                                                           color="purple"),
                                                  shinydashboard::valueBox(value="P3",
                                                                           subtitle=numbers[["ban_additions_p3"]], width=2,
                                                                           color="blue"),
                                                  shinydashboard::valueBox(value="P4",
                                                                           subtitle=numbers[["ban_additions_p4"]], width=2,
                                                                           color="fuchsia"),
                                                  shinydashboard::valueBox(value="Other",
                                                                           subtitle=numbers[["ban_additions_other"]], width=2,
                                                                           color="olive"),
                                                  linebreaks(3)
                                         ), # div
                                         column(width = 12,
                                                plots[["activity_additions"]]),
                                         # Admitted
                                         br(),
                                         h3("Number of TTG patients admitted for treatment"),
                                         br(),
                                         tags$div(class = "bans",
                                                  h4(ban_header[["admitted"]]),
                                                  shinydashboard::valueBox(value="P1A-1B",
                                                                           subtitle= numbers[["ban_admitted_p1"]], width=2,
                                                                           color="green"),
                                                  shinydashboard::valueBox(value="P2",
                                                                           subtitle=numbers[["ban_admitted_p2"]], width=2,
                                                                           color="purple"),
                                                  shinydashboard::valueBox(value="P3",
                                                                           subtitle=numbers[["ban_admitted_p3"]], width=2,
                                                                           color="blue"),
                                                  shinydashboard::valueBox(value="P4",
                                                                           subtitle=numbers[["ban_admitted_p4"]], width=2,
                                                                           color="fuchsia"),
                                                  shinydashboard::valueBox(value="Other",
                                                                           subtitle=numbers[["ban_admitted_other"]], width=2,
                                                                           color="olive"),
                                                  linebreaks(3)
                                         ), # div
                                         # Activity plot
                                         column(width = 12,
                                                plots[["activity_admitted"]]),

                                         # Waiting
                                         br(),
                                         h3("Number of TTG patients waiting for treatment"),
                                         br(),
                                         tags$div(class = "bans",
                                                  h4(ban_header[["waiting"]]),
                                                  shinydashboard::valueBox(value="P1A-1B",
                                                                           subtitle= numbers[["ban_waiting_p1"]], width=2,
                                                                           color="green"),
                                                  shinydashboard::valueBox(value="P2",
                                                                           subtitle=numbers[["ban_waiting_p2"]], width=2,
                                                                           color="purple"),
                                                  shinydashboard::valueBox(value="P3",
                                                                           subtitle=numbers[["ban_waiting_p3"]], width=2,
                                                                           color="blue"),
                                                  shinydashboard::valueBox(value="P4",
                                                                           subtitle=numbers[["ban_waiting_p4"]], width=2,
                                                                           color="fuchsia"),
                                                  shinydashboard::valueBox(value="Other",
                                                                           subtitle=numbers[["ban_waiting_other"]], width=2,
                                                                           color="olive"),
                                                  linebreaks(3)
                                         ), # div
                                         # Activity plot
                                         column(width = 12,
                                                plots[["activity_waiting"]])

                                       ) # taglist
                              ), # tabPanel

                              tabPanel("Data",
                                       tagList(
                                         h3("Number of TTG patients added to the list,
                                                   admitted and waiting"),
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
                                                choices = get_month(unique(app_data[["perf_mon_split_jun"]]$date)),
                                                selected = "June 2022")
                                  ) # pickerInput
             ) # box
    ),

    fluidRow(width=12, height="50px", shinydashboard::box(width=NULL, height="50px", br())),

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="800px", side="right",

                                     tabPanel("Charts",
                                              tagList(
                                                column(7,
                                                       linebreaks(2),
                                                       plots[["waits_breakdown_facets"]] # facetted DoW plot
                                                ), #column
                                                column(5,
                                                       h4("Median and 90th percentile waits for TTG patients admitted for treatment"),
                                                       # shunting table down to make it more centred
                                                       numbers[["median_table_output"]],
                                                       linebreaks(2),
                                                       actionButton("btn_modal_median",
                                                                    "What is the median?",
                                                                    icon = icon('question-circle')),
                                                       linebreaks(2),
                                                       actionButton("btn_modal_90th",
                                                                    "What is the 90th percentile?",
                                                                    icon = icon('question-circle')),
                                                       linebreaks(2),
                                                       actionButton("btn_modal_cp1",
                                                                    "What what what?",
                                                                    icon = icon('question-circle')),
                                                       linebreaks(2),
                                                       actionButton("btn_modal_cp2",
                                                                    "What something else?",
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

## Reactive updates ----

timescale_choices <- list("monthly" = get_month(sort(unique(app_data[["perf_mon_split_jun"]]$date), decreasing=TRUE)),
                          "quarterly" = get_month(sort(unique(app_data[["perf_qtr_split_jun"]]$date), decreasing=TRUE)))

ban_header <- reactiveValues("additions" = renderText({glue("Number of patients added to the waiting list in the latest {gsub('ly', ' ', input$timescale_choice)}")}),
                             "admitted" = renderText({glue("Number of patients admitted for treatment in the latest {gsub('ly', ' ', input$timescale_choice)}")}),
                             "waiting" = renderText({glue("Number of patients waiting for treatment at the latest {gsub('ly', ' ', input$timescale_choice)} end")}))

# This makes sure that timescale filters on bottom box update dependent on whether
# monthly or quarterly is selected in timescale_choice
observeEvent(

  eventExpr=input$timescale_choice,

  handlerExpr={

    if( !is.null(input$timescale_choice) ) {

      updatePickerInput(session, inputId="timescale_filter_waits_f",
                        label = case_when(input$timescale_choice=="monthly" ~ "3. Select month",
                                          input$timescale_choice=="quarterly" ~ "3. Select quarter"),
                        selected = "June 2022",
                        choices = timescale_choices[[input$timescale_choice]]
      )

    }

  }
)


## BANs ----
waiting_statuses <- c("additions", "admitted", "waiting")
cps <- c("p1"="P1A-1B", "p2"="P2", "p3"= "P3", "p4"="P4", "other"="Other")

# Get number for every waiting status and CP
for (waiting_status in waiting_statuses){
  for (cpname in names(cps)){
    # Need this local to make sure that cpname and waiting_status
    # are evaluated each time and not just taken as last value
    # See https://gist.github.com/wch/5436415/
    local({
      # Making local variables
      local_cpname <- cpname
      local_waiting_status <- waiting_status

      numbers[[paste0("ban_", local_waiting_status, "_", local_cpname)]] <- renderText({
        ban(list(quarterly=app_data[["add_perf_qtr_specs_jun"]],
                 monthly=app_data[["add_perf_mon_specs_jun"]]),
            cp=cps[[local_cpname]],
            waiting_status=local_waiting_status,
            hbt=input$hbt_filter,
            chosen_specialty=input$specialty_filter_lp,
            timescale=input$timescale_choice)
      })# renderText

    }) # local
  }
}

## Activity plots ----

# plot patients waiting
plots$activity_waiting <- renderPlotly({
  withProgress(message="Loading plot ... please wait", {
    activity_trendplot(list(quarterly=app_data[["add_perf_qtr_specs_jun"]],
                            monthly=app_data[["add_perf_mon_specs_jun"]]),
                       waiting_status = "waiting",
                       hbt=input$hbt_filter,
                       chosen_specialty = input$specialty_filter_lp,
                       timescale=input$timescale_choice)
  })
})
# plot patients admitted
plots$activity_admitted <- renderPlotly({
  withProgress(message="Loading plot ... please wait", {
    activity_trendplot(list(quarterly=app_data[["add_perf_qtr_specs_jun"]],
                            monthly=app_data[["add_perf_mon_specs_jun"]]),
                       waiting_status = "admitted",
                       hbt=input$hbt_filter,
                       chosen_specialty = input$specialty_filter_lp,
                       timescale=input$timescale_choice)
  })
})

# plot additions to the list
plots$activity_additions <- renderPlotly({
  withProgress(message="Loading plot ... please wait", {
    activity_trendplot(list(quarterly=app_data[["add_perf_qtr_specs_jun"]],
                            monthly=app_data[["add_perf_mon_specs_jun"]]),
                       waiting_status = "additions",
                       hbt=input$hbt_filter,
                       chosen_specialty = input$specialty_filter_lp,
                       timescale=input$timescale_choice)
  })
})




## Distribution of waits plots ----

plots$waits_breakdown_facets <- renderPlotly({
  withProgress(message="Loading plot ... please wait", {

    # DoW plot patients waiting
    p4 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_jun"]],
                                       monthly=app_data[["dow_4wk_mon_jun"]]),
                                  waiting_status="waiting",
                                  timescale=input$timescale_choice,
                                  chosen_specialty = input$specialty_filter_lp,
                                  time_chunk_end=input$timescale_filter_waits_f,
                                  hbt=input$hbt_filter)

    # DoW plot patients admitted
    p5 <- waits_distribution_plot(list(quarterly=app_data[["dow_4wk_qtr_pub_jun"]],
                                       monthly=app_data[["dow_4wk_mon_jun"]]),
                                waiting_status="admitted",
                                timescale=input$timescale_choice,
                                chosen_specialty = input$specialty_filter_lp,
                                time_chunk_end=input$timescale_filter_waits_f,
                                hbt=input$hbt_filter)

  # Create annotations for graphs
  annotations = make_annotation()

  # make facets
  subplot(style(p5, showlegend = FALSE), p4,
          nrows = 2, titleY = TRUE, shareX = TRUE) %>%
    layout(annotations = annotations)
  })

})

## Activity numbers ----
numbers$activity_table_output <- DT::renderDataTable({
  withProgress(message="Loading data table ... please wait", {

  make_table(activity_table(list(quarterly=app_data[["add_perf_qtr_specs_jun"]],
                                 monthly=app_data[["add_perf_mon_specs_jun"]]),
                            hbt=input$hbt_filter,
                            chosen_specialty = input$specialty_filter_lp,
                            timescale=input$timescale_choice),
             rows_to_display = 25,
             # These columns have thousand separator added
             add_separator_cols = c(7,8,9))
  })

})

## Distribution of waits numbers ----

# Median and 90th percentile
numbers$median_table_output <- DT::renderDataTable({
  withProgress(message="Loading data table ... please wait", {

    info_table(median_byurgency_table(list(quarterly=app_data[["perf_qtr_split_jun"]],
                                           monthly=app_data[["perf_mon_split_jun"]]),
                     timescale=input$timescale_choice,
                     chosen_specialty = input$specialty_filter_lp,
                     time_chunk_end=input$timescale_filter_waits_f,
                     hbt=input$hbt_filter)
                 )

  })


})



# Raw data table
numbers$waits_table_output <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(waits_table(list(quarterly=app_data[["dow_4wk_qtr_pub_jun"]],
                              monthly=app_data[["dow_4wk_mon_jun"]]),
                            hbt=input$hbt_filter,
                            time_chunk_end=input$timescale_filter_waits_f,
                            chosen_specialty = input$specialty_filter_lp,
                            timescale=input$timescale_choice),
             # These columns have thousand separator added
             add_separator_cols = c(3),
             rows_to_display = 10)

  })

})
