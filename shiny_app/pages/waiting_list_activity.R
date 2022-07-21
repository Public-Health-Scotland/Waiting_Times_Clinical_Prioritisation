####################### Waiting List Activity #######################

output$activity_waiting_ui <-  renderUI({

  div(

    h2("Waiting patients"),

    fluidRow(height="50px"),

    fluidRow(
      shinydashboard::box( width=12, height="200px",
                           tagList(

                             h4("HBT summary figures")

                           ) # taglist
      ) # box

    ), #fluidRow

    fluidRow(height="200px", br()),


    fluidRow(

        tagList(

          plots[["activity_waiting"]]

        ) # taglist
      ) # fluidrow

    ) # div

})

output$activity_admitted_ui <-  renderUI({

  div(

    fluidRow(

      tagList(

        h2("Distribution of admitted patients"),
        p("Some text here"),
        plots[["activity_admitted"]]

      ) # taglist
    ) # fluidrow

  ) # div

})


plots$activity_waiting <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]], waiting_status = "waiting")})
plots$activity_admitted <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]], waiting_status = "admitted")})
plots$activity_additions <- renderPlotly({activity_trendplot(app_data[["add_perf_mar"]], waiting_status = "additions")})



