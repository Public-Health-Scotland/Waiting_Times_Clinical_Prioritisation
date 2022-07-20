####################### Waiting List Activity #######################

output$activity_waiting_ui <-  renderUI({

  div(

    fluidRow(

        tagList(

          h2("Distribution of waits"),
          p("Some text here"),
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

plots$activity_waiting <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "waiting")})
plots$activity_admitted <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "admitted")})

