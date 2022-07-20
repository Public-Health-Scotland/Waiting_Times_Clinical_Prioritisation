####################### Waiting List Activity #######################

output$waiting_list_activity_ui <-  renderUI({

  div(

    fluidRow(

        tagList(

          h2("Distribution of waits"),
          p("Some text here"),
          withSpinner(plotlyOutput("activity_waiting"))

        ) # taglist
      ) # fluidrow

    ) # div

})

output$activity_waiting <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "waiting")})
output$activity_admitted <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "admitted")})