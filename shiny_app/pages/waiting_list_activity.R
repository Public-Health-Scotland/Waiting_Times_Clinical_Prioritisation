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

output$activity_additions_ui <-  renderUI({

  div(

    h2("Distribution of additions"),
    p("Some text here"),

    fluidRow(
      column(width=4,
      pickerInput("hbr_filter_wpage", "Select Health Board of Residence ",
                         choices = c("NHS SCOTLAND", unique(app_data[["addhbr"]]$health_board_of_residence)),
                         selected = "NHS SCOTLAND",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("hbt_filter_wpage", "Select Health Board of Treatment ",
                         choices = unique(app_data[["addhbr"]]$nhs_board_of_treatment),
                         selected = "NHS Scotland",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("specialty_filter_wpage", "Select specialty ",
                         choices = unique(app_data[["addhbr"]]$specialty),
                         selected = "All Specialties",
                         multiple = FALSE)
      )
    ),

    fluidRow(

      tagList(

        plots[["additions_byboard"]]

      ) # taglist
    ) # fluidrow

  ) # div

})

plots$activity_waiting <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "waiting")})
plots$activity_admitted <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "admitted")})
plots$activity_additions <- renderPlotly({activity_trendplot(app_data[["add_perf"]], waiting_status = "additions")})

plots$additions_byboard <- renderPlotly({
  additions_trendplot_byboard(app_data[["addhbr"]],
                              hbt=input$hbt_filter_wpage,
                              hbr=input$hbr_filter_wpage,
                              chosen_specialty=input$specialty_filter_wpage)})


