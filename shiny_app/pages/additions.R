####################### Additions by HBR #######################

output$additions_ui <-  renderUI({

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
    ), # fluidrow

    fluidRow(

      tagList(

        plots[["additions_byboard"]]

      ) # taglist
    ) # fluidrow

  ) # div

})

plots$additions_byboard <- renderPlotly({
  additions_trendplot_byboard(app_data[["addhbr_mar"]],
                              hbt=input$hbt_filter_wpage,
                              hbr=input$hbr_filter_wpage,
                              chosen_specialty=input$specialty_filter_wpage)})