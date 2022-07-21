####################### Distribution of waits #######################

output$distribution_waits_ui <-  renderUI({

  div(

    fluidRow(

      tagList(

        h2("Distribution of waits"),
        p("Some text here")
      ) # taglist
    ) # fluidrow

  ) # div

})