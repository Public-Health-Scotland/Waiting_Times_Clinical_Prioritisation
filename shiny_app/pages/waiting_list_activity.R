####################### Waiting List Activity #######################

output$waiting_list_activity_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    fluidRow(

      h2("Distribution of waits"),
      p("Some text here")

      ) # fluidrow

    ) # div

})