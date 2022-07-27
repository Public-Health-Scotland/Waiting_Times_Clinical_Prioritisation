####################### Distribution of waits #######################

output$distribution_waits_waiting_ui <-  renderUI({

  div(

    h2("Distribution of waits"),

    fluidRow(
      column(width=4,
             pickerInput("quarter_filter_w_dpage", "Select quarter",
                         choices = c("September 2021", "December 2021", "March 2022"),
                         selected = "March 2022",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("hbt_filter_w_dpage", "Select Health Board of Treatment ",
                         choices = unique(app_data[["dow_4wk_qtr_pub"]]$nhs_board_of_treatment),
                         selected = "NHS Scotland",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("specialty_filter_w_dpage", "Select specialty ",
                         choices = unique(app_data[["dow_4wk_qtr_pub"]]$specialty),
                         selected = "All Specialties",
                         multiple = FALSE)
      )
    ), # fluidrow

    fluidRow(

      tagList(

        plots[["waits_breakdown_waiting"]]

      ) # taglist
    ) # fluidrow

  ) # div

})

output$distribution_waits_admitted_ui <-  renderUI({

  div(

    h2("Distribution of waits"),
    p("Some text here"),

    fluidRow(
      column(width=4,
             pickerInput("quarter_filter_a_dpage", "Select quarter",
                         choices = c("September 2021", "December 2021", "March 2022"),
                         selected = "March 2022",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("hbt_filter_a_dpage", "Select Health Board of Treatment ",
                         choices = unique(app_data[["dow_4wk_qtr_pub"]]$nhs_board_of_treatment),
                         selected = "NHS Scotland",
                         multiple = FALSE)
      ),
      column(width=4,
             pickerInput("specialty_filter_a_dpage", "Select specialty ",
                         choices = unique(app_data[["dow_4wk_qtr_pub"]]$specialty),
                         selected = "All Specialties",
                         multiple = FALSE)
      )
    ), # fluidrow

    fluidRow(

      tagList(

        plots[["waits_breakdown_admitted"]]

      ) # taglist
    ) # fluidrow

  ) # div

})



