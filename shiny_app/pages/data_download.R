####################### Data download #######################

output$download_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="1000px",
                                 tagList(
                                   p(strong("Follow the steps below to generate the dataset to download.")),
                                   radioButtons("download_timescale",
                                                "1. Select timescale ",
                                                choices = c("monthly", "quarterly"),
                                                selected = "monthly",
                                                inline = TRUE),
                                   pickerInput("download_hbt",
                                               "2. Select Health Boards to include (defaults to all)",
                                               choices = unique(app_data[["perf_qtr_split_mar"]]$nhs_board_of_treatment),
                                               selected = unique(app_data[["perf_qtr_split_mar"]]$nhs_board_of_treatment),
                                               inline = TRUE,
                                               multiple = TRUE,
                                               width = "100%",
                                               options = pickerOptions(liveSearch = TRUE, showTick = TRUE)
                                   ),
                                   pickerInput("download_specialty",
                                               "3. Select specialties to include (defaults to all)",
                                               choices = unique(app_data[["hb_plotdata_mar"]]$specialty),
                                               selected = unique(app_data[["hb_plotdata_mar"]]$specialty),
                                               inline = TRUE,
                                               multiple = TRUE,
                                               width = "100%",
                                               options = pickerOptions(
                                                 showTick = TRUE,
                                                 liveSearch =TRUE)
                                   )

                                 )
             ) # box
    ) # fluidrow

  ) # div

}) # renderUI

