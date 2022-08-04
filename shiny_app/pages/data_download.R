####################### Data download #######################

output$download_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="300px",
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
                                   ),
                                   pickerInput("download_dataset",
                                               "4. Choose dataset    ",
                                               choices = c("Patients waiting, admitted and seen",
                                                           "Distribution of waits",
                                                           "Activity",
                                                           "Summary of patients waiting and admitted"),
                                               selected = "Patients waiting, admitted and seen",
                                               inline = TRUE,
                                               multiple = FALSE,
                                               width = "100%",
                                               options = pickerOptions(
                                                 showTick = TRUE)
                                   )

                                 )
             ) # box
    ), # fluidrow

    # Data download area
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="300px",
                                 tagList(

                                   #numbers[["data_download_table_output"]]

                                 ) # taglist

             ) # box

    ) # fluidrow

  ) # div

}) # renderUI


data_choices <- list("monthly" = c("Patients waiting, admitted and seen",
                                   "Distribution of waits",
                                   "Summary of patients waiting and admitted"),
                     "quarterly" = c("Patients waiting, admitted and seen",
                                     "Distribution of waits",
                                     "Activity",
                                     "Summary of patients waiting and admitted"))

# This makes sure that choice of datasets update depending on whether
# monthly or quarterly is selected
observeEvent(

  eventExpr=input$download_timescale,

  handlerExpr={

    if( !is.null(input$download_timescale) ) {

      updatePickerInput(session, inputId="download_dataset",
                        selected = "Patients waiting, admitted and seen",
                        choices = data_choices[[input$download_timescale]]
      )

    }

  }
)

# ## Choices of dataset for filtering
# chosen_dataset <- reactive({
#   case_when((input$download_dataset == "Patients waiting, admitted and seen" &
#                input$download_timescale == "monthly") ~ "add_perf_mon_mar",
#
#             (input$download_dataset == "Patients waiting, admitted and seen" &
#                input$download_timescale == "quarterly") ~ "add_perf_qtr_mar",
#
#             (input$download_dataset == "Distribution of waits" &
#                input$download_timescale == "monthly") ~ "add_perf_qtr_mar",
#
#             (input$download_dataset == "Distribution of waits" &
#                input$download_timescale == "quarterly") ~ "add_perf_qtr_mar",
#
#             (input$download_dataset == "Activity" &
#                input$download_timescale == "quarterly") ~ "add_perf_qtr_mar",
#
#             (input$download_dataset == "Summary of patients waiting and admitted" &
#                input$download_timescale == "monthly") ~ "add_perf_qtr_mar",
#
#             (input$download_dataset == "Summary of patients waiting and admitted" &
#                input$download_timescale == "quarterly") ~ "add_perf_qtr_mar",
#
#             TRUE ~ NULL
#
#             ) # case when
# })


#
# # Checking that a valid dataset has been chosen
# observeEvent(
#
#   eventExpr=chosen_dataset(),
#
#   handlerExpr={
#
#     validate(
#       need(!is.null(chosen_dataset()),
#            "Invalid choice of options. Please choose again.")
#     )
#
#   }
# )

## Data table to be downloaded

numbers$data_download_table_output <- DT::renderDataTable({

  DT::datatable(data.frame(c("a", "b"), c("1", "2")))

  #make_table(data_download_table(input_data=app_data[["perf_qtr_split_mar"]],
  #                            hbts=input$download_hbt,
  #                            chosen_specialties=input$download_specialty),
  #           # These columns have thousand separator added
  #           add_separator_cols = c(6),
  #           rows_to_display = 10)

})

