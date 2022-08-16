####################### Data download #######################

output$download_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="400px",
                                 tagList(
                                   p(strong("Follow the steps below to generate the dataset to download.")),
                                   radioButtons("download_timescale",
                                                "1. Select timescale ",
                                                choices = c("monthly", "quarterly"),
                                                selected = "monthly",
                                                inline = TRUE),
                                   pickerInput("download_hbt",
                                               "2. Select Health Boards to include (defaults to all)",
                                               choices = unique(app_data[["perf_qtr_split_jun"]]$nhs_board_of_treatment),
                                               selected = unique(app_data[["perf_qtr_split_jun"]]$nhs_board_of_treatment),
                                               inline = TRUE,
                                               multiple = TRUE,
                                               width = "100%",
                                               options = pickerOptions(liveSearch = TRUE, showTick = TRUE,
                                                                       `actions-box` = TRUE )
                                   ),
                                   pickerInput("download_dataset",
                                               "3. Choose dataset    ",
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
                                   ),
                                   pickerInput("download_specialty",
                                               "4. Select specialties to include (defaults to all)",
                                               choices = "All Specialties",
                                               selected = "All Specialties",
                                               inline = TRUE,
                                               multiple = TRUE,
                                               width = "100%",
                                               options = pickerOptions(
                                                 showTick = TRUE,
                                                 liveSearch =TRUE,
                                                 `actions-box` = TRUE)
                                   ),
                                   pickerInput("download_filetype",
                                               "5. Choose file type   ",
                                               choices = c(".csv", ".xlsx"),
                                               selected = ".csv",
                                               inline = TRUE,
                                               multiple = FALSE,
                                               width = "100%",
                                               options = pickerOptions(
                                                 showTick = TRUE)
                                   )

                                 )
             ) # box
    ), # fluidrow

    fluidRow(width=12, height="100px", br()),

    # Data download area
    fluidRow(width=12,
             h3("Download data "),
             downloadButton("data_download_output",
                            "Download data"),
             shinydashboard::tabBox(width=NULL, type="pills", side="right", height="800px",

                                    tabPanel("Data summary",
                                      tagList(

                                        h3("Summary of data to download: "),
                                        linebreaks(1),
                                        numbers[["data_download_summary_output"]]


                                        ) # taglist
                                    ), # tabpanel
                                    tabPanel("Data preview",
                                      tagList(

                                        h3("Preview of data to download (first 10 rows): "),
                                        linebreaks(1),
                                        numbers[["data_download_table_output"]]

                                             ))

             ) # box

    ), # fluidrow

    fluidRow(width=12, height="50px", br())

  ) # div

}) # renderUI


### ---- CHOOSING CORRECT DATASET

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
chosen_dataset <- reactive({

   case_when((input$download_dataset == "Patients waiting, admitted and seen" &
               input$download_timescale == "monthly") ~ "add_perf_mon_jun",
            (input$download_dataset == "Patients waiting, admitted and seen" &
               input$download_timescale == "quarterly") ~ "add_perf_qtr_jun",
            (input$download_dataset == "Distribution of waits" &
               input$download_timescale == "monthly") ~ "dow_4wk_mon_jun",
            (input$download_dataset == "Distribution of waits" &
               input$download_timescale == "quarterly") ~ "dow_4wk_qtr_jun",
            (input$download_dataset == "Activity" &
               input$download_timescale == "quarterly") ~ "hb_plotdata_jun",
            (input$download_dataset == "Summary of patients waiting and admitted" &
               input$download_timescale == "monthly") ~ "perf_mon_split_jun",
            (input$download_dataset == "Summary of patients waiting and admitted" &
               input$download_timescale == "quarterly") ~ "perf_qtr_split_jun",
            TRUE ~ "no_choice"

            ) # case when
})



observeEvent(

  eventExpr=chosen_dataset(),

  handlerExpr={

    if( !is.null(chosen_dataset()) ) {

      # Checking that a valid dataset has been chosen
      validate(
        need(!(chosen_dataset()=="no_choice"),
           "Invalid choice of options. Please choose again.")
      )

     # Updating specialty choice selection
     updatePickerInput(session, inputId="download_specialty",
                      selected = unique(app_data[[chosen_dataset()]]$specialty),
                      choices = unique(app_data[[chosen_dataset()]]$specialty)
        )
    }


  }
)

# ---- GETTING DOWNLOAD DISPLAY TABLE

## Data table to be downloaded

numbers$data_download_table_output <- DT::renderDataTable({

  make_table(data_download_table_head(input_data=app_data[[chosen_dataset()]],
                             hbts=input$download_hbt,
                             chosen_specialties=input$download_specialty),
            # These columns have thousand separator added
            add_separator_cols = c(6),
            rows_to_display = 4,
            scrollX = TRUE,
            scrollY = TRUE)

})

numbers$data_download_summary_output <- renderPrint({
  data_download_table_summary(input_data=app_data[[chosen_dataset()]],
                                                                    hbts=input$download_hbt,
                                                                    chosen_specialties=input$download_specialty)
})

# ---- DATA DOWNLOAD BUTTON

## Download button
data_download <- reactive({
  data_download_table(input_data=app_data[[chosen_dataset()]],
                      hbts=input$download_hbt,
                      chosen_specialties=input$download_specialty)
})

output$data_download_output <- downloadHandler(
  filename = function(){
    if(input$download_filetype == ".csv"){
      "CP_data.csv"
    } else if (input$download_filetype == ".xlsx"){
      "CP_data.xlsx"
    } else {
      "invalid"
    }
  },
  content = function(file) {

    if(input$download_filetype == ".csv"){
      write.csv(data_download(),
                file,
                row.names=FALSE)
    } else if (input$download_filetype == ".xlsx"){
      openxlsx::write.xlsx(data_download(), file)
    } else {
      validate(TRUE, "Invalid download file type selected")
    }

  })




