####################### Data download #######################

# UI ----
output$download_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="400px",
                                 tagList(
                                   p(strong("Follow the steps below to generate the dataset to download.")),
                                   p(strong("Note that the figures presented for 'All Specialities' do not match the total of the specialty breakdown. This is due to specialties with a low volume of patients at Scotland level being excluded from the specialty breakdown.")),
                                   radioButtons("download_timescale",
                                                "1. Select timescale ",
                                                choices = c("monthly", "quarterly"),
                                                selected = "monthly",
                                                inline = TRUE),
                                   pickerInput("download_hbt",
                                               "2. Select Health Boards to include (defaults to all)",
                                               choices = hb_ordered,
                                               selected = hb_ordered,
                                               inline = TRUE,
                                               multiple = TRUE,
                                               width = "100%",
                                               options = pickerOptions(liveSearch = TRUE, showTick = TRUE,
                                                                       `actions-box` = TRUE )
                                   ),
                                   pickerInput("download_dataset",
                                               "3. Choose dataset    ",
                                               choices = c("Patients added, admitted and waiting",
                                                           "Distribution of waits"),
                                               selected = "Patients added, admitted and waiting",
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


# Reactive updates ----

data_choices <- list("monthly" = c("Patients added, admitted and waiting",
                                   "Distribution of waits"),
                     "quarterly" = c("Patients added, admitted and waiting",
                                     "Distribution of waits"))

# This makes sure that choice of datasets update depending on whether
# monthly or quarterly is selected
observeEvent(

  eventExpr=input$download_timescale,

  handlerExpr={

    if( !is.null(input$download_timescale) ) {

      updatePickerInput(session, inputId="download_dataset",
                        selected = "Patients added, admitted and waiting",
                        choices = data_choices[[input$download_timescale]]
      )

    }

  }
)

# ## Choices of dataset for filtering
chosen_dataset <- reactive({

   case_when(
            (input$download_dataset == "Distribution of waits" &
               input$download_timescale == "monthly") ~ "dow_4wk_mon_jun",
            (input$download_dataset == "Distribution of waits" &
               input$download_timescale == "quarterly") ~ "dow_4wk_qtr_pub_jun",
            (input$download_dataset == "Patients added, admitted and waiting" &
               input$download_timescale == "monthly") ~ "add_perf_mon_specs_jun",
            (input$download_dataset == "Patients added, admitted and waiting" &
               input$download_timescale == "quarterly") ~ "add_perf_qtr_specs_jun",
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
                      choices = sort(unique(app_data[[chosen_dataset()]]$specialty))
        )
    }


  }
)

# Download display table ----

## Data table to be downloaded

numbers$data_download_table_output <- DT::renderDataTable({

  withProgress(message="Loading data table ... please wait", {

  make_table(data_download_table_head(input_data=app_data[[chosen_dataset()]],
                             hbts=input$download_hbt,
                             chosen_specialties=input$download_specialty),
            # These columns have thousand separator added
            add_separator_cols = c(6),
            rows_to_display = 10,
            scrollX = TRUE,
            scrollY = TRUE)
  })

})

numbers$data_download_summary_output <- renderPrint({
  withProgress(message="Loading download summary ... please wait", {
  data_download_table_summary(input_data=app_data[[chosen_dataset()]],
                                                                    hbts=input$download_hbt,
                                                                    chosen_specialties=input$download_specialty)
  })
})

# Data download button ----

## Download button
data_download <- reactive({
  withProgress(message="Loading data download ... please wait", {
  data_download_table(input_data=app_data[[chosen_dataset()]],
                      hbts=input$download_hbt,
                      chosen_specialties=input$download_specialty)
  })
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




