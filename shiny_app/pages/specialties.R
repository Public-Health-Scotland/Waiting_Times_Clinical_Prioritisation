####################### Specialties #######################

output$specialties_ui <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),


    # Filters and toggles
    fluidRow(width=12,
             shinydashboard::box(width=NULL, height="100px",
                                 column(width=4,
                                        pickerInput("hbt_filter_spec",
                                                    "1. Select Health Board ",
                                                    choices = unique(app_data[["dow_4wk_qtr_pub_mar"]]$nhs_board_of_treatment),
                                                    selected = "NHS Scotland",
                                                    options = pickerOptions(liveSearch = TRUE, showTick=TRUE),
                                                    multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("quarter_end_spec",
                                                     "2. Select quarter end date ",
                                                     choices = c("September 2021", "December 2021", "March 2022"),
                                                     selected = "March 2022",
                                                     multiple = FALSE)
                                 ), # column
                                 column(width=4,
                                        pickerInput("specialty_filter",
                                                    "3. Select specialties ",
                                                    choices = unique(app_data[["hb_plotdata_mar"]]$specialty),
                                                    selected = c("Orthopaedics", "General Surgery",
                                                                 "Opthalmology", "Urology",
                                                                 "Ear, Nose & Throat", "Gynaecology"),
                                                    options = pickerOptions(
                                                      liveSearch=TRUE,
                                                      maxOptions = 6,
                                                      maxOptionsText="Choose up to six options"),

                                                    multiple = TRUE)
                                 ) # column
             ) # box
    ), # fluidrow

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="1000px", side="right",
                                     tabPanel("Activity",
                                              tagList(
                                                h3("Activity"),
                                                plots[["activity_facet_plot"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Distribution of waits",
                                              tagList(
                                                h3("Distribution of waits")
                                              ) # taglist
                                     )
             ) # tabbox
    ) # fluidRow

  ) # div


})


observe({
  if( (!is.null(input$quarter_end_spec) & (!is.null(input$hbt_filter_spec))) ) {
    updateSelectInput(session, "specialty_filter",
                      selected = topsix_specs(input$quarter_end_spec,
                                              input$hbt_filter_spec) )
  }
})



## Plots

plots$activity_facet_plot <- renderPlotly({activity_specs(input_data=app_data[["hb_plotdata"]],
                                                    qend=input$quarter_end_spec,
                                                    hbt=input$hbt_filter_spec,
                                                    specialties=input$specialty_filter)})
