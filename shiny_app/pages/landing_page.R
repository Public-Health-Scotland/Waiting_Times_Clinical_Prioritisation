####################### Landing Page (Overview) #######################

output$landing_page_info <-  renderUI({

div(

  fluidRow(width=12, height="50px", br()),

  fluidRow(width=12, height="100px", pickerInput("hbt_filter", "Select Health Board of Treatment ",
                                                choices = c("a", "b", "c"),
                                                selected = c("a"),
                                                multiple = FALSE)),

  fluidRow(
           shinydashboard::tabBox( width=NULL, type="pills", height="200px",
                 tabPanel("Waiting",
                          tagList(
                            h3("Number of patients waiting"),
                            p("waiting information goes here")
                          ) # taglist
                 ),
                 tabPanel("Admitted",
                          tagList(
                            h3("Number of patients admitted"),
                            p("admitted information goes here")
                          ) # taglist
                 ),
                 tabPanel("Additions",
                          tagList(
                            h3("Number of additions to list"),
                            p("additions information goes here")
                          ) # taglist
                 )
           ) # tabbox
  ), # fluidRow

  fluidRow(width=12, height="50px", br()),

  fluidRow(width=12,
           shinydashboard::tabBox( width=NULL, type="pills", height="200px", side="right",
              tabPanel("Waiting",
                       tagList(
                         h3("Distribution of waits"),
                         p("waiting information goes here")
                       ) # taglist
              ),
              tabPanel("Admitted",
                       tagList(
                         h3("Distribution of admitted patients"),
                         p("waiting information goes here")
                       ) # taglist
           )
    ) # tabbox
  ), # fluidRow

  fluidRow(width=12, height="50px", br())

) # div

})

output$landing_page_graphs <-  renderUI({

  div(

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12, height="100px",  pickerInput("hbt_filter", "Select Health Board of Treatment ",
                                                   choices = c("a", "b", "c"),
                                                   selected = c("a"),
                                                   multiple = FALSE)),

    fluidRow(
             shinydashboard::tabBox( width=NULL, type="pills", height="500px",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Number of patients waiting"),
                                                plots[["activity_waiting"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Number of patients admitted"),
                                                plots[["activity_admitted"]]
                                              ) # taglist
                                     ),
                                     tabPanel("Additions",
                                              tagList(
                                                h3("Number of additions to list"),
                                                plots[["activity_additions"]]
                                              ) # taglist
                                     )
             ) # tabbox

    ), # fluidRow

    fluidRow(width=12, height="50px", br()),

    fluidRow(width=12,
             shinydashboard::tabBox( width=NULL, type="pills", height="500px", side="right",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Distribution of waits"),
                                                p("waiting information goes here")
                                              ) # taglist
                                     ),
                                     tabPanel("Admitted",
                                              tagList(
                                                h3("Distribution of admitted patients"),
                                                p("waiting information goes here")
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow

    fluidRow(width=12, height="50px", br())

  ) # div


})

