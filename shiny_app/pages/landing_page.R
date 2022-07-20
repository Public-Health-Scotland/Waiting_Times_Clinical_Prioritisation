####################### Landing Page (Overview) #######################

output$landing_page_info <-  renderUI({

div(

  fluidRow(width=12, height="50px", br()),

  fluidRow(
    column(width=6,
           shinydashboard::tabBox( width=NULL, type="pills", height="200px",
                 tabPanel("Waiting",
                          tagList(
                            h3("Number of patients waiting"),
                            p("waiting information goes here")
                          ) # taglist
                 ),
                 tabPanel("Seen",
                          tagList(
                            h3("Number of patients seen"),
                            p("seen information goes here")
                          ) # taglist
                 )
           ) # tabbox
               ), # column
    column(width=6,
           shinydashboard::box(width=NULL, height="200px",
              tagList(
                pickerInput("hbt_filter", "Select HBT ",
                                       choices = c("a", "b", "c"),
                                       selected = c("a"),
                                       multiple = FALSE),
                h3("Additions"),
                p("Additions information goes here")
              ) # taglist
           ) # box
            ) # column

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
              tabPanel("Seen",
                       tagList(
                         h3("Distribution of seen patients"),
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

    fluidRow(
      column(width=6,
             shinydashboard::tabBox( width=NULL, type="pills", height="200px",
                                     tabPanel("Waiting",
                                              tagList(
                                                h3("Number of patients waiting"),
                                                p("waiting information goes here")
                                              ) # taglist
                                     ),
                                     tabPanel("Seen",
                                              tagList(
                                                h3("Number of patients seen"),
                                                p("seen information goes here")
                                              ) # taglist
                                     )
             ) # tabbox
      ), # column
      column(width=6,
             shinydashboard::box(width=NULL, height="200px",
                                 tagList(
                                   pickerInput("hbt_filter", "Select HBT ",
                                               choices = c("a", "b", "c"),
                                               selected = c("a"),
                                               multiple = FALSE),
                                   h3("Additions"),
                                   p("Additions information goes here")
                                 ) # taglist
             ) # box
      ) # column

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
                                     tabPanel("Seen",
                                              tagList(
                                                h3("Distribution of seen patients"),
                                                p("waiting information goes here")
                                              ) # taglist
                                     )
             ) # tabbox
    ), # fluidRow

    fluidRow(width=12, height="50px", br())

  ) # div


})

