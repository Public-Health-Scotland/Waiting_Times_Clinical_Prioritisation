####################### Waiting Times Dashboard #######################

# Get packages
source("setup.R")

# UI
ui <- fluidPage(

  tagList(
    useShinyjs(),
    # Specify most recent fontawesome library
    tags$style("@import url(https://use.fontawesome.com/releases/v6.1.1/css/all.css);"),
    navbarPage(
      id = "intabset", # id used for jumping between tabs
      title = div(
        tags$a(img(src = "phs-logo.png", height = 40),
               href = "https://www.publichealthscotland.scot/",
               target = "_blank"), # PHS logo links to PHS website
        style = "position: relative; top: -5px;"),
      windowTitle = "PHS Waiting Times Dashboard",    # Title for browser tab
      header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
                         tags$link(rel = "shortcut icon", href = "favicon_phs.ico"), # Icon for browser tab
                         #TODO: Include Google analytics
                         #includeHTML("www/google-analytics.html"),
                         HTML("<html lang='en'>")
      ),
      ##############################################.
      # OVERVIEW PAGE ----
      ##############################################.
      tabPanel(title = "Overview",
               icon = icon_no_warning_fn("circle-info"),
               value = "overview",
               h1("Waiting times dashboard"),

               tabsetPanel(
                 tabPanel("Info",
                          uiOutput("landing_page_info")
                          ),
                 tabPanel("Graphs",
                          uiOutput("landing_page_graphs")
                          )
               ) # tabsetpanel

               ), # tabpanel
      ##############################################.
      # WAITING LIST ACTIVITY PAGE ----
      ##############################################.
      tabPanel(title = "Waiting list activity",
               icon = icon_no_warning_fn("calendar-check"),
               value = "activity",
               tabsetPanel(
                 tabPanel("Waiting",
                          uiOutput("activity_waiting_ui")
                 ),
                 tabPanel("Admitted",
                          uiOutput("activity_admitted_ui")
                 )
               ) # tabsetpanel
      ), # tabpanel
      ##############################################.
      # DISTRIBUTION OF WAITS PAGE ----
      ##############################################.
      tabPanel(title = "Distribution of waits",
               icon = icon_no_warning_fn("chart-area"),
               value = "distribution",
               tabsetPanel(
                 tabPanel("Waiting",
                          uiOutput("distribution_waits_waiting_ui")
                 ),
                 tabPanel("Admitted",
                          uiOutput("distribution_waits_admitted_ui")
                 )
               ) # tabsetpanel
      ), # tabpanel
      ##############################################.
      # ADDITIONS BY HBR PAGE ----
      ##############################################.
      tabPanel(title = "Additions",
               icon = icon_no_warning_fn("hospital-user"),
               value = "additions",
               uiOutput("additions_ui")
      ) # tabpanel

      ) # navbar
    ) # taglist
) # ui fluidpage

# ----------------------------------------------------------------------
# Server

server <- function(input, output) {

  # Get plot functions
  source(file.path("functions/waiting_list_activity_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/distribution_of_waits_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/additions_plot_functions.R"), local = TRUE)$value

  # Get content for individual pages
  source(file.path("pages/landing_page.R"), local = TRUE)$value
  source(file.path("pages/waiting_list_activity.R"), local = TRUE)$value
  source(file.path("pages/distribution_of_waits.R"), local = TRUE)$value
  source(file.path("pages/additions.R"), local = TRUE)$value



}

# Run the application
shinyApp(ui = ui, server = server)

