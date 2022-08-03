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
      # INTRO PAGE ----
      ##############################################.
      tabPanel(title = "Introduction",
               icon = icon_no_warning_fn("circle-info"),
               value = "intro",

               h1("Welcome to the dashboard"),
               p("Some caveats"),
               br(),
               p("Navigation boxes to take you to the differnet tabs")

      ), # tabpanel
      ##############################################.
      # OVERVIEW PAGE ----
      ##############################################.
      tabPanel(title = "Overview",
               icon = icon_no_warning_fn("magnifying-glass-chart"),
               value = "overview",

               h1("Overview"),
               uiOutput("landing_page_ui")

               ), # tabpanel
      ##############################################.
      # SPECIALTY VARIATION PAGE ----
      ##############################################.
      tabPanel(title = "Specialties",
               icon = icon_no_warning_fn("stethoscope"),
               value = "specialties",

               h2("Specialties"),
               uiOutput("specialties_ui")

      ), # tabpanel
      ##############################################.
      # HEALTH BOARD TREATMENT PAGE ----
      ##############################################.
      tabPanel(title = "Health Board of Treatment",
               icon = icon_no_warning_fn("people-roof"),
               value = "hbt",

               h2("Health Board of Treatment")

      ), # tabpanel
      ##############################################.
      # DATA PAGE ----
      ##############################################.
      tabPanel(title = "Download data",
               icon = icon_no_warning_fn("file-arrow-down"),
               value = "data",

               h2("Download data")

      ), # tabpanel
      ##############################################.
      # NOTES PAGE ----
      ##############################################.
      tabPanel(title = "Notes",
               icon = icon_no_warning_fn("file-lines"),
               value = "notes",

               h2("Notes"),
               uiOutput("notes_ui")

      ) # tabpanel
      ) # navbar
    ) # taglist
) # ui fluidpage

# ----------------------------------------------------------------------
# Server

server <- function(input, output, session) {

  # Get navigation buttons
  source(file.path("functions/navigation_buttons.R"), local = TRUE)$value

  # Get modal information boxes
  source(file.path("functions/modals.R"), local = TRUE)$value

  # Get plot & number functions
  source(file.path("functions/landing_page_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/landing_page_number_functions.R"), local = TRUE)$value
  source(file.path("functions/specialties_plot_functions.R"), local = TRUE)$value

  # Get content for individual pages
  source(file.path("pages/landing_page.R"), local = TRUE)$value
  source(file.path("pages/specialties.R"), local = TRUE)$value
  source(file.path("pages/notes.R"), local = TRUE)$value


}

# Run the application
shinyApp(ui = ui, server = server)

