####################### Waiting Times Dashboard #######################

# Get packages
source("setup.R")

# UI ----------------------------------------------------------------------
ui <- #secure_app( #uncomment if needing password protection

  # Theme for shinymanager
  #theme = shinythemes::shinytheme("flatly"), #uncomment if needing password protection

  fluidPage(

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
      windowTitle = "PHS Waiting Times and Clinical Prioritisation",    # Title for browser tab
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

               h1("Waiting times and Clinical Prioritisation"),
               h2("Inpatients and day cases covered by the Treatment Time Guarantee (TTG)"),
               linebreaks(1),
               p(strong("An Official Statistics release for Scotland (Experimental)")),
               linebreaks(1),
               uiOutput("intro_page_ui")

      ), # tabpanel
      ##############################################.
      # OVERVIEW PAGE ----
      ##############################################.
      tabPanel(title = "Overview of waiting list activity",
               icon = icon_no_warning_fn("magnifying-glass-chart"),
               value = "overview",

               h2("Overview of waiting list activity by Clinical Prioritisation category"),
               h4("This page presents the trend in TTG patients added to the waiting list, admitted and waiting, for the selected Health Board of Treatment, specialty and time period. The distribution of waits for patients admitted and waiting is shown, with median and 90th percentile statistics for admitted patients."),
               uiOutput("landing_page_ui")

               ), # tabpanel
      ##############################################.
      # SPECIALTY VARIATION PAGE ----
      ##############################################.
      tabPanel(title = "Variation by specialty",
               icon = icon_no_warning_fn("stethoscope"),
               value = "specialties",

               h2("Variation in Clinical Prioritisation of patients by specialty"),
               h4("This page presents the percentage of TTG patients in each clinical prioritisation category by specialty, for the selected Health Board of Treatment, quarter and specialties. Data are shown for patients added to the waiting list, admitted and waiting, as well as distributions of wait. The specialty filter defaults to the top six specialties by volume of patients waiting for the chosen Board and date."),
               uiOutput("specialties_ui")

      ), # tabpanel
      ##############################################.
      # HEALTH BOARD TREATMENT PAGE ----
      ##############################################.
      tabPanel(title = "Variation by Health Board of Treatment",
               icon = icon_no_warning_fn("people-roof"),
               value = "hbt",

               h2("Variation in Clinical Prioritisation of patients by Health Board of Treatment"),
               h4("This page presents the percentage of TTG patients in each clinical prioritisation category by Health Board of Treatment, for the selected specialty, quarter and Health Boards of Treatment. Data are shown for patients added to the waiting list, admitted and waiting, as well as distributions of wait. The Health Board filter defaults to the six Boards with the largest waiting lists."),
               h4("Note that direct comparisons cannot be drawn between Boards of Treatment due to variation in provision of services, complexity of care provided by Boards of varying sizes and the differing health needs across local populations, along with upstream levels of outpatient activity, which result in onward referrals for treatment."),
               uiOutput("hbt_ui")

      ), # tabpanel
      ##############################################.
      # DATA PAGE ----
      ##############################################.
      tabPanel(title = "Download data",
               icon = icon_no_warning_fn("file-arrow-down"),
               value = "data",

               h2("Download data"),
               uiOutput("download_ui")

      ) # tabpanel
      ) # navbar
    ) # taglist
) # ui fluidpage
#) #secureapp

# Server ----------------------------------------------------------------------

credentials <- readRDS("admin/credentials.rds") #read in credentials

server <- function(input, output, session) {

  # Creating list of reactiveValues to store the plots and numbers in so that they can be accessed when needed.
  # Note that these CANNOT be stored as output$ instead, because they are used in a uiOutput in ui.R
  # And it is bad practice to call output$ objects from within another output$. In this case it leads
  # to mysterious rendering errors due to breaking the reactive dependency tree.
  plots <- reactiveValues()
  numbers <- reactiveValues()

  # Shinymanager Auth

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  # Get navigation buttons
  source(file.path("functions/navigation_buttons.R"), local = TRUE)$value

  # Get modal information boxes
  source(file.path("functions/modals.R"), local = TRUE)$value

  # Get intro page funcitons
  source(file.path("functions/intro_page_functions.R"), local = TRUE)$value

  # Get plot & number functions
  source(file.path("functions/landing_page_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/landing_page_number_functions.R"), local = TRUE)$value
  source(file.path("functions/specialties_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/specialties_number_functions.R"), local = TRUE)$value
  source(file.path("functions/hbt_plot_functions.R"), local = TRUE)$value
  source(file.path("functions/hbt_number_functions.R"), local = TRUE)$value
  source(file.path("functions/data_download_functions.R"), local = TRUE)$value

  # Get content for individual pages
  source(file.path("pages/intro_page.R"), local = TRUE)$value
  source(file.path("pages/landing_page.R"), local = TRUE)$value
  source(file.path("pages/specialties.R"), local = TRUE)$value
  source(file.path("pages/hbt.R"), local = TRUE)$value
  source(file.path("pages/data_download.R"), local = TRUE)$value


}

# Run the application
shinyApp(ui = ui, server = server)

