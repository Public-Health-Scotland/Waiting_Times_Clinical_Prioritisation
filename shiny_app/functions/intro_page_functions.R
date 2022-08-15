####################### Intro Page Functions #######################

## Functions from ScotPHO dashboard https://github.com/Public-Health-Scotland/scotpho-profiles-tool/
# Creating big boxes for main tabs in the landing page (see ui for formatting css)
intro_main_box <- function(title_box, button_name, description) {
  div(class="intro-page-box",
      div(title_box, class = "intro-page-box-title"),
      div(description, class = "intro-page-box-description"),
      actionButton(button_name, NULL, class="intro-page-button")
  )
}
