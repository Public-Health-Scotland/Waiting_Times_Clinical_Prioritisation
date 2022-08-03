####################### Modals #######################

### Modals

## Modal to explain what median is
median_modal <- modalDialog(
  h5("What is the median?"),
  p("The median is the value separating the higher half from the lower half of
    a data sample, a population, or a probability distribution. For a data set, it may be thought
    of as \"the middle\" value."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)


### Modal links
observeEvent(input$btn_modal_median, { showModal(median_modal) })

