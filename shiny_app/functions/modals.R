####################### Modals #######################

## Modal to explain what median is
median_modal <- modalDialog(
  h3("What is the median?"),
  p("This a measure of the typical wait experienced by patients being treated in this Board/specialty who completed their wait in the period. One simple way of explaining this statistic is that approximately  half of patients treated waited less than the figure shown and half experienced a wait greater than this."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)

## Modal to explain what 90th percentile is
p90_modal <- modalDialog(
  h3("What is the 90th percentile?"),
  p("This reflects the maximum wait experienced by 9 out of 10 patients that were treated in this Board/specialty in this period."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)

cp_1_modal <- modalDialog(
  h3("What is the final priority?"),
  p("The data captured and presented reflects the category most recently applied to the patient. If the patient's clinical need is deemed to have increased during a routine re-evaluation then their catgeory may be escalated, for example from category P4 to P3; conversely, if the patient's condition has stabilised, they may have been moved to a lower category. As a result, there may be occasions where patients are recorded as having waited beyond the recommended timescales outlined in the Framework but may have only been assigned to the more urgent category recently."),
  size = "l",
  easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
)




### Modal links
observeEvent(input$btn_modal_median, { showModal(median_modal) })

observeEvent(input$btn_modal_90th, { showModal(p90_modal) })

observeEvent(input$btn_modal_cp1, { showModal(cp_1_modal) })

