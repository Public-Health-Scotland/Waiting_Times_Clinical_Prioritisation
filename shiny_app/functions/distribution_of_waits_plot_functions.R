####################### Distribution of Waits Plot Functions #######################


waits_distribution_plot <- function(input_data, waiting_status, quarter_ending="March 2022",
                                    chosen_specialty="All Specialties",
                                    hbt="NHS Scotland") {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                TRUE ~ "")

  qend <- case_when(quarter_ending == "September 2021" ~ "2021-09-30",
                    quarter_ending == "December 2021" ~ "2021-12-31",
                    quarter_ending == "March 2022" ~ "2022-03-31")

  dataset <- input_data %>%
    filter(ongoing_completed == indicator_string,
           date == qend,
           specialty == chosen_specialty,
           nhs_board_of_treatment == hbt) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  yaxis_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           TRUE ~ "")

  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"

  tooltip_trend <- glue("Quarter ending: {quarter_ending}",
                        "Weeks waiting: {dataset$weeks}<br>",
                        "HBT: {hbt}",
                        "Clinical prioritisation: {dataset$urgency}<br>",
                        "Specialty: {chosen_specialty}",
                        "Number of patients: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>")

  p <- dataset %>%
    plot_ly(x = ~weeks) %>%
    add_bars(y = ~`number_seen/on_list`,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             name = ~urgency) %>%
    #Layout
    layout(margin = list(b = 80, t = 5), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(p)


}