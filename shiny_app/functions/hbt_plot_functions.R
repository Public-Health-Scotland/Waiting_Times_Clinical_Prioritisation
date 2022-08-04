####################### Health Board of Treatment Plot Functions #######################

## Faceted activity graph

activity_specs_hbt <- function(input_data,
                           qend="March 2022",
                           hbts=c("NHS Scotland"),
                           specialty_choice="All Specialties") {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment %in% hbts,
           date == get_short_date(qend),
           specialty == specialty_choice) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  facets <- unique(dataset$indicator)

  # Check that there is enough data to be displayed, otherwise throw error to user
  validate(
    need(length(facets)>=1 , "There are no entries matching your selection. Please choose again.")
  )

  p <- ggplot(dataset, aes(x=nhs_board_of_treatment, y=proportion, group=urgency,
                           text = paste(
                             '</br>HBT: ', nhs_board_of_treatment,
                             '</br>Urgency: ', urgency,
                             '</br>Percentage: ', paste0(round(100*proportion, 2), '%'))
  )) +
    geom_col(aes(fill = urgency),
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = waiting_times_palette) +
    scale_x_discrete(labels = ~ stringr::str_wrap(.x, width = 10)) +
    scale_y_continuous(labels = scales::percent) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(colour = phs_colours("phs-purple")),
          strip.text = element_text(colour = phs_colours("phs-purple"), size=12)) +
    facet_wrap(~indicator, nrow = 3, scales = "free_y",  strip.position = "top",
               labeller = as_labeller(c(additions_to_list ="Additions to list \n",
                                        Ongoing = "Patients waiting \n",
                                        Completed = "Patients admitted \n") ))


  plotlyp <- ggplotly(p, height=600, tooltip=c("text"))%>%
    #Layout
    layout(margin = list(l=100, r=100, b=50, t=50, pad=4), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(plotlyp)


}
#
# # --------------------------------------------------------------------------
# ## Faceted waits graph
#
# waits_specs <- function(input_data,
#                         qend="March 2022",
#                         hbt="NHS Scotland",
#                         specialties=c("All Specialties")) {
#
#
#
#   dataset <- input_data %>%
#     filter(nhs_board_of_treatment == hbt,
#            date == get_short_date(qend),
#            specialty %in% specialties) %>%
#     mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
#            weeks = get_pretty_weeks(weeks),
#            seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
#                                        ongoing_completed == "Completed" ~ "Number seen")) %>%
#     mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
#     )
#
#
#   facets <- unique(dataset$specialty)
#
#
#   p <- ggplot(dataset, aes(x=weeks, y=`number_seen/on_list`, group=urgency,
#                            text = paste0(
#                              '</br>Weeks waiting: ', weeks,
#                              '</br>Specialty: ', specialty,
#                              '</br>Urgency: ', urgency,
#                              '</br>', seen_or_on_list, ': ', format(`number_seen/on_list`, big.mark=","))
#   )) +
#     geom_col(aes(fill = urgency),
#              position = position_stack(reverse = TRUE)) +
#     scale_fill_manual(values = waiting_times_palette) +
#     scale_y_continuous() +
#     xlab("Weeks waiting") +
#     ylab("Number seen / on list") +
#     theme_minimal() +
#     theme(legend.title = element_blank(),
#           strip.text.x = element_text(colour = phs_colours("phs-purple"), size=12, angle=0),
#           strip.text.y = element_text(colour = phs_colours("phs-purple"), size=12, angle=360),
#           axis.text.x = element_text(angle=70, size=12),
#           axis.title.x = element_text(margin=margin(t=500)),
#           axis.title.y = element_text(margin=margin(r=500))) +
#     facet_grid(specialty ~ ongoing_completed,  scales="free_y",
#                # This wraps the facet label text to fit it on the plot
#                labeller = label_wrap_gen(width=10))
#
#
#   plotlyp <- ggplotly(p, height=1200, tooltip=c("text"))%>%
#     #Layout
#     layout(margin = list(l=100, r=150, b=160, t=50, pad=0), #to avoid labels getting cut out
#            yaxis = yaxis_plots, xaxis = xaxis_plots,
#            paper_bgcolor = phs_colours("phs-liberty-10"),
#            plot_bgcolor = phs_colours("phs-liberty-10"),
#            # Explanation of this legend nightmare:
#            # -------------------------------------
#            # 1. orientation h means horizontal i.e. the legend reads left to right
#            # 2. paper means that the reference scale for x/y axes are set to [0,1]
#            #    where 0 is left/bottom and 1 is right/top, respectively
#            #    (numbers outwith this interval are outwith the plot area)
#            # 3. anchors set to center means that the centre of the legend is its
#            #    reference point
#            # 4. x and y are the locations of the centre of the legend on this paper scale
#            #    i.e. legend x is in middle of plot and legend y is 15% of plot's size below
#            #    the plot
#            legend = list(orientation = "h", xref="paper", yref="paper",
#                          xanchor="center", yanchor="center",
#                          x=0.5, y=-0.15), #position of legend
#            barmode = "stack") %>% #split by group
#     # leaving only save plot button
#     config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) %>%
#     stop_axis_title_overlap()
#
#   return(plotlyp)
#
#
# }
#
# # --------------------------------------------------------------------------
# ## Data tables
#
# spec_activity_table <-  function(input_data,
#                                  qend="March 2022",
#                                  hbt="NHS Scotland",
#                                  specialties=c("All Specialties")) {
#
#
#   dataset <- input_data %>%
#     filter(nhs_board_of_treatment == hbt,
#            date == get_short_date(qend),
#            specialty %in% specialties) %>%
#     mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) ) %>%
#     select(date, indicator, nhs_board_of_treatment, specialty, urgency, number) %>%
#     dplyr::rename("Quarter ending" = "date",
#                   "Waiting status" = "indicator",
#                   "Health Board of Treatment" = "nhs_board_of_treatment",
#                   "Specialty" = "specialty",
#                   "Clinical Prioritisation" = "urgency",
#                   "Count" = "number")
#
#
#   return(dataset)
#
#
# }
#
# spec_waits_table <- function(input_data,
#                              qend="March 2022",
#                              hbt="NHS Scotland",
#                              specialties=c("All Specialties")) {
#
#
#
#   dataset <- input_data %>%
#     filter(nhs_board_of_treatment == hbt,
#            date == get_short_date(qend),
#            specialty %in% specialties) %>%
#     mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
#            weeks = get_pretty_weeks(weeks),
#            seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
#                                        ongoing_completed == "Completed" ~ "Number seen")) %>%
#     mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
#     ) %>%
#     select(date, ongoing_completed, nhs_board_of_treatment, specialty, urgency, `number_seen/on_list`) %>%
#     dplyr::rename("Quarter ending" = "date",
#                   "Waiting status" = "ongoing_completed",
#                   "Health Board of Treatment" = "nhs_board_of_treatment",
#                   "Specialty" = "specialty",
#                   "Clinical Prioritisation" = "urgency",
#                   "Count" = "number_seen/on_list")
#
#   return(dataset)
#
# }
#
#
#
#
