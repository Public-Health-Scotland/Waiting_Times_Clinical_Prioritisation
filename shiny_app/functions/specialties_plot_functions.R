####################### Specialties Plot Functions #######################

## Get top six specialties given quarter and HBT
topsix_specs <- function(qend, hbt){

 specs <- app_data[["topsix_specs_mar"]] %>%
          filter(date==get_quarter_date(qend),
             nhs_board_of_treatment==hbt) %>%
          select("specialties") %>% .[[1]] %>%
          strsplit(split = '\"') %>%
          unlist()

  specs <- specs[! specs %in% c("c(", ", ", ")")]

  return(specs)

}

## Faceted activity graph

activity_specs <- function(input_data,
                           qend="March 2022",
                           hbt="NHS Scotland",
                           specialties=c("All Specialties")) {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_quarter_date(qend),
           specialty %in% specialties) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  facets <- unique(dataset$indicator)

  p <- ggplot(dataset, aes(x=specialty, y=proportion, group=urgency,
                           text = paste(
                             '</br>Specialty: ', specialty,
                             '</br>Urgency: ', urgency,
                             '</br>Percentage: ', paste0(round(100*proportion, 2), '%'))
                           )) +
    geom_col(aes(fill = urgency),
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = waiting_times_palette) +
    scale_y_continuous(labels = scales::percent) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.text = element_text(colour = phs_colours("phs-purple"), size=12)) +
    facet_wrap(~indicator, nrow = 3, scales = "free_y",  strip.position = "top",
               labeller = as_labeller(c(additions_to_list ="Additions to list \n",
                                        Ongoing = "Patients waiting \n",
                                        Completed = "Patients admitted \n") ))


  plotlyp <- ggplotly(p, height=600, tooltip=c("text"))%>%
    #Layout
    layout(margin = list(l=100, r=100, b=50, t=50, pad=4), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(plotlyp)


}

# --------------------------------------------------------------------------
## Faceted waits graph

waits_specs <- function(input_data,
                           qend="March 2022",
                           hbt="NHS Scotland",
                           specialties=c("All Specialties")) {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_quarter_date(qend),
           specialty %in% specialties) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )


  facets <- unique(dataset$specialty)

  p <- ggplot(dataset, aes(x=weeks, y=`number_seen/on_list`, group=urgency,
                           text = paste(
                             '</br>Weeks waiting: ', weeks,
                             '</br>Specialty: ', specialty,
                             '</br>Urgency: ', urgency,
                             '</br>Number seen/on list: ', format(`number_seen/on_list`, big.mark=","))
  )) +
    geom_col(aes(fill = urgency),
             position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = waiting_times_palette) +
    scale_y_continuous() +
    xlab("Weeks waiting") +
    ylab("Number seen / on list") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.text = element_text(colour = phs_colours("phs-purple"), size=12, angle=0),
          axis.text.x = element_text(angle=70, vjust=0.5, size=12)) +
    facet_grid(specialty ~ ongoing_completed,  scales="free_y", labeller = label_wrap_gen())


  plotlyp <- ggplotly(p, height=1000, tooltip=c("text"))%>%
    #Layout
    layout(margin = list(l=100, r=100, b=50, t=50, pad=8), #to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = "#F0EFF3",
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )

  return(plotlyp)


}



