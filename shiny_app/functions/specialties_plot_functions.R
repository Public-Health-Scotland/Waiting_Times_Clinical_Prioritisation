####################### Specialties Plot Functions #######################

## Get top six specialties given quarter and HBT
topsix_specs <- function(qend, hbt){

 specs <- app_data[["topsix_specs_jun"]] %>%
          filter(date==get_short_date(qend),
             nhs_board_of_treatment==hbt) %>%
          select("specialties") %>% .[[1]] %>%
          strsplit(split = '\"') %>%
          unlist()

  specs <- specs[! specs %in% c("c(", ", ", ")")]

  return(specs)

}

## Faceted activity graph

activity_specs <- function(input_data,
                           waiting_status,
                           qend="March 2022",
                           hbt="NHS Scotland",
                           specialties=c("All Specialties")) {

  # Waiting status
  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                waiting_status == "additions" ~ "additions_to_list",
                                TRUE ~ "")


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           indicator == indicator_string,
           !urgency == "Total",
           date == get_short_date(qend),
           specialty %in% input$specialty_filter) %>%
    mutate(
      urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")) )

  # Wrapping text on specialties for plotting
  dataset$specialty_wrapped <- purrr::map_chr(dataset$specialty, wrap_label)

  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                           waiting_status == "admitted" ~ "Patients admitted",
                           waiting_status == "additions" ~ "Additions to list",
                           TRUE ~ "")

  xaxis_plots[["categoryorder"]] <-"trace"
  yaxis_plots[["tickformat"]] <- "%"


  # facets <- unique(dataset$indicator)

  p <- dataset %>%
    plot_ly(x = ~factor(specialty_wrapped),
            y = ~round(proportion,2),
            height = 600,
            type = "bar",
            customdata = ~number,
            text = ~total,
            color = ~urgency,
            colors = waiting_times_palette,
            # stroke = I("black"),
            marker = list(line = list(color = "black", width = 1)),
            legendgroup = ~urgency,
            hovertemplate = paste(
              "<b>Specialty</b>:  %{x}",
              "<b>Number of Patients</b>: %{customdata:,}",
              "<b>Percentage</b>: %{y}",
              "<b>Total</b>: %{text:,}",
              sep = "\n")) %>%
    add_annotations(
      text = ~paste("\n",strwrap(unique(plot_title),25), collapse="\n"),
      x = 0,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "left",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 14, face = "bold")
    )

  p %<>%  layout(margin = list(b = 80, t = 50), #to avoid labels getting cut out
                 yaxis = yaxis_plots, xaxis = xaxis_plots,
                 paper_bgcolor = phs_colours("phs-liberty-10"),
                 plot_bgcolor = phs_colours("phs-liberty-10"),
                 legend = list(x = 100, y = 0.5), #position of legend
                 barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  return(p)

}

# --------------------------------------------------------------------------
## Faceted waits graph

waits_specs <- function(input_data, waiting_status,
                           qend="March 2022",
                           hbt="NHS Scotland",
                        chosen_specialty="All Specialties",
                        legend = FALSE) {

  indicator_string <- case_when(waiting_status == "waiting" ~ "Ongoing",
                                waiting_status == "admitted" ~ "Completed",
                                TRUE ~ "")
  

  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           ongoing_completed == indicator_string,
           !urgency == "Total",
           date == get_short_date(qend),
           specialty == chosen_specialty) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                      ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
           )
  
  
  yaxis_title <- chosen_specialty
  
  yaxis_plots[["title"]] <- yaxis_title
  xaxis_plots[["title"]] <- "Weeks waiting"
  

  tooltip_trend <- glue("Quarter ending: {qend}<br>",
                        "Weeks waiting: {dataset$weeks}<br>",
                        "HBT: {dataset$nhs_board_of_treatment}<br>",
                        "Clinical prioritisation: {dataset$urgency}<br>",
                        "Specialty: {dataset$specialty}<br>",
                        "Number of patients: {format(dataset$`number_seen/on_list`, big.mark=',')}<br>")
  
  
  p <- dataset %>%
    plot_ly(x = ~weeks, height = 1000) %>%
    add_bars(y = ~`number_seen/on_list`,
             color = ~urgency,
             colors = waiting_times_palette,
             text = tooltip_trend,
             stroke = I("black"),
             hoverinfo = "text",
             legendgroup = ~urgency,
             name = ~urgency,
             showlegend = legend) %>%
    #Layout
    layout(#to avoid labels getting cut out
           yaxis = yaxis_plots, xaxis = xaxis_plots,
           paper_bgcolor = phs_colours("phs-liberty-10"),
           plot_bgcolor = phs_colours("phs-liberty-10"),
           legend = list(x = 100, y = 0.5), #position of legend
           barmode = "stack") %>% #split by group
    # leaving only save plot button
    config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove )
  
  return(p)

}

make_dow_suplots <- function(data, specialties = c("All Specialties"), n_specs, 
                             waiting_status, qend, hbt){
  
  # specialties <- input$specialty_filter #list of specs selected
  # 
  # n_specs <- length(input$specialty_filter) #num of specs selected
  
  plot_list <- vector("list", length = n_specs) #initialize empty list to store plots
  
  #create patients waiting DoW plots for each spec
  for(i in seq_along(specialties)){
    
    if(i < n_specs){ 
      spec_plot <- waits_specs(input_data = data,
                               waiting_status = waiting_status,
                               qend=input$quarter_end_spec,
                               hbt=input$hbt_filter_spec,
                               chosen_specialty = specialties[[i]])
    }
    
    else{ #add legend to last plot 
      spec_plot <- waits_specs(input_data = data,
                               waiting_status = waiting_status,
                               qend=input$quarter_end_spec,
                               hbt=input$hbt_filter_spec,
                               chosen_specialty = specialties[[i]],
                               legend = TRUE)
    }
    
    plot_list[[i]] <- spec_plot #save plot
  }
  
  plot_title <- case_when(waiting_status == "waiting" ~ "Patients waiting",
                          waiting_status == "admitted" ~ "Patients admitted",
                          TRUE ~ "")
  
  #create facetted plot by specialty
  subplot(plot_list, nrows=n_specs, shareX = TRUE, titleY = TRUE) %>% 
    layout(title=plot_title, margin = list(b = 10, t = 20))
  
}


###ggplot version
# facets <- unique(dataset$specialty)
# 
# 
# p <- ggplot(dataset, aes(x=weeks, y=`number_seen/on_list`, group=urgency,
#                          text = paste0(
#                            '</br>Weeks waiting: ', weeks,
#                            '</br>Specialty: ', specialty,
#                            '</br>Urgency: ', urgency,
#                            '</br>', seen_or_on_list, ': ', format(`number_seen/on_list`, big.mark=","))
# )) +
#   geom_col(aes(fill = urgency),
#            position = position_stack(reverse = TRUE)) +
#   scale_fill_manual(values = waiting_times_palette) +
#   scale_y_continuous() +
#   xlab("Weeks waiting") +
#   ylab("Number seen / on list") +
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         strip.text.x = element_text(colour = phs_colours("phs-purple"), size=12, angle=0),
#         strip.text.y = element_text(colour = phs_colours("phs-purple"), size=12, angle=360),
#         axis.text.x = element_text(angle=70, size=12),
#         axis.title.x = element_text(margin=margin(t=500)),
#         axis.title.y = element_text(margin=margin(r=500))) +
#   facet_grid(specialty ~ ongoing_completed,  scales="free_y",
#              # This wraps the facet label text to fit it on the plot
#              labeller = label_wrap_gen(width=10))
# 
# 
# plotlyp <- ggplotly(p, height=1200, tooltip=c("text"))%>%
#   #Layout
#   layout(margin = list(l=100, r=150, b=160, t=50, pad=0), #to avoid labels getting cut out
#          yaxis = yaxis_plots, xaxis = xaxis_plots,
#          paper_bgcolor = phs_colours("phs-liberty-10"),
#          plot_bgcolor = phs_colours("phs-liberty-10"),
#          # Explanation of this legend nightmare:
#          # -------------------------------------
#          # 1. orientation h means horizontal i.e. the legend reads left to right
#          # 2. paper means that the reference scale for x/y axes are set to [0,1]
#          #    where 0 is left/bottom and 1 is right/top, respectively
#          #    (numbers outwith this interval are outwith the plot area)
#          # 3. anchors set to center means that the centre of the legend is its
#          #    reference point
#          # 4. x and y are the locations of the centre of the legend on this paper scale
#          #    i.e. legend x is in middle of plot and legend y is 15% of plot's size below
#          #    the plot
#          legend = list(orientation = "h", xref="paper", yref="paper",
#                        xanchor="center", yanchor="center",
#                        x=0.5, y=-0.15), #position of legend
#          barmode = "stack") %>% #split by group
#   # leaving only save plot button
#   config(displaylogo = F, displayModeBar = TRUE, modeBarButtonsToRemove = bttn_remove ) %>%
#   stop_axis_title_overlap()
# 
# return(plotlyp)
# --------------------------------------------------------------------------
## Data tables

spec_activity_table <-  function(input_data,
                                 qend="March 2022",
                                 hbt="NHS Scotland",
                                 specialties=c("All Specialties")) {


  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_short_date(qend),
           specialty %in% input$specialty_filter) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")) ) %>%
    select(date, indicator, nhs_board_of_treatment, specialty, urgency, number)

  names(dataset) <- replace_colnames(names(dataset))


  return(dataset)


}

spec_waits_table <- function(input_data,
                             qend="March 2022",
                             hbt="NHS Scotland",
                             specialties=c("All Specialties")) {



  dataset <- input_data %>%
    filter(nhs_board_of_treatment == hbt,
           date == get_short_date(qend),
           specialty %in% specialties) %>%
    mutate(urgency = factor(urgency, levels=c("P1A-1B", "P2", "P3", "P4", "Other", "Total")),
           weeks = get_pretty_weeks(weeks),
           seen_or_on_list = case_when(ongoing_completed == "Ongoing" ~ "Number on list",
                                       ongoing_completed == "Completed" ~ "Number seen")) %>%
    mutate(weeks = factor(weeks, levels=get_pretty_weeks(unique(input_data$weeks)))
    ) %>%
    select(date, ongoing_completed, nhs_board_of_treatment, specialty, urgency, `number_seen/on_list`)

  names(dataset) <- replace_colnames(names(dataset))

  return(dataset)

}




