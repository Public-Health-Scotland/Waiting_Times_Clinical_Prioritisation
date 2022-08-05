########################################################################
# Name of file - CP-functions.R
# Data release - Stage of Treatment
# Original Authors - Caroline Thomson
# Orginal Date - October 2021
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Functions used in CP analysis code
#
# Approximate run time - NA
#########################################################################

#1 - Function to round values up for creating upper limits for y axes ----
roundUpNice <- function(x, nice_small=c(1,2,3,4,5,6,8,10),
                        nice_mid=c(1,1.5,2,3,4,5,6,8,10), 
                        nice_big=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  if (abs(x) > 1000) {
    nice = nice_big
  } else if (abs(x) < 10){
    nice = nice_small
  } else {
    nice = nice_mid
  }
  
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#2 - Function to highlight particular axis labels (e.g. NHS Scotland) ----
highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}

#Example usage:
# scale_x_discrete(labels= function(x) highlight(x, "NHS Scotland", "black")) + ...

#3 - Function to modify gg_x_repel to be aware of other repel layers ----
# Wrapper of ggrepel::geom_text_repel
geom_text_repel2 <- function(...) {
  layer <- ggrepel::geom_text_repel(...)
  layer$ggrepel <- TRUE
  class(layer) <- c("ggrepel", class(layer))
  return(layer)
}

ggplot_add.ggrepel <- function(object, plot, object_name) {
  if (any(do.call(c, lapply(plot$layer, function(x) x$ggrepel)))) {
    warning(
      "There is more than one ggrepel layers. ",
      "This may cause overlap of labels"
    )
  }
  # Optionally, one may modify `object` here.
  NextMethod("ggplot_add")
}


# 4. Function plots the barchart of ppl waiting and admitted over time for HB and Specialty ----


trendbar <- function(data, spec, hb)
{
  data %>% filter(specialty==spec,
                  nhs_board_of_treatment==hb
  ) %>%
    ggplot(aes(x =floor_date(date, "month"), y = `number_seen/on_list`), group = ongoing_completed) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
    theme_bw() +
    scale_x_date(labels = date_format("%b %y"),
                 breaks = seq(from = floor_date(min(addrem$date), "month"), 
                              to = floor_date(max(addrem$date), "month"), by = "1 months")) +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name ="") +
    geom_blank(aes(y = y_max)) +
    facet_wrap(~ongoing_completed, nrow = 2, scales = "free_y",  strip.position = "top", 
               labeller = as_labeller(c(Ongoing = "Patients waiting", Completed = "Patients seen") )) +
    ylab(NULL) +
    xlab("Month ending") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          panel.spacing = unit(1, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          legend.position="bottom")
}


# 5. Function to highlight particular axis labels (e.g. NHS Scotland)  -----

highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}


# 6. Plot additions, waiting and admitted for topsix specs ----

topsixplot <- function(date_choice, board_choice) {hb_var_plotdata %>%
    filter(date == date_choice, 
           !urgency=="Total",
           nhs_board_of_treatment == board_choice,
           str_detect(specialties, specialty)) %>%
    group_by(nhs_board_of_treatment, date, specialty, indicator) %>%
    ggplot(aes(x = fct_reorder(specialty, p2_proportion, .desc = FALSE), y = proportion), group = urgency) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
    theme_bw() +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    scale_y_continuous(labels=scales::percent) +
    facet_wrap(~indicator, nrow=1, strip.position = "top",
               labeller = as_labeller(c(additions_to_list = "Patients added to the waiting list", Completed = "Patients admitted", Ongoing = "Patients waiting"), default=label_wrap_gen(20))) +
    labs(x = NULL, y = "Percentage of total") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12, vjust = 1),
          #panel.grid.minor.x = element_blank(),
          #panel.grid.major.x = element_blank(),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.margin=margin(0,0,0,0),
          legend.spacing= unit(0.0, "cm"),
          legend.text = element_text(size = 10)) +
    coord_flip()
}



# 7. Plot cp code breakdown by hb -------

hb_var_plot <- function(date_choice) {hb_var_plotdata %>% 
    filter(specialty == "All Specialties",
           date == date_choice,
           !urgency == "Total") %>%
    ggplot(aes(x = fct_reorder(nhs_board_of_treatment, p2_proportion, .desc =FALSE), y = proportion), urgency) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity", width=0.75) +
    #scale_x_reordered() +
    theme_bw() + 
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    scale_x_discrete(labels= function(x) highlight(x, "NHS Scotland", "black")) +
    scale_y_continuous(labels=scales::percent) +
    facet_wrap(~indicator, nrow=1, strip.position = "top",
               labeller = as_labeller(c(additions_to_list = "Patients added to the waiting list", Completed = "Patients admitted", Ongoing = "Patients waiting"), default=label_wrap_gen(20))) +
    labs(x = NULL, y = "Percentage of total") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12, vjust = 0),
          #panel.grid.minor.x = element_blank(), 
          #panel.grid.major.x = element_blank(),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8)) +
    coord_flip() +
    theme(axis.text.y=element_markdown())
}


# 8. Plot number activity for two diff healthboards for chosen specialty ----------------------------------------------------------------------
hb_spec_plot <- function(date_choice, spec_choice, hb_list) {hb_var_plotdata %>% 
    filter(date == date_choice,
           !urgency == "Total",
           specialty == spec_choice,
           nhs_board_of_treatment %in% hb_list) %>%
    rowwise() %>%
    mutate(y_max = roundUpNice(total)) %>%
    ggplot(aes(x = nhs_board_of_treatment, y = number), group=urgency) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity", width=0.9) +
    #scale_x_reordered() +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    theme_bw() + 
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
    geom_blank(aes(y = y_max)) +
    facet_wrap(.~indicator, 
               labeller = as_labeller(c(`additions_to_list` = "Additions to list", Completed = "Patients admitted", Ongoing = "Patients waiting"))) +
    #facet_grid(cols = vars(ongoing_completed), scales = "free_x",drop = TRUE)+
    labs(x = NULL, y = NULL) +
    theme(text = element_text(size = 14),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 14),
          #panel.grid.minor.x = element_blank(), 
          #panel.grid.major.x = element_blank(),
          panel.spacing = unit(0.25, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 10)) 
}


# 9. Plot DoW admitted/waiting  ------
dow_barplot <- function(data_choice, board_choice, specialty_choice, date_choice) {data_choice %>%
    filter(nhs_board_of_treatment == board_choice, 
           specialty == specialty_choice, 
           !urgency == "Total",
           date == date_choice) %>%
    group_by(nhs_board_of_treatment,  ongoing_completed, specialty, weeks, date) %>%
    mutate(y_max = roundUpNice(sum(`number_seen/on_list`, na.rm=T))) %>% 
    group_by(nhs_board_of_treatment, ongoing_completed, specialty, date) %>%
    mutate(y_max = max(y_max)) %>%
    ggplot(aes(x = weeks, y = `number_seen/on_list`), group = ongoing_completed) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
    theme_bw() +
    scale_x_discrete(labels = unique(dow_4wk_plot$weeks2)) +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    geom_blank(aes(y = plyr::round_any(y_max,2000, f = ceiling))) +
    facet_wrap(~ongoing_completed, nrow = 2, scales = "free_y",  strip.position = "top", 
               labeller = as_labeller(c(Completed = "Number of patients admitted", Ongoing = "Number of patients waiting"))) +
    ylab(NULL) +
    xlab("Weeks waiting") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing = unit(1, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8))
}

# 10. Barplot of two contrasting Boards for single specialty -----
hb_dow_bar <- function(df, specialty_choice, date_choice, board_list) {df %>%
    filter(specialty == specialty_choice, 
           date == date_choice, 
           !urgency == "Total",
           nhs_board_of_treatment %in% board_list) %>%
    group_by(nhs_board_of_treatment,  ongoing_completed, specialty, weeks, date) %>%
    mutate(y_max = roundUpNice(sum(`number_seen/on_list`, na.rm=T))) %>%
    group_by(nhs_board_of_treatment, ongoing_completed, specialty, date) %>%
    mutate(y_max = max(y_max),
           ongoing_completed = if_else(ongoing_completed =="Ongoing", "Patients waiting", "Patients admitted")) %>%
    unite("BothLabels", ongoing_completed, nhs_board_of_treatment, sep = " - ", remove = FALSE) %>% #Create labels
    ggplot(aes(x = weeks, y = `number_seen/on_list`, group = BothLabels)) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)),
                 fill=fct_rev(factor(urgency, levels = colourset$codes))),
             stat="identity") +
    geom_blank(aes(y = y_max)) + #add blank geom to extend y axis up to y_max
    theme_bw() +
    scale_x_discrete(labels = unique(dow_4wk_plot$weeks2)) +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    facet_wrap(~BothLabels, nrow = 2,  strip.position = "top") + 
    ylab(NULL) +
    xlab("Weeks waited or waiting") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8))
}


# 11. Barplot of two contrasting specialties ------------------------------

spec_dow_bar <-  function(df, specialty_list, date_choice, board_choice) {df %>%
    filter(specialty %in% specialty_list, 
           date == date_choice, 
           !urgency == "Total",
           nhs_board_of_treatment ==board_choice) %>%
    group_by(nhs_board_of_treatment,  ongoing_completed, specialty, weeks, date) %>%
    mutate(y_max = roundUpNice(sum(`number_seen/on_list`, na.rm=T))) %>%
    group_by(nhs_board_of_treatment, ongoing_completed, date) %>%
    mutate(y_max = max(y_max),
           ongoing_completed = if_else(ongoing_completed =="Ongoing", "Patients waiting", "Patients admitted")) %>%
    unite("BothLabels", ongoing_completed, specialty, sep = " - ", remove = FALSE) %>% #Create labels
    ggplot(aes(x = weeks, y = `number_seen/on_list`, group = BothLabels)) +
    geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)),
                 fill=fct_rev(factor(urgency, levels = colourset$codes))),
             stat="identity") +
    geom_blank(aes(y = plyr::round_any(y_max,1000,f = ceiling))) + #add blank geom to extend y axis up to nearest 1000
    theme_bw() +
    scale_x_discrete(labels = unique(dow_4wk_plot$weeks2)) +
    scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
    facet_wrap(~BothLabels, nrow = 2, scales = "free_y") + #,
    ylab(NULL) +
    xlab("Weeks waited or waiting") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing.x = unit(0.25, "cm"),
          panel.spacing.y = unit(0.5, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8))
}
