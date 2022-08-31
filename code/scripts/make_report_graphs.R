########################################################################
# Name of file - make_report_graphs.R
# Data release - Stage of Treatment
# Original Author - Caroline Thomson
# Orginal Date - June 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Data wrangling for CP element of SoT publication and CP Shiny app
#
# Approximate run time - xx minutes
#########################################################################

#1.3 - Colours ----
colourset = data.frame(codes = c("P1A-1B",
                                 "P2",
                                 "P3",
                                 "P4",
                                 "Other"),
                       colours = c("phs-magenta",
                                   "phs-blue",
                                   "phs-green",
                                   "phs-graphite",
                                   "phs-purple"))


linecolours <- phs_colours(c("phs-green","phs-purple","phs-blue"))
names(linecolours) <- c("Additions", "Seen", "All removals (including patients seen)")


# 1.4 Import Data -----
add_perf <- read.csv(here::here("data", "processed data", "add_perf_mon_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(date = as.Date(date),
         nhs_board_of_treatment = str_replace(nhs_board_of_treatment, "NHS Scotland", "NHSScotland"))

perf_qtr_split <- read.csv(here::here("data", "processed data", "perf_qtr_split_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(date = as.Date(date))

dow_4wk_qtr_pub <- read.csv(here::here("data", "processed data", "dow_4wk_qtr_pub_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(date = as.Date(date))

#addhbr <- read.csv(file = here::here("data", "processed data", "addhbr_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>%
#  mutate(date = as.Date(date))

hb_var_plotdata <- read.csv(here::here("data", "processed data", "hb_plotdata_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% mutate(date = as.Date(date))

topsix <- read.csv(file = here::here("data", "processed data", "topsix_specs_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% mutate(date = as.Date(date))

specstats <- read.csv(file = here::here("data", "processed data", "specstats.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% mutate(date = as.Date(date))

spec_p2_prop <-  read.csv(file = here::here("data", "processed data", "spec_p2_prop.csv"), stringsAsFactors = FALSE, check.names = FALSE)



#### 2 - Data wrangling & Graphs----

#2.1 - Completed and ongoing waits ----

#2.1.1 - Graph of ongoing and completed waits, by month ----
activity_trendplot_jun <- add_perf %>%
  filter(specialty == "All Specialties",
         !urgency=="Total",
         nhs_board_of_treatment == "NHSScotland") %>%
  ggplot(aes(x =floor_date(date, "month"), y = number), group = urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  geom_hline(aes(yintercept=monthly_avg, #Add monthly averages
                 linetype = "2019 monthly average"),
             colour = "#000000") +
  scale_linetype_manual(name ="", values = c('dashed')) +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y"),
               breaks = seq(from = floor_date(min(add_perf$date), "month"),
                            to = floor_date(max(add_perf$date), "month"), by = "1 months")) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name ="") +
  # scale_linetype_manual(name = "2019 average",values = c(1,1)) +
  theme(text = element_text(size = 12))+
  geom_blank(aes(y = y_max)) +
  geom_blank(aes(y = y_max2)) +
  facet_wrap(~indicator, nrow = 3, scales = "free_y",  strip.position = "top",
             labeller = as_labeller(c(additions_to_list ="Number of patients added to the waiting list \n", Ongoing = "Number of patients waiting for treatment \n", Completed = "Number of patients admitted for treatment\n") )) +
  ylab(NULL) +
  xlab("Month ending") +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0, size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.spacing= unit(0.0, "cm"),
        legend.text = element_text(size = 8))


#Save the image
ggsave("allspecs_activity_trend_monthly_jun.png", plot = activity_trendplot_jun, dpi=300, dev='png', height=20, width=18, units="cm", path = here::here("plots", "Snapshot plots", "June 2022"))

#2.1.2 - Top 6 specialties by additions/admitted/waiting ----

#Data for top six specialties
specstats %<>%
  left_join(topsix, by=c("nhs_board_of_treatment", "date")) %>%
  filter(str_detect(specialties, specialty))

#Proportion of total seen/waiting represented by these 6 specialties
topsix_prop <- specstats %>%
  filter(nhs_board_of_treatment == "NHS Scotland") %>%
  group_by(date, indicator) %>%
  summarise(`proportion of total` = sum(proportion))

# #Calculate proportion of additions that are P2 per specialty
# spec_p2_prop <- addrem_qtr  %>%
#   filter(nhs_board_of_treatment == "NHS Scotland",
#          indicator == "additions_to_list",
#          date == max_date) %>%
#   group_by(specialty) %>%
#   mutate(total = sum(number[!urgency=="Total"], na.rm = T),
#          p2_prop = sum(number[urgency == "P2"], na.rm = T)/total) %>%
#   select(indicator, specialty, number, p2_prop)

#Calculate proportion of additions by HB/spec/CP/date
addrem_qtr_split <- hb_var_plotdata %>%
  filter(indicator == "additions_to_list") %>%
  select(-p2_proportion) %>%
  group_by(nhs_board_of_treatment, specialty, indicator,date) %>%
  mutate(total = sum(number[!urgency=="Total"], na.rm = T)) %>%
  ungroup() %>%
  mutate(proportion = number/total)

# addrem_qtr_split <- addrem_qtr %>%
# group_by(nhs_board_of_treatment, specialty, indicator,date) %>%
# mutate(total = sum(number[!urgency=="Total"], na.rm = T)) %>%
# ungroup() %>%
# mutate(proportion = number/total)

#topsix_plot_data <- perf_qtr_split %>%
#  ungroup() %>%
#  select(nhs_board_of_treatment, specialty, indicator = ongoing_completed,
#         urgency, date, number = `number_seen/on_list`, proportion = `proportion_seen/on_list`) %>%
#  mutate(proportion = proportion/100) %>%
#  bind_rows(select(addrem_qtr_split, - total)) %>%
#  filter(str_detect(topsix$specialties[topsix$date == max_date & topsix$nhs_board_of_treatment == "NHS Scotland"], specialty),
#         nhs_board_of_treatment=="NHS Scotland",
#         indicator %in% c("additions_to_list", "Completed", "Ongoing")) %>%
#  ungroup() %>%
#  left_join(select(ungroup(spec_p2_prop), -c(indicator, number)),
#            by = c("specialty")) %>%
#  unique() %>%
#  arrange(indicator,-p2_prop)

#Save March and June graphs
ggsave("top_six_spec_plot_additions_jun.png", plot = topsixplot(max_date, "NHS Scotland"), dpi=300, dev='png', height=10, width=20, units="cm", path = here::here("plots", "Snapshot plots", "June 2022"))

ggsave("top_six_spec_plot_additions_mar.png", plot = topsixplot(max_date2, "NHS Scotland"), dpi=300, dev='png', height=10, width=20, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))

#2.1.3 - HBT variation ----
#Graph
#bar chart qe march, All Speciaties, stacked by urgency code, hbt on x axis, facet additions/seen/waiting

#Save March and June graphs
ggsave("hb_var_plot_jun.png", plot = hb_var_plot(max_date), dpi=300, dev='png', height=24, width=36, units="cm", path = here::here("plots", "Snapshot plots", "June 2022"))

ggsave("hb_var_plot_mar.png", plot = hb_var_plot(max_date2), dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))


#2.1.4 - HBT comparison for a particular specialty ----
#D&G and FV for ophthalmology?

#Save version for QE June
ggsave("hb_comparison_ophthalmology_dg_fv_jun.png", plot = hb_spec_plot(max_date, "Ophthalmology", c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=12, width=26, units="cm", path = here::here("plots", "Snapshot plots", "June 2022"))


#Save version for QE March
ggsave("hb_comparison_ophthalmology_dg_fv_mar.png", plot = hb_spec_plot(max_date2, "Ophthalmology", c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=12, width=26, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))


#2.2 - Distribution of waits ----
#2.2.1 - Barplot of number seen/waiting by 4 week intervals and CP split ----

dow_4wk_plot <- dow_4wk_qtr_pub %>%
  mutate(weeks2 = case_when(weeks == "000-004"  ~"<=4",
                            weeks == "Over 104" ~">104",
                            TRUE ~  gsub("(?<![0-9])0+", "", weeks, perl = TRUE)))

#Save this plot
ggsave("dow Scotland all specs qe mar 2022.png", plot = dow_barplot(dow_4wk_plot,"NHS Scotland", "All Specialties", max_date2), dpi=300, dev='png', height=15, width=18, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))

#2.2.2 - Barplot of two contrasting specialties (Urology and Orthopaedics) ----
#Save plot for QE March 2022
ggsave("dow_ortho_urology_mar2022.png", plot = spec_dow_bar(dow_4wk_plot, c("Urology", "Orthopaedics"), max_date2, "NHS Scotland"), dpi=300, dev='png', height=18, width=20, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))


#2.2.3 - Barplot of two contrasting Boards for single specialty (D&G and FV) ----

#Save June Ophthalmology D&G and FV plot
ggsave("dow_ophthalmology_d&g_fv_jun2022.png", plot = hb_dow_bar(dow_4wk_plot,"Ophthalmology", max_date, c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=15, width=20, units="cm", path = here::here("plots", "Snapshot plots", "June 2022"))


#Save March Ophthalmology D&G and FV plot
ggsave("dow_ophthalmology_d&g_fv_mar2022.png", plot = hb_dow_bar(dow_4wk_plot,"Ophthalmology", max_date2, c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=15, width=20, units="cm", path = here::here("plots", "Snapshot plots", "March 2022"))