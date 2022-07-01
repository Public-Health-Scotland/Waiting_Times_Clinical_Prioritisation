########################################################################
# Name of file - CP-publication-prep.R
# Data release - Stage of Treatment
# Original Author - Caroline Thomson
# Orginal Date - June 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Data wrangling for CP element of SoT publication
#
# Approximate run time - xx minutes
#########################################################################

#### 1 - Packages and functions ----
#1.1 - Load packages ----
source("packages/packages.R")
source("functions/CP-functions.R")

Sys.umask(002) #Used to ensure directory permissions are correct

#1.2 - Dates ----
min_date <- as.Date("2021-07-30") #Start date of September 2021 - check if this is needed or if the date filter in BOXI worked
max_date <- as.Date("2022-03-31")
max_date2 <- as.Date("2022-06-30") #max date for CP DQ data 

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

#1.4 - Functions (move to functions file) ----
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
    theme(text = element_text(size = 16))+
    geom_blank(aes(y = y_max)) +
    facet_wrap(~ongoing_completed, nrow = 2, scales = "free_y",  strip.position = "top", 
               labeller = as_labeller(c(Ongoing = "Patients waiting", Completed = "Patients seen") )) +
    ylab(NULL) +
    xlab("Month ending") +
    theme(text = element_text(size = 16),
          strip.background = element_blank(),
          strip.text.x = element_text(angle = 0,hjust = 0,size = 16),
          panel.spacing = unit(1, "cm"),
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          legend.position="bottom")
}




#### 2 - Import data ----

#2.1 - Specialty exclusions ----
#Use the latest specialty exclusions list from the publication folder

exclusions_path <- here::here("..", "..", "..", #Takes us back up to SoT folder
                              "Publications",
                              "Inpatient, Day case and Outpatient Stage of Treatment Waiting Times",
                              "Publication R Script", 
                              "Publication", 
                              "Data", 
                              "Spec Exclusions.xlsx")

exclusions <- read.xlsx(exclusions_path, sheet = "IPDC") %>%
  as.list(Specialties)


#2.2 - Performance ----
#Read in the BOXI publication output, reformat dates and select correct specialties

#2.2.1 - Monthly ---- 
perf <- read.xlsx("data/Performance excl. Lothian Dental Monthly.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date2), !specialty %in% exclusions) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_12_weeks = 0)) 
                  
#Create version of data that has proportions per CP code per month
perf_split <- perf %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
         mutate(`proportion_seen/on_list` = 100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T),
                y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits

#Save version for DQ shiny app ----
perf_split_monthly <- perf_split %>%
  select(-c(starts_with("Waited"), y_max)) %>%
  pivot_longer(c(`number_seen/on_list`:`proportion_seen/on_list`), names_to = "Indicator", values_to = "value")

saveRDS(perf_split_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.RDS")
write.xlsx(perf_split_monthly, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_monthly.xlsx")
#2.2.2 - Quarterly ---- 

#Note that this is done by summing up monthly data until the queries have been re-run
perf_qtr <- read.xlsx("data/Performance excl. Lothian Dental.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         #mutate(date = date) %>%
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% 
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12), 
                ongoing_completed == "Completed")) %>% 
  #convert monthly dates to end of quarter dates
 # mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>% 
#  group_by(across(-`number_seen/on_list`:`90th_percentile`)) %>% 
  #get the sum of waits/patients seen for each quarter
#  summarise(`number_seen/on_list` = sum(`number_seen/on_list`)) 
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_12_weeks = 0))

#Create version of data that has proportions per CP code per month
perf_qtr_split <- perf_qtr %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = 100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits

#Save version for DQ shiny app ----
perf_split_qtr <- perf_qtr_split %>%
  select(-c(starts_with("Waited"), y_max)) %>% #Change this line to get new >52 and >104 weeks waits
  pivot_longer(c(`number_seen/on_list`:`proportion_seen/on_list`), names_to = "Indicator", values_to = "value")

saveRDS(perf_split_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_quarterly.RDS")
write.xlsx(perf_split_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/performance_quarterly.xlsx")



#2.3 - Distribution of wait ----
dow_4wk_all <-  read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         weeks = as.factor(ifelse(weeks != "Over 104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))

#dow 4 week bands data for publication, max date set to end of latest quarter
dow_4wk <- dow_4wk_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#dow 4 week bands data for CP DQ shiny, max date set to end of latest available month
dow_4wk2 <- dow_4wk_all %>% 
  filter(between(date, min_date, max_date2), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#quaterly 4 week bands dow data for CP DQ shiny
dow_4wk_qtr <- dow_4wk2 %>% 
  #keep last month of quarter for ongoing waits, all months for completed
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12), 
                ongoing_completed == "Completed")) %>% 
  #convert monthly dates to end of quarter dates
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>% 
  group_by(across(-`number_seen/on_list`)) %>% 
  #get the sum of waits/patients seen for each quarter
  summarise(`number_seen/on_list` = sum(`number_seen/on_list`)) 


dow_large_all <-  read.xlsx("data/Distribution of Waits larger time bands.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         weeks = as.factor(ifelse(weeks != ">104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) #Rename T&O as orthopaedics

#dow large week bands data for publication, max date set to end of latest quarter
dow_large <- dow_large_all %>%
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#dow large week bands data for CP DQ shiny, max date set to the last day of latest month
dow_large2 <- dow_large_all %>%
  filter(between(date, min_date, max_date2), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#quaterly large week bands dow data for CP DQ shiny
dow_large_qtr <- dow_large2 %>% 
  #keep last month of quarter for ongoing waits, all months for completed
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12), 
                ongoing_completed == "Completed")) %>% 
  #convert monthly dates to end of quarter dates
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>% 
  group_by(across(-`number_seen/on_list`)) %>% 
  #get the sum of waits/patients seen for each quarter
  summarise(`number_seen/on_list` = sum(`number_seen/on_list`))   
  
#save rds version of monthly and quarterly dow data for CP DQ shiny
saveRDS(dow_4wk2, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_monthly.RDS")
saveRDS(dow_4wk_qtr, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_quarterly.RDS")
saveRDS(dow_large2, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_large_monthly.RDS")
saveRDS(dow_large_qtr, file="/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_large_quarterly.RDS")

write.xlsx(dow_4wk2, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_monthly.xlsx")
write.xlsx(dow_4wk_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_4wk_quarterly.xlsx")
write.xlsx(dow_large2, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_large_monthly.xlsx")
write.xlsx(dow_large_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/dow_large_quarterly.xlsx")


#2.4 - Additions by HBT ----
addrem <- read.xlsx("data/Removal Reason excl. Lothian Dental.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date2), !specialty %in% exclusions) %>%
  pivot_longer(c(additions_to_list:other_reasons), values_to = "number", names_to = "indicator") %>%
  complete(urgency, date, indicator,
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(number = 0)) %>% 
  group_by(patient_type, nhs_board_of_treatment, specialty, date, indicator) %>%
  mutate(proportion_CP = 100*number/sum(number, na.rm=T)) %>% #Calculate proportion of indicator by CP
  group_by(patient_type, nhs_board_of_treatment, specialty, date) %>%
  mutate(proportion_removals = if_else(!indicator %in% c("additions_to_list", "removals_from_list"), 
                                       100* number/sum(number[!indicator %in% c("additions_to_list", "removals_from_list")]),
                                       0)) #Calculate proportion of total removals by removal reason

#2.5 - Additions by HBR ----
addhbr <- read.xlsx("data/Removal Reason excl. Lothian Dental.xlsx", sheet = "IPDC Additions HBR") %>%
clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
filter(between(date, min_date, max_date2), !specialty %in% exclusions)


#### 3 - Data wrangling ----

#3.1 - Completed and ongoing waits ----

#3.1.1 -Graph of ongoing and completed waits, by month ----

activity_trendplot <- trendbar(perf_split, "All Specialties", "NHS Scotland")

#3.1.2 - Top 6 specialties by waiting/seen ----

#Identify top 6 specialties by number waiting, calculate what proportion of waiting and seen these represent 

specstats <- perf  %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
       summarise(month = sum(`number_seen/on_list`[date == max(date)], na.rm=T),
                 quarter = sum(`number_seen/on_list`[between(date, max(date)-months(2), max(date))], na.rm=T)) %>%
       mutate(`number_seen/on_list` = if_else(ongoing_completed=="Ongoing", month, quarter)) %>%
  ungroup() %>%
  mutate(allspec = `number_seen/on_list`)[specialty=="All Specialties"])#,
              proportion = 100*`number_seen/on_list`/allspec) %>%
       select(-c(month, quarter))

#List of top six specialties
topsix <- specstats %>%
  filter(ongoing_completed=="Ongoing", nhs_board_of_treatment == "NHS Scotland", !specialty=="All Specialties") %>% 
  arrange(desc(proportion)) %>% 
  slice_head(n=6) %>%
  ungroup() %>%
  select(specialty) %>%
  as.list()

#Data for top six specialties by 
specstats %<>% filter(specialty %in% topsix$specialty)

#Graph
topsixplot <- perf_split %>%
  filter(specialty %in% topsix$specialty, nhs_board_of_treatment=="NHS Scotland", date == max(date)) %>% #Amend to include latest quarter for completed waits!
  ggplot(aes(x = specialty, y = `proportion_seen/on_list`, colour = urgency, fill = urgency), group = ongoing_completed) +
  geom_bar(stat="identity") +
  facet_wrap(~ongoing_completed, nrow=2, scales = "free_y") +
  theme_bw()

#3.2 - Distribution of waits ----
#3.2.1 - Barplot of number seen/waiting by 4 week intervals and CP split ----
dow_barplot <- dow_4wk %>%
  filter(nhs_board_of_treatment == "NHS Scotland", specialty == "All Specialties", date == max(dow_4wk$date)) %>%
  group_by(nhs_board_of_treatment,  ongoing_completed, specialty, weeks, date) %>%
  mutate(y_max = roundUpNice(sum(`number_seen/on_list`, na.rm=T))) %>% 
  group_by(nhs_board_of_treatment, ongoing_completed, specialty, date) %>%
  mutate(y_max = max(y_max)) %>%
  ggplot(aes(x = weeks, y = `number_seen/on_list`), group = ongoing_completed) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  theme_bw() +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  theme(text = element_text(size = 16))+
  geom_blank(aes(y = plyr::round_any(y_max,2000,f = ceiling))) +
  facet_wrap(~ongoing_completed, nrow = 2, scales = "free_y",  strip.position = "top") +
  ylab(NULL) +
 xlab("Weeks waiting") +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.placement = "outside",
        striptext.x = element_text(angle = 0,hjust = 0,size = 16),
        panel.grid.minor.x = lement_blank(), 
        panel.r.x = element_blank(),
        panel.spacing = unit(1, "cm"),
        panel.border = element_blank(),
        legend.position="bottom")

#3.2.2 - Barplot showing number waiting by defined wait lengths by HBT ----
dow_hb <- dow_4wk %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, urgency, date) %>%
  summarise(`< 4 weeks` = sum(`number_seen/on_list`[weeks == "000-004 Weeks"], na.rm=T), 
            `4-12 weeks` = sum(`number_seen/on_list`[weeks %in% c("004-008 Weeks","008-012 Weeks")], na.rm=T), 
            `12-24 weeks` = sum(`number_seen/on_list`[weeks %in% c("012-016 Weeks","016-020 Weeks", "020-024 Weeks")], na.rm=T), 
            `24-52 weeks` = sum(`number_seen/on_list`[weeks %in% c("024-028 Weeks","028-032 Weeks", "032-036 Weeks", "036-040 Weeks", "040-044 Weeks", "044-048 Weeks", "048-052 Weeks")], na.rm=T), 
            `52-76 weeks` = sum(`number_seen/on_list`[weeks %in% c("052-056 Weeks","056-060 Weeks", "060-064 Weeks", "064-068 Weeks", "068-072 Weeks", "072-076 Weeks")], na.rm=T),
            `76-104 weeks` = sum(`number_seen/on_list`[weeks %in% c("076-080 Weeks","080-084 Weeks", "084-088 Weeks", "088-092 Weeks", "092-096 Weeks", "096-100 Weeks", "100-104 Weeks")], na.rm=T),
            `> 104 weeks` = sum(`number_seen/on_list`[weeks == "Over 104 Weeks"], na.rm=T))

#Create all CP codes level
dow_hb_allCP <- dow_hb %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  summarise(across(c(`< 4 weeks`:`> 104 weeks`), sum)) %>%
  mutate(urgency = "All CP codes")

#Bind all CP codes onto original data and calculate proportion by indicator
dow_hb %<>% 
  bind_rows(dow_hb_allCP) %>%
  pivot_longer(c(`< 4 weeks`:`> 104 weeks`), names_to = "indicator", values_to = "number") %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, urgency, date) %>% 
  mutate(total = sum(number, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, urgency, indicator, date) %>% 
  mutate(percentage = 100*number/total)

#Plot of proportions by HBT
dow_hbplot <- dow_hb %>%
  mutate(indicator = factor(indicator, 
                            levels = c("< 4 weeks", "4-12 weeks", "12-24 weeks", "24-52 weeks", "52-76 weeks", "76-104 weeks", "> 104 weeks"))#,
        # nhs_board_of_treatment = factor(nhs_board_of_treatment,                                    # Factor levels in decreasing order
                    #                     levels = nhs_board_of_treatment[order(percentage[indicator=="< 4 weeks"], decreasing = TRUE)])
        ) %>%
  filter(specialty=="All Specialties", date == as.Date("2022-03-31"), urgency == "All CP codes", ongoing_completed =="Ongoing", !nhs_board_of_treatment=="NHS Scotland") %>%
  ggplot(aes(x = nhs_board_of_treatment, y = percentage, fill = indicator, colour = indicator)) +
  geom_bar(position = position_fill(reverse = TRUE), stat="identity") +
  theme_bw() +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_colour_discrete_phs(palette = "all") +
  scale_fill_discrete_phs(palette = "all") +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  theme(text = element_text(size = 16),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.position="bottom")

#3.3 - Additions and removals ----

#Trend in additions and removals by CP (barplot) ----
additions_barplot <- addrem %>%
  filter(nhs_board_of_treatment == "NHS Scotland", specialty == "All Specialties", indicator %in% c("additions_to_list", "removals_from_list")) %>%
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number, na.rm=T))) %>% 
  group_by(nhs_board_of_treatment, specialty, indicator) %>%
  mutate(y_max = max(y_max)) %>%
  ggplot(aes(x = floor_date(date, "month"), y = number), group = indicator) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y"),
               breaks = seq(from = floor_date(min(addrem$date), "month"), 
                            to = floor_date(max(addrem$date), "month"), by = "1 months")) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  theme(text = element_text(size = 16))+
  geom_blank(aes(y = y_max)) +
  facet_wrap(~indicator, nrow = 2, scales = "free_y",  strip.position = "top", 
             labeller = as_labeller(c(additions_to_list = "Additions to list", removals_from_list = "Removals from list") )) +
  ylab(NULL) +
  xlab("Month ending") +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 16),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "cm"),
        panel.border = element_blank(),
        legend.position="bottom")
  

#Trend in additions and removals, by CP category (line plot) ----

additions_trendplot <- addrem %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, indicator, date) %>%
  summarise(number = sum(number, na.rm = T),
            y_max = roundUpNice(sum(number, na.rm=T)),
            urgency = "All CP codes") %>% 
  bind_rows(addrem %>% group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
              mutate(y_max = roundUpNice(sum(number, na.rm=T)))) %>% 
  group_by(nhs_board_of_treatment, specialty) %>%
  mutate(y_max = max(y_max)) %>%
  filter(nhs_board_of_treatment == "NHS Scotland", 
         specialty == "All Specialties", 
         indicator %in% c("additions_to_list", "removals_from_list", "attended"),
         urgency == "All CP codes") %>%
  mutate(indicator = fct_recode(factor(indicator, levels = c("additions_to_list", "removals_from_list", "attended")),"Additions" = "additions_to_list", "All removals (including patients seen)" = "removals_from_list", "Seen" = "attended")) %>%
  ggplot(aes(x = floor_date(date, "month"), y = number, colour = indicator)) +
  geom_line(stat="identity") +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y"),
               breaks = seq(from = floor_date(min(addrem$date), "month"), 
                            to = floor_date(max(addrem$date), "month"), by = "1 months")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual("",values = linecolours) +
  theme(text = element_text(size = 16), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        legend.position="bottom",
        panel.border = element_blank())+
  labs(x="Month ending", y = NULL, title =NULL) +
  geom_blank(aes(y = y_max))

####3.4 - Additions by HBR ----

#3.4.1 - Rates per 100k population ----

pop_path <- ("/conf/linkage/output/lookups/Unicode/Populations")

#Get 2021 populations and calculate Scotland total
popproj <- readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
  mutate(board = paste0("NHS ",str_replace(hb2019name, " and ", " & "))) %>% #Reformat names to match other data 
  group_by(board, year) %>%
  summarise(pop = sum(pop, na.rm=T)) %>%
  bind_rows(readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
              group_by(year) %>%
              summarise(pop = sum(pop, na.rm=T)) %>%
              mutate(board = "NHS Scotland")) %>%
  filter(year >= "2021")

#Get population estimates and calculate Scotland total
popest <- readRDS(glue::glue(pop_path, "/Estimates/HB2019_pop_est_1981_2020.rds")) %>% 
  mutate(board = str_replace(hb2019name, " and ", " & ")) %>% #Reformat names to match other data 
  group_by(board, year) %>%
  summarise(pop = sum(pop, na.rm=T)) %>%
  bind_rows(readRDS(glue::glue(pop_path, "/Estimates/HB2019_pop_est_1981_2020.rds")) %>% 
              group_by(year) %>%
              summarise(pop = sum(pop, na.rm=T)) %>%
              mutate(board = "NHS Scotland"))

pop <- bind_rows(popest, popproj) %>%
  mutate(board = toupper(board))


#Calculate crude rates pet month
add_rate <- addhbr %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  filter(!health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  ungroup() %>%
  complete(date, urgency, nesting(patient_type,health_board_of_residence, specialty), fill = list(additions_to_list = 0)) %>%
  mutate(year=year(date)) %>%
  left_join(pop, by = c(health_board_of_residence="board","year")) %>%
  mutate(additions_per_100k = (100000*additions_to_list/pop)) %>%
  select(-year) 

add_rate_scot <- addhbr %>%
  filter(nhs_board_of_treatment =="NHS Scotland", !health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, urgency, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  complete(date, urgency, nesting(patient_type, nhs_board_of_treatment, specialty), fill = list(additions_to_list = 0)) %>%
  mutate(year=year(date), nhs_board_of_treatment = toupper(nhs_board_of_treatment)) %>%
  left_join(pop, by = c("nhs_board_of_treatment" ="board","year")) %>%
  mutate(additions_per_100k = (100000*additions_to_list/pop)) %>%
  select(-year) %>%
  rename(health_board_of_residence = nhs_board_of_treatment)

add_rate %<>% bind_rows(add_rate_scot)

saveRDS(add_rate, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/addition_rate_monthly.RDS")
write.xlsx(add_rate, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/addition_rate_monthly.xlsx")

#Calculate crude rates per quarter
add_rate_qtr <- addhbr %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  filter(!health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  ungroup() %>%
  complete(date, urgency, nesting(patient_type,health_board_of_residence, specialty), fill = list(additions_to_list = 0)) %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  mutate(year=year(date)) %>%
  left_join(pop, by = c(health_board_of_residence="board","year")) %>%
  mutate(additions_per_100k = (100000*additions_to_list/pop)) %>%
  select(-year)


#Scotland level rates
add_rate_qtr_scot <- addhbr %>%
  filter(nhs_board_of_treatment =="NHS Scotland", !health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, urgency, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  complete(date, urgency, nesting(patient_type, nhs_board_of_treatment, specialty), fill = list(additions_to_list = 0)) %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, urgency, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  mutate(year=year(date), nhs_board_of_treatment = toupper(nhs_board_of_treatment)) %>%
  left_join(pop, by = c(nhs_board_of_treatment="board","year")) %>%
  mutate(additions_per_100k = (100000*additions_to_list/pop)) %>%
  select(-year) %>%
  rename(health_board_of_residence = nhs_board_of_treatment)

add_rate_qtr %<>% bind_rows(add_rate_qtr_scot)

saveRDS(add_rate_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/addition_rate_quarterly.RDS")
write.xlsx(add_rate_qtr, file = "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI/CP DQ/shiny/addition_rate_quarterly.xlsx")


#Graph for selected specialty ----

rate_plot <- add_rate %>% 
  filter(specialty == "All Specialties",
         urgency %in% c("P2", "P3", "P4")) %>%
  ggplot(aes(x = date, y = additions_per_100k, colour = health_board_of_residence)) +
  geom_line(stat="identity") +
  facet_wrap(~urgency) +
  theme_bw() +
  scale_x_date()

rate_plot_2 <- add_rate %>% 
  filter(specialty == "All Specialties",
         urgency %in% c("P2", "P3", "P4")) %>%
  ggplot(aes(x = date, y = additions_per_100k, colour = urgency, shape = urgency)) +
  geom_line(stat="identity") +
  geom_point(stat="identity") +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
  facet_wrap(~health_board_of_residence, ncol = 3) +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y")) +
  scale_y_continuous(breaks = breaks_pretty(n = 5)) +
  labs(x = NULL, y = "Crude rate of additions, per 100k population")
  

#cross-border flow
cbf <- addhbr %>% 
  filter(!nhs_board_of_treatment=="NHS Scotland",
         between(date, max(addhbr$date) - months(3), max(addhbr$date)),
         !nhs_board_of_treatment == "NHS Scotland",
         specialty == "All Specialties") %>%
  mutate(health_board_of_residence = if_else(is.na(health_board_of_residence), "NOT KNOWN", health_board_of_residence)) %>%
  group_by(source = health_board_of_residence, target = nhs_board_of_treatment) %>%
  summarise(value = sum(additions_to_list, na.rm = T))

