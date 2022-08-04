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
max_date <- as.Date("2022-06-30")
max_date2 <- as.Date("2022-03-31")

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


#### 2 - Import data ----

#2.1 - Specialty exclusions ----
#Use the latest specialty exclusions list from the publication folder

exclusions_path <- here::here("data", "Spec Exclusions.xlsx")

exclusions <- read.xlsx(test_path, sheet = "IPDC", na.strings = "") %>%
  as.list(Specialties)



#2.2 - Performance ----
#Read in the BOXI publication output, reformat dates and select correct specialties

#Read in publication performance data to get 2019 all specs averages
perf_2019 <- read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/PerformanceIPDC.csv", check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(year(Date) =="2019",
         Specialty == "All Specialties") %>%
  group_by(`NHS Board of Treatment`, `Ongoing/Completed`, Specialty) %>%
  summarise(monthly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed", sum(`Number Seen/On list`, na.rm=T)/12, mean(`Number Seen/On list`, na.rm=T)),0),0),
            quarterly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed", mean(`Number Seen/On list`, na.rm=T), mean(`Number Seen/On list`, na.rm=T)),0),0))%>%
  rename(Indicator = `Ongoing/Completed`) %>%
  unique()

#Read in publication performance data to get 2019 individual specs averages
perf_2019_specs <- read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/PerformanceIPDC.csv", check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), 
         `Number Seen/On list` = as.numeric(`Number Seen/On list`)) %>%
  filter(year(Date) =="2019",
         !Specialty %in% exclusions$Specialties) %>%
  ungroup() %>%
  group_by(`NHS Board of Treatment`,`Ongoing/Completed`, Specialty) %>%
  summarise(monthly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed", 
                                                       sum(`Number Seen/On list`, na.rm=T)/12, 
                                                       mean(`Number Seen/On list`, na.rm=T)),0),0),
            quarterly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed", 
                                                         mean(`Number Seen/On list`, na.rm=T), 
                                                         mean(`Number Seen/On list`, na.rm=T)),0),0)) %>%
  rename(Indicator = `Ongoing/Completed`) %>%
  unique()
  

#2.2.1 - Monthly ---- 
  
#monthly ipdc wt data
perf_all <- read.xlsx(here::here("data", "snapshot", "Performance excl. Lothian Dental Monthly.xlsx"),
                      sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))  #Rename T&O as orthopaedics


#monthly data for report, July 2021 to latest complete quarter
perf <- perf_all %>%
  filter(!specialty %in% exclusions$Specialties) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_26_weeks = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#Create version of data that has proportions per CP code per month
perf_split <- perf %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T)) %>% #Need to exclude the Total group to avoid double-counting
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits

#Create version for Excel with monthly/qtrly averages
perf_avg <- perf_split %>% 
  left_join(perf_2019_specs, 
            by = c("nhs_board_of_treatment" = "NHS Board of Treatment", "ongoing_completed" = "Indicator", "specialty" = "Specialty")) %>%
  mutate(across(c(monthly_avg, quarterly_avg), ~ replace_na(.x,0)))

#Completeness per month
perf_comp <- perf %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  summarise(completeness = round(100*sum(`number_seen/on_list`[!urgency %in% c("Other", "Total")], na.rm=T)/sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T),2)) %>%
  filter(specialty == "All Specialties") %>%
  rename(indicator = ongoing_completed)


#2.2.2 - Quarterly ---- 
perf_qtr_all <- read.xlsx(here::here("data", "snapshot", "Performance excl. Lothian Dental Quarterly.xlsx"), 
                          sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) 

#data for report up to latest complete quarter
perf_qtr <- perf_qtr_all %>% 
  filter(!specialty %in% exclusions$Specialties) %>%
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12),
                ongoing_completed == "Completed")) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_26_weeks = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#Create version of data that has proportions per CP code per month
perf_qtr_split <- perf_qtr %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits


#2.3 - Distribution of wait ----
#dow 4 week bands data for publication, max date set to end of latest quarter

dow_4wk <- read.xlsx(here::here("data", "snapshot", "Distribution of Waits 4 week bands.xlsx"), sheet = "IPDC Clinical Prioritisation", detectDates = FALSE) %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date = openxlsx::convertToDate(date),
         weeks = as.factor(ifelse(weeks != "Over 104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>%
  complete(urgency, date, ongoing_completed, weeks,
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#quarterly 4 week bands dow data for publication
dow_4wk_qtr_pub <- dow_4wk %>%
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12),
                ongoing_completed == "Completed"),
         !specialty %in% exclusions$Specialties) %>%
  #convert monthly dates to end of quarter dates
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  group_by(across(-`number_seen/on_list`)) %>%
  #get the sum of waits/patients seen for each quarter
  summarise(`number_seen/on_list` = sum(`number_seen/on_list`))

#dow large week bands data for publication, max date set to end of latest quarter
dow_large <-  read.xlsx(here::here("data", "snapshot", "Distribution of Waits larger time bands.xlsx"), sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         weeks = as.factor(ifelse(weeks != ">104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics%>%
  filter(!specialty %in% exclusions$Specialties) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#2.4 - Additions by HBT ----
addrem <- read.xlsx(here::here("data","snapshot", "Removal Reason excl. Lothian Dental.xlsx"), 
                    sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(!specialty %in% exclusions$Specialties) %>%
  pivot_longer(c(additions_to_list:other_reasons), values_to = "number", names_to = "indicator") %>%
  complete(urgency, date, indicator,
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(number = 0)) %>% 
  group_by(patient_type, nhs_board_of_treatment, specialty, date, indicator) %>%
  mutate(proportion_CP = ifelse(urgency=="Total", NA, 100*number/sum(number[!urgency=="Total"], na.rm=T)),
         completeness = 100*sum(number[!urgency %in% c("Other","Total")], na.rm=T)/sum(number[!urgency=="Total"], na.rm=T)) #Calculate proportion of indicator by CP
  

#Additions only for Excel MI
add_mon <- addrem %>% 
  filter(indicator == "additions_to_list") 

#Additions completeness
add_comp <- addrem %>%
  filter(specialty == "All Specialties",
         indicator == "additions_to_list") %>%
  select(patient_type, indicator, nhs_board_of_treatment, specialty, date, completeness) %>%
  summarise(completeness = unique(completeness)) #Only take one row per group

#Completeness for all metrics
total_comp <- bind_rows(add_comp, perf_comp) %>%
  select(patient_type,indicator,nhs_board_of_treatment, specialty, date, completeness)

#Quarterly additions ----
addrem_qtr <- addrem %>%
  group_by(patient_type, nhs_board_of_treatment, indicator,  specialty, urgency, date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(number = sum(number[!urgency=="Total"], na.rm = T))


#2.4.1 - long-term additions to get 2019 average ----
#All Specialties only version
add_2019 <-read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/Removal ReasonsIPDC.csv", stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(year(Date) =="2019",
         Specialty == "All Specialties") %>%
  group_by(`NHS Board of Treatment`, Specialty) %>%
  summarise(Indicator = "additions_to_list",
            monthly_avg =  replace_na(round_half_up(sum(`Additions to list`, na.rm=T)/12,0),0),
            quarterly_avg = replace_na(round_half_up(mean(`Additions to list`, na.rm = T),0),0))

#Specialty level version
add_2019_specs <-read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/Removal ReasonsIPDC.csv", stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(year(Date) =="2019") %>%
  group_by(`NHS Board of Treatment`, Specialty) %>%
  summarise(Indicator = "additions_to_list",
            monthly_avg =  replace_na(round_half_up(sum(`Additions to list`, na.rm=T)/12,0),0),
            quarterly_avg = replace_na(round_half_up(mean(`Additions to list`, na.rm = T),0),0))

#Additions only for Excel MI
add_mon <- addrem %>% 
  filter(indicator == "additions_to_list") %>%
  left_join(select(add_2019_specs, -quarterly_avg), 
            by = c("nhs_board_of_treatment" = "NHS Board of Treatment","specialty" = "Specialty", "indicator"="Indicator"))


#2.4.2 - Create 2019 average combined lookup and combined data for additions, completed, waiting ----

avg_2019 <- bind_rows(add_2019, perf_2019)

add_perf_monthly  <- perf_split %>% #First modify perf_split
  rename(indicator = ongoing_completed,
         number = `number_seen/on_list`) %>%
  select(-c(waited_waiting_over_26_weeks:y_max)) %>%
  bind_rows(select(addrem %>% select(-completeness) %>% filter(indicator == "additions_to_list"),-c(starts_with("proportion")))) %>% #Then bind onto filtered additions
  filter(specialty=="All Specialties") %>%
  left_join(select(avg_2019, -quarterly_avg), 
            by=c("nhs_board_of_treatment" = "NHS Board of Treatment", "indicator" = "Indicator", "specialty" = "Specialty")) %>% #Then bind on monthly averages from 2019
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number[!urgency=="Total"], na.rm=T)), #Calculate max from current data per group
         y_max2 = roundUpNice(max(monthly_avg))) #Calculate max from 2019 data per group

add_perf_quarterly  <- perf_qtr_split %>% #First modify perf_split
  rename(indicator = ongoing_completed,
         number = `number_seen/on_list`) %>%
  select(-c(waited_waiting_over_26_weeks:y_max)) %>%
  bind_rows(select(addrem_qtr %>% 
                     filter(indicator == "additions_to_list"),-c(starts_with("proportion")))) %>% #Then bind onto filtered additions
  filter(specialty=="All Specialties") %>%
  left_join(select(avg_2019, -monthly_avg), by=c("nhs_board_of_treatment" = "NHS Board of Treatment", "indicator" = "Indicator", "specialty" = "Specialty")) %>% #Then bind on monthly averages from 2019
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number[!urgency=="Total"], na.rm=T)), #Calculate max from current data per group
         y_max2 = roundUpNice(max(quarterly_avg))) #Calculate max from 2019 data per group


#2.6 - Save data for Excel and app ----

#place data in a list
#write function that saves out two versions of the data and appends the month to the name

#1A - add_perf_monthly
write.csv(add_perf_monthly %>% filter(date <= max_date), file = here::here("data", "processed data", "add_perf_mon_jun.csv"), row.names = FALSE)
write.csv(add_perf_monthly %>% filter(date <= max_date2), file = here::here("data", "processed data", "add_perf_mon_mar.csv"), row.names = FALSE)

#1B - add_perf_quarterly
write.csv(add_perf_quarterly %>% filter(date <= max_date), file = here::here("data", "processed data", "add_perf_qtr_jun.csv"), row.names = FALSE)
write.csv(add_perf_quarterly %>% filter(date <= max_date2), file = here::here("data", "processed data", "add_perf_qtr_mar.csv"), row.names = FALSE)


#2 - add_mon (for Excel)
write.csv(add_mon %>% filter(date <= max_date), file = here::here("data", "processed data", "add_mon_jun.csv"), row.names = FALSE)
write.csv(add_mon %>% filter(date <= max_date2), file = here::here("data", "processed data", "add_mon_mar.csv"), row.names = FALSE)


#3A - perf_qtr_split
write.csv(perf_qtr_split %>% filter(date <= max_date2), file = here::here("data", "processed data", "perf_qtr_split_mar.csv"), row.names = FALSE)
write.csv(perf_qtr_split %>% filter(date <= max_date), file = here::here("data", "processed data", "perf_qtr_split_jun.csv"), row.names = FALSE)

#3B - perf_split
write.csv(perf_split %>% filter(date <= max_date2), file = here::here("data", "processed data", "perf_mon_split_mar.csv"), row.names = FALSE)
write.csv(perf_split %>% filter(date <= max_date), file = here::here("data", "processed data", "perf_mon_split_jun.csv"), row.names = FALSE)

#4 - perf_avg
write.csv(perf_avg %>% filter(date <= max_date2), file = here::here("data", "processed data", "perf_avg_mar.csv"), row.names = FALSE)
write.csv(perf_avg %>% filter(date <= max_date), file = here::here("data", "processed data", "perf_avg_jun.csv"), row.names = FALSE)

#5 - total_comp ("All Specialties" completeness for Excel MI)
write.csv(total_comp %>% filter(date <= max_date), file = here::here("data", "processed data", "total_comp_jun.csv"), row.names = FALSE)
write.csv(total_comp %>% filter(date <= max_date2), file = here::here("data", "processed data", "total_comp_mar.csv"), row.names = FALSE)

#6 - dow_4wk_qtr_pub
write.csv(dow_4wk_qtr_pub %>% filter(date <= max_date), file = here::here("data", "processed data", "dow_4wk_qtr_pub_jun.csv"), row.names = FALSE)
write.csv(dow_4wk_qtr_pub %>% filter(date <= max_date2), file = here::here("data", "processed data", "dow_4wk_qtr_pub_mar.csv"), row.names = FALSE)

#6B - dow_4wk_mon
write.csv(dow_4wk %>% filter(date <= max_date), file = here::here("data", "processed data", "dow_4wk_mon_jun.csv"), row.names = FALSE)
write.csv(dow_4wk %>% filter(date <= max_date2), file = here::here("data", "processed data", "dow_4wk_mon_mar.csv"), row.names = FALSE)

##10 - hb_plotdata (run line 672 onwards first!)
write.csv(hb_var_plotdata %>% filter(date <= max_date), file = here::here("data", "processed data", "hb_plotdata_jun.csv"), row.names = FALSE)
write.csv(hb_var_plotdata %>% filter(date <= max_date2), file = here::here("data", "processed data", "hb_plotdata_mar.csv"), row.names = FALSE)

##11 - topsix_specs (run line 546 onwards first!)
write.csv(topsix %>% filter(date <= max_date), file = here::here("data", "processed data", "topsix_specs_jun.csv"), row.names = FALSE)
write.csv(topsix %>% filter(date <= max_date2), file = here::here("data", "processed data", "topsix_specs_mar.csv"), row.names = FALSE)

#### 3 - Data wrangling ----

add_perf <- read.csv(here::here("data", "processed data", "add_perf_mon_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% 
  mutate(date = as.Date(date))

perf_qtr_split <- read.csv(here::here("data", "processed data", "perf_qtr_split_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% 
  mutate(date = as.Date(date))

dow_4wk_qtr_pub <- read.csv(here::here("data", "processed data", "dow_4wk_qtr_pub_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% 
  mutate(date = as.Date(date))

addhbr <- read.csv(file = here::here("data", "processed data", "addhbr_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% 
  mutate(date = as.Date(date))

hb_var_plotdata <- read.csv(here::here("data", "processed data", "hb_plotdata_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% mutate(date = as.Date(date))

topsix <- read.csv(file = here::here("data", "processed data", "topsix_specs_jun.csv"), stringsAsFactors = FALSE, check.names = FALSE) %>% mutate(date = as.Date(date))


#3.1 - Completed and ongoing waits ----

#3.1.1 - Graph of ongoing and completed waits, by month ----
activity_trendplot_jun <- add_perf_monthly %>% 
  filter(specialty == "All Specialties", 
         !urgency=="Total",
         nhs_board_of_treatment == "NHS Scotland") %>%
  ggplot(aes(x =floor_date(date, "month"), y = number), group = urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  geom_hline(aes(yintercept=monthly_avg, #Add monthly averages
                 linetype = "2019 monthly average"), 
             colour = "#000000") +
  scale_linetype_manual(name ="", values = c('dashed')) +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y"),
               breaks = seq(from = floor_date(min(add_perf_monthly$date), "month"), 
                            to = floor_date(max(add_perf_monthly$date), "month"), by = "1 months")) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name ="") +
  theme(text = element_text(size = 12))+
  geom_blank(aes(y = y_max)) +
  geom_blank(aes(y = y_max2)) +
  facet_wrap(~indicator, nrow = 3, scales = "free_y",  strip.position = "top", 
             labeller = as_labeller(c(additions_to_list ="Additions to list \n", Ongoing = "Patients waiting \n", Completed = "Patients admitted \n") )) +
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
ggsave("allspecs_activity_trend_monthly_jun.png", plot = activity_trendplot_jun, dpi=300, dev='png', height=20, width=18, units="cm", path = here::here("..","R plots", "Snapshot plots", "June 2022"))

#3.1.2 - Top 6 specialties by additions/admitted/waiting ----

#Identify top 6 specialties by number waiting, calculate what proportion of waiting and seen these represent 

#Calculate the proportion of additions represented by each specialty, per HB 
add_stats <- addrem_qtr %>%
  filter(indicator == "additions_to_list") %>%
  group_by(date, indicator, nhs_board_of_treatment) %>%
  mutate(allspec = sum(number[specialty=="All Specialties" & !urgency=="Total"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date, indicator, nhs_board_of_treatment, specialty) %>% 
  summarise(number = sum(number[!urgency=="Total"], na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = 100*number/allspec) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique()

#Calculate the proportion of admissions and ongoing waits represented by each specialty per HB, bind onto additions 
specstats <- perf_qtr_split  %>% 
  group_by(date, patient_type, ongoing_completed, nhs_board_of_treatment) %>%
  mutate(allspec = sum(`number_seen/on_list`[specialty=="All Specialties" & !urgency=="Total"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date,patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>% 
  summarise(`total_seen/on_list` = sum(`number_seen/on_list`[!urgency=="Total"], na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = 100*`total_seen/on_list`/allspec) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique() %>%
  rename(indicator = ongoing_completed,
         number = `total_seen/on_list`) %>%
  ungroup() %>%
  select(-`patient_type`) %>%
  bind_rows(add_stats)

topsix <- specstats %>%
  filter(indicator=="Ongoing", !specialty=="All Specialties") %>%
  arrange(desc(proportion)) %>%
  group_by(nhs_board_of_treatment, date) %>%
  slice(1:6) %>%
  group_by(date, nhs_board_of_treatment) %>%
  summarise(specialties = as.character(list(unique(specialty))))

#Data for top six specialties
specstats %<>% 
  left_join(topsix, by=c("nhs_board_of_treatment", "date")) %>% 
  filter(str_detect(specialties, specialty))

#Proportion of total seen/waiting represented by these 6 specialties
topsix_prop <- specstats %>%
  filter(nhs_board_of_treatment == "NHS Scotland") %>%
  group_by(date, indicator) %>%
  summarise(`proportion of total` = sum(proportion))

#Calculate proportion of additions that are P2 per specialty
spec_p2_prop <- addrem_qtr  %>%
  filter(nhs_board_of_treatment == "NHS Scotland",
         indicator == "additions_to_list",
         date == max_date) %>%
  group_by(specialty) %>%
  mutate(total = sum(number[!urgency=="Total"], na.rm = T),
         p2_prop = sum(number[urgency == "P2"], na.rm = T)/total) %>%
  select(indicator, specialty, number, p2_prop) 

#Calculate proportion of additions by HB/spec/CP/date
addrem_qtr_split <- addrem_qtr %>%
  group_by(nhs_board_of_treatment, specialty, indicator,date) %>%
  mutate(total = sum(number[!urgency=="Total"], na.rm = T)) %>%
  ungroup() %>%
  mutate(proportion = number/total)

topsix_plot_data <- perf_qtr_split %>%
  ungroup() %>%
  select(nhs_board_of_treatment, specialty, indicator = ongoing_completed, 
         urgency, date, number = `number_seen/on_list`, proportion = `proportion_seen/on_list`) %>%
  mutate(proportion = proportion/100) %>%
  bind_rows(select(addrem_qtr_split, - total)) %>%
  filter(str_detect(topsix$specialties[topsix$date == max_date & topsix$nhs_board_of_treatment == "NHS Scotland"], specialty), 
         nhs_board_of_treatment=="NHS Scotland",
         indicator %in% c("additions_to_list", "Completed", "Ongoing")) %>%
  ungroup() %>%
  left_join(select(ungroup(spec_p2_prop), -c(indicator, number)),
            by = c("specialty")) %>%
  unique() %>%
  arrange(indicator,-p2_prop)

#Save March and June graphs
ggsave("top_six_spec_plot_additions_jun.png", plot = topsixplot(topsix_plot_data, max_date), dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "June 2022"))

ggsave("top_six_spec_plot_additions_mar.png", plot = topsixplot(topsix_plot_data, max_date2), dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))

#3.1.3 - HBT variation ----
#Graph
#bar chart qe march, All Speciaties, stacked by urgency code, hbt on x axis, facet additions/seen/waiting

#additions from addrem_qtr
hb_var_data <- perf_qtr_split %>%
  ungroup() %>%
  select(-c(waited_waiting_over_52_weeks:y_max)) %>%
  rename(number = `number_seen/on_list`,
         indicator = `ongoing_completed`) %>%
  bind_rows(addrem_qtr) %>%
  group_by(nhs_board_of_treatment, indicator, specialty, date) %>%
  mutate(total = sum(number[!urgency=="Total"], na.rm = TRUE),
         p2_proportion = sum(number[urgency == "P2" & !urgency=="Total"])/total) %>%
  group_by(nhs_board_of_treatment, indicator, specialty, urgency, date) %>%
  mutate(proportion = number/total) 


#Calculate proportion that is P2 to allow ordering of Boards
hb_p2_prop <- hb_var_data %>% 
  ungroup() %>%
  filter(indicator == "additions_to_list", urgency=="P2") %>% 
  select(date, specialty, nhs_board_of_treatment, indicator, p2_proportion)


#Subset data for plotting and bind on 
hb_var_plotdata <- hb_var_data %>% 
  filter(indicator %in% c("additions_to_list", "Completed", "Ongoing")) %>% 
  left_join(ungroup(hb_p2_prop)) %>% 
  arrange(date, indicator,-`p2_proportion`)

#Save March and June graphs
ggsave("hb_var_plot_jun.png", plot = hb_var_plot(hb_var_plotdata, max_date), dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "June 2022"))

ggsave("hb_var_plot_mar.png", plot = hb_var_plot(hb_var_plotdata, max_date2), dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))

#Save this image
ggsave("hb_var_plot_2.png", dpi=300, dev='png', height=10, width=20, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#3.1.4 - HBT comparison for a particular specialty ----
#A&A and Lanarkshire for ophthalmology?
#Save version for QE March
ggsave("hb_comparison_ophthalmology_dg_fv_mar.png", plot = hb_spec_plot(hb_var_plotdata, max_date2, "Ophthalmology", c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=12, width=26, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))


#3.2 - Distribution of waits ----
#3.2.1 - Barplot of number seen/waiting by 4 week intervals and CP split ----

dow_4wk_plot <- dow_4wk_qtr_pub %>%
  mutate(weeks2 = case_when(weeks == "000-004"  ~"<=4",
                            weeks == "Over 104" ~">104",
                            TRUE ~  gsub("(?<![0-9])0+", "", weeks, perl = TRUE))) 

#Save this plot
ggsave("dow Scotland all specs qe mar 2022.png", plot = dow_barplot(dow_4wk_plot,"NHS Scotland", "All Specialties", max_date2), dpi=300, dev='png', height=15, width=18, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))

#3.2.2 - Barplot of two contrasting specialties (Gynae and Ophthalmology) ----
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

#Save plot for QE March 2022
ggsave("dow_ortho_urology_mar2022.png", plot = spec_dow_bar(dow_4wk_plot, c("Urology", "Orthopaedics"), max_date2, "NHS Scotland"), dpi=300, dev='png', height=18, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))


#3.2.3 - Barplot of two contrasting Boards for single specialty (D&G and FV) ----

#Save March Ophthalmology D&G and FV plot
ggsave("dow_ophthalmology_d&g_fv_mar2022.png", plot = hb_dow_bar(dow_4wk_plot,"Ophthalmology", max_date2, c("NHS Dumfries & Galloway", "NHS Forth Valley")), dpi=300, dev='png', height=15, width=20, units="cm", path = here::here("..","R plots", "Snapshot plots", "March 2022"))
