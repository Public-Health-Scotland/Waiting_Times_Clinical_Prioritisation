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

#### 2 - Import data ----

# Copy data quality summaries

file.copy(here::here("data", "dq_summaries.csv"),
          here::here("data", "processed data", "dq_summaries.csv"))

#2.1 - Specialty exclusions ----
#Use the latest specialty exclusions list from the publication folder

exclusions_path <- here::here("data", "Spec Exclusions.xlsx")

exclusions <- read.xlsx(exclusions_path, sheet = "IPDC", na.strings = "") %>%
  as.list(Specialties)


#2.2 - Performance ----
#Read in the BOXI publication output, reformat dates and select correct specialties

#Read in publication performance data to get 2019 all specs averages
perf_2019 <- read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/PerformanceIPDC.csv",
                      check.names = FALSE, stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(year(Date) =="2019",
         Specialty == "All Specialties") %>%
  group_by(`NHS Board of Treatment`, `Ongoing/Completed`, Specialty) %>%
  summarise(monthly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed",
                                                           sum(`Number Seen/On list`, na.rm=T)/12,
                                                           mean(`Number Seen/On list`, na.rm=T)),0),0),
            quarterly_avg = replace_na(round_half_up(if_else(`Ongoing/Completed` =="Completed",
                                                             mean(`Number Seen/On list`, na.rm=T),
                                                             mean(`Number Seen/On list`, na.rm=T)),0),0))%>%
  rename(Indicator = `Ongoing/Completed`) %>%
  unique()

#Read in publication performance data to get 2019 individual specs averages
perf_2019_specs <- read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/PerformanceIPDC.csv",
                            check.names = FALSE, stringsAsFactors = FALSE) %>%
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
  #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  clean_names(use_make_names = FALSE) %>%
  mutate(date = openxlsx::convertToDate(date), #Convert dates from Excel format
         #Rename T&O as orthopaedics
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))


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
  filter(urgency!="Total") %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(
            ifelse(`number_seen/on_list`!=0,
                    100*`number_seen/on_list`/sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T),
                   0),
            2),
         #Need to exclude the Total group to avoid double-counting
         y_max = sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max, na.rm=TRUE))) #calculate max y for graph limits

#Create version for Excel with monthly/qtrly averages
perf_avg <- perf_split %>%
  left_join(perf_2019_specs,
            by = c("nhs_board_of_treatment" = "NHS Board of Treatment",
                   "ongoing_completed" = "Indicator",
                   "specialty" = "Specialty")) %>%
  mutate(across(c(monthly_avg, quarterly_avg), ~ replace_na(.x,0)))

#Completeness per month
perf_comp <- perf %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  summarise(completeness = round(
    100*sum(`number_seen/on_list`[!urgency %in% c("Other", "Total")], na.rm=T)/sum(
      `number_seen/on_list`[!urgency=="Total"], na.rm=T),
    2)) %>%
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
  mutate(`proportion_seen/on_list` = round(
              ifelse(`number_seen/on_list`!=0,
                      100*`number_seen/on_list`/sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T),
                     0),
              1),
         y_max = sum(`number_seen/on_list`[!urgency=="Total"], na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max, na.rm=TRUE))) #calculate max y for graph limits


#2.3 - Distribution of wait ----
#dow 4 week bands data for publication, max date set to end of latest quarter

dow_4wk <- read.xlsx(here::here("data", "snapshot", "Distribution of Waits 4 week bands.xlsx"),
                     sheet = "IPDC Clinical Prioritisation", detectDates = FALSE) %>%
  #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  clean_names(use_make_names = FALSE) %>%
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
dow_large <-  read.xlsx(here::here("data", "snapshot", "Distribution of Waits larger time bands.xlsx"),
                        sheet = "IPDC Clinical Prioritisation") %>%
  #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  clean_names(use_make_names = FALSE) %>%
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
         weeks = as.factor(ifelse(weeks != ">104 Weeks", substr(weeks, 1, 7), "Over 104")),
         #Rename T&O as orthopaedics
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>%
  filter(!specialty %in% exclusions$Specialties) %>%
  complete(urgency, weeks, date, ongoing_completed,
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0))

#2.4 - Additions by HBT ----
addrem <- read.xlsx(here::here("data","snapshot", "Removal Reason excl. Lothian Dental.xlsx"),
                    sheet = "IPDC Clinical Prioritisation") %>%
  #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  clean_names(use_make_names = FALSE) %>%
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
         #Rename T&O as orthopaedics
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>%
  filter(!specialty %in% exclusions$Specialties) %>%
  pivot_longer(c(additions_to_list:other_reasons), values_to = "number", names_to = "indicator") %>%
  complete(urgency, date, indicator,
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(number = 0)) %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, date, indicator) %>%
  mutate(proportion_CP = ifelse(urgency=="Total",
                                NA,
                                round(100*number/sum(number[!urgency=="Total"], na.rm=T),2)),
         completeness = round(100*sum(number[!urgency %in% c("Other","Total")], na.rm=T)/sum(
           number[!urgency=="Total"], na.rm=T),2)) #Calculate proportion of indicator by CP


#Additions only for Excel MI
add_mon <- addrem %>%
  filter(indicator == "additions_to_list")

#Additions completeness
add_comp <- addrem %>%
  filter(indicator == "additions_to_list") %>%
  select(patient_type, indicator, nhs_board_of_treatment, specialty, date, completeness) %>%
  summarise(completeness = unique(completeness)) #Only take one row per group

#Completeness for all metrics
total_comp <- bind_rows(add_comp, perf_comp) %>%
  select(patient_type,indicator,nhs_board_of_treatment, specialty, date, completeness)

#Quarterly additions ----
addrem_qtr <- addrem %>%
  group_by(patient_type, nhs_board_of_treatment, indicator,  specialty, urgency,
           date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(number = if_else(!urgency=="Total", sum(number[!urgency=="Total"], na.rm = T),
                             sum(number[urgency=="Total"], na.rm = T))) %>%
  unique()


#2.4.1 - long-term additions to get 2019 average ----
#All Specialties only version
add_2019 <-read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/Removal ReasonsIPDC.csv",
                    stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(year(Date) =="2019",
         Specialty == "All Specialties") %>%
  group_by(`NHS Board of Treatment`, Specialty) %>%
  summarise(Indicator = "additions_to_list",
            monthly_avg =  replace_na(round_half_up(sum(`Additions to list`, na.rm=T)/12,0),0),
            quarterly_avg = replace_na(round_half_up(mean(`Additions to list`, na.rm = T),0),0))

#Specialty level version
add_2019_specs <-read.csv("/PHI_conf/WaitingTimes/SoT/Publications/Inpatient, Day case and Outpatient Stage of Treatment Waiting Times/Publication R Script/Publication/Output/Removal ReasonsIPDC.csv",
                          stringsAsFactors = FALSE, check.names = FALSE) %>%
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
            by = c("nhs_board_of_treatment" = "NHS Board of Treatment",
                   "specialty" = "Specialty",
                   "indicator"="Indicator"))


#2.4.2 - Create 2019 average combined lookup and combined data for additions, completed, waiting ----

avg_2019 <- bind_rows(add_2019, add_2019_specs, perf_2019, perf_2019_specs)

add_perf_monthly  <- perf_split %>% #First modify perf_split
  rename(indicator = ongoing_completed,
         number = `number_seen/on_list`) %>%
  select(-c(waited_waiting_over_26_weeks:y_max)) %>%
  bind_rows(select(addrem %>% select(-completeness) %>%
                     filter(indicator == "additions_to_list"),
                   -c(starts_with("proportion")))) %>% #Then bind onto filtered additions
  # filter(specialty=="All Specialties") %>%
  left_join(select(avg_2019, -quarterly_avg),
            by=c("nhs_board_of_treatment" = "NHS Board of Treatment",
                 "indicator" = "Indicator",
                 "specialty" = "Specialty")) %>% #Then bind on monthly averages from 2019
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number[!urgency=="Total"], na.rm=T)), #Calculate max from current data per group
         y_max2 = roundUpNice(max(monthly_avg, na.rm=TRUE))) #Calculate max from 2019 data per group

add_perf_quarterly  <- perf_qtr_split %>% #First modify perf_split
  rename(indicator = ongoing_completed,
         number = `number_seen/on_list`) %>%
  select(-c(waited_waiting_over_26_weeks:y_max)) %>%
  bind_rows(select(addrem_qtr %>%
                     filter(indicator == "additions_to_list"),
                   -c(starts_with("proportion")))) %>% #Then bind onto filtered additions
  #filter(specialty=="All Specialties") %>%
  left_join(select(avg_2019, -monthly_avg), by=c("nhs_board_of_treatment" = "NHS Board of Treatment",
                                                 "indicator" = "Indicator",
                                                 "specialty" = "Specialty")) %>% #Then bind on monthly averages from 2019
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number[!urgency=="Total"], na.rm=T)), #Calculate max from current data per group
         y_max2 = roundUpNice(max(quarterly_avg, na.rm=TRUE))) #Calculate max from 2019 data per group


# 2.6 HBT Variation Plot Data ---------------------------------------------
#additions from addrem_qtr
hb_var_data <- perf_qtr_split %>%
  ungroup() %>%
  select(-c(waited_waiting_over_26_weeks:y_max)) %>%
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
  select(date, specialty, nhs_board_of_treatment, p2_proportion)


#Subset data for plotting and bind on
hb_var_plotdata <- hb_var_data %>%
  filter(indicator %in% c("additions_to_list", "Completed", "Ongoing")) %>%
  select(-p2_proportion) %>%
  left_join(ungroup(hb_p2_prop), by = c("date", "nhs_board_of_treatment", "specialty")) %>%
  arrange(date, indicator,-`p2_proportion`)

#Bind p2_proportion onto dow_4wk_qtr_pub to allow sorting in the app
dow_4wk_qtr_pub %<>%
  left_join(ungroup(hb_p2_prop), by = c("date", "nhs_board_of_treatment", "specialty")) %>%
  arrange(date, -`p2_proportion`)

# 2.7 Top 6 Specialties Lookup  ------
#Identify top 6 specialties by number waiting, calculate what proportion of waiting and seen these represent

#Calculate the proportion of additions represented by each specialty, per HB
add_stats <- addrem_qtr %>%
  filter(indicator == "additions_to_list") %>%
  group_by(date, indicator, nhs_board_of_treatment) %>%
  mutate(allspec = sum(number[specialty=="All Specialties" & !urgency=="Total"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date, indicator, nhs_board_of_treatment, specialty) %>%
  summarise(number = sum(number[!urgency=="Total"], na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = round(100*number/allspec,2)) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique()

#Calculate the proportion of admissions and ongoing waits represented by each specialty per HB, bind onto additions
specstats <- perf_qtr_split  %>%
  group_by(date, patient_type, ongoing_completed, nhs_board_of_treatment) %>%
  mutate(allspec = sum(`number_seen/on_list`[specialty=="All Specialties" & !urgency=="Total"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date,patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  summarise(`total_seen/on_list` = sum(`number_seen/on_list`[!urgency=="Total"], na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = round(100*`total_seen/on_list`/allspec,2)) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique() %>%
  rename(indicator = ongoing_completed,
         number = `total_seen/on_list`) %>%
  ungroup() %>%
  select(-`patient_type`) %>%
  bind_rows(add_stats)

#Lists of top six specialties per HBT and quarter
topsix <- specstats %>%
  filter(indicator=="Ongoing", !specialty=="All Specialties") %>%
  arrange(desc(proportion)) %>%
  group_by(nhs_board_of_treatment, date) %>%
  slice(1:6) %>%
  group_by(date, nhs_board_of_treatment) %>%
  summarise(specialties = as.character(list(unique(specialty))))

##Data for top six specialties
topsixstats <- specstats %>%
left_join(topsix, by=c("nhs_board_of_treatment", "date")) %>%
filter(str_detect(specialties, specialty))

##Proportion of total seen/waiting represented by these 6 specialties
topsix_prop <- topsixstats %>%
  filter(nhs_board_of_treatment == "NHS Scotland") %>%
  group_by(date, indicator) %>%
  summarise(`proportion of total` = sum(proportion))

##Calculate proportion of additions that are P2 per specialty
spec_p2_prop <- addrem_qtr  %>%
  filter(nhs_board_of_treatment == "NHS Scotland",
         indicator == "additions_to_list",
         date == max_date) %>%
  group_by(indicator,specialty) %>%
  summarise(total = sum(number[!urgency=="Total"], na.rm = T),
         p2_prop = round(sum(number[urgency == "P2"], na.rm = T)/total,2)) %>%
  select(indicator, specialty, number = total, p2_prop)

##Bind top six list onto hb_var_plotdata for use in topsix plot function
hb_var_plotdata %<>%
  left_join(topsix, by = c("nhs_board_of_treatment", "date"))

# 2.8 - Save data for Excel and app ----

# LHS is base for file name, RHS is object to write
marjun_data <- list("add_perf_mon_specs" = add_perf_monthly,
              "add_perf_qtr_specs" = add_perf_quarterly,
              "add_mon" = add_mon,
              "perf_qtr_split" = perf_qtr_split,
              "perf_mon_split" = perf_split,
              "perf_avg" = perf_avg,
              "total_comp" = total_comp,
              "dow_4wk_qtr_pub" = dow_4wk_qtr_pub,
              "dow_4wk_mon" = dow_4wk,
              "hb_plotdata" = hb_var_plotdata,
              "topsix_specs" = topsix)

message("Writing data to processed_data")
# Writing data for March and June
purrr::walk(names(marjun_data), write_marjun_data)

##12 - specstats
write.csv(specstats, file = here::here("data", "processed data", "specstats.csv"), row.names = FALSE)

##13 - spec P2 proportions
write.csv(spec_p2_prop, file = here::here("data", "processed data", "spec_p2_prop.csv"), row.names = FALSE)

