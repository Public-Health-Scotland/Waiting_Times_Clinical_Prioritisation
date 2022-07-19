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


# Function to highlight particular axis labels (e.g. NHS Scotland) 
highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
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

#monthly ipdc wt data
perf_all <- read.xlsx(here::here("data", "Performance excl. Lothian Dental Monthly Week Flags.xlsx"),
                      sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) #%>% #Rename T&O as orthopaedics
#  rename("waited_waiting_over_52_weeks"="waited_waiting_over_54_weeks") #temp fix for typo


#monthly data for report, July 2021 to latest complete quarter
perf <- perf_all %>%
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#Create version of data that has proportions per CP code per month
perf_split <- perf %>% 
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits



#2.2.2 - Quarterly ---- 
perf_qtr_all <- read.xlsx(here::here("data", "Performance excl. Lothian Dental Quarterly Week Flags.xlsx"), 
                          sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) 

#data for report up to latest complete quarter
perf_qtr <- perf_qtr_all %>% 
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12),
                ongoing_completed == "Completed")) %>%
  complete(urgency, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0,
                       waited_waiting_over_52_weeks = 0,
                       waited_waiting_over_104_weeks = 0)) 


#Create version of data that has proportions per CP code per month
perf_qtr_split <- perf_qtr %>% 
  filter(date <= max_date) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty, date) %>%
  mutate(`proportion_seen/on_list` = round(ifelse(`number_seen/on_list`!=0, 
                                                  100*`number_seen/on_list`/sum(`number_seen/on_list`, na.rm=T), 0), 1),
         y_max = sum(`number_seen/on_list`, na.rm=T)) %>%
  group_by(patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>%
  mutate(y_max = roundUpNice(max(y_max))) #calculate max y for graph limits


#2.3 - Distribution of wait ----
#dow 4 week bands data for publication, max date set to end of latest quarter

dow_4wk <- read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation", detectDates = FALSE) %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date = if_else(ongoing_completed == "Completed", openxlsx::convertToDate(date), dmy(date)),
         weeks = as.factor(ifelse(weeks != "Over 104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))

#uncomment code below if dates do not parse properly 
# #DoW ongoing waits
# dow_4wk_ongoing <- read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation", detectDates = TRUE) %>%
#   clean_names(use_make_names = FALSE) %>%
#   filter(ongoing_completed=="Ongoing") %>%
#   mutate(date= base::as.Date(date, format = "%d/%m/%Y"))
# 
# #DoW completed waits
# dow_4wk_comp <- read.xlsx("data/Distribution of Waits 4 week bands.xlsx", sheet = "IPDC Clinical Prioritisation", detectDates = TRUE) %>%
#   clean_names(use_make_names = FALSE) %>%
#   filter(ongoing_completed=="Completed") %>%
#   mutate(date= base::as.Date(date, format = "%Y-%m-%d"))
# 
# #bind completed and ongoing into a single df
# dow_4wk <- rbind(dow_4wk_comp, dow_4wk_ongoing) %>%
#   mutate(weeks = as.factor(ifelse(weeks != "Over 104 Weeks", substr(weeks, 1, 7), "Over 104")),
#          specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty))


#quarterly 4 week bands dow data for publication
dow_4wk_qtr_pub <- dow_4wk %>%
  filter(ifelse(ongoing_completed == "Ongoing", month(date) %in% c(3,6,9,12),
                ongoing_completed == "Completed")) %>%
  #convert monthly dates to end of quarter dates
  mutate(date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  group_by(across(-`number_seen/on_list`)) %>%
  #get the sum of waits/patients seen for each quarter
  summarise(`number_seen/on_list` = sum(`number_seen/on_list`))

#dow large week bands data for publication, max date set to end of latest quarter
dow_large <-  read.xlsx("data/Distribution of Waits larger time bands.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         weeks = as.factor(ifelse(weeks != ">104 Weeks", substr(weeks, 1, 7), "Over 104")),
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) #Rename T&O as orthopaedics%>%
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
  complete(urgency, weeks, date, ongoing_completed, 
           nesting(nhs_board_of_treatment, specialty, patient_type),
           fill = list(`number_seen/on_list` = 0)) 

#2.4 - Additions by HBT ----
addrem <- read.xlsx("data/Removal Reason excl. Lothian Dental.xlsx", sheet = "IPDC Clinical Prioritisation") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format 
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date), !specialty %in% exclusions) %>%
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


#Quarterly additions ----

addrem_qtr <- addrem %>%
  group_by(nhs_board_of_treatment, indicator,  specialty, urgency, date = as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(number = sum(number, na.rm = T))


#2.4.1 - long-term additions to get 2019 average ----
add_2019 <- import_list("/PHI_conf/WaitingTimes/SoT/Projects/R Shiny DQ/Live BOXI/RR Monthly.xlsx", rbind =  TRUE) %>%
  select(- `_file`) %>%
  filter(year(as.yearmon(Date, "%m %Y")) =="2019",
         `NHS Board of Treatment` == "NHS Scotland",
         Specialty == "All Specialties",
         `Patient Type` == "Inpatient/Day case") %>%
  select(`Additions to list`) %>%
  summarise(Indicator = "additions_to_list",
            monthly_avg = round(mean(`Additions to list`),0))

#2.4.2 - Create 2019 average combined lookup and combined data for additions, completed, waiting ----

avg_2019 <- bind_rows(add_2019, perf_2019)

add_perf  <- perf_split %>% #First modify perf_split
  rename(indicator = ongoing_completed,
         number = `number_seen/on_list`) %>%
  select(-c(waited_waiting_over_52_weeks:y_max)) %>%
  bind_rows(select(addrem %>% filter(indicator == "additions_to_list"),-c(starts_with("proportion")))) %>% #Then bind onto filtered additions
  filter(specialty=="All Specialties",
         nhs_board_of_treatment=="NHS Scotland") %>%
  left_join(avg_2019, by=c("indicator" = "Indicator")) %>% #Then bind on monthly averages from 2019
  group_by(nhs_board_of_treatment, specialty, indicator, date) %>%
  mutate(y_max = roundUpNice(sum(number, na.rm=T)), #Calculate max from current data per group
         y_max2 = roundUpNice(max(monthly_avg))) #Calculate max from 2019 data per group


#2.5 - Additions by HBR ----
addhbr <- read.xlsx("data/Removal Reason excl. Lothian Dental by age gender.xlsx", sheet = "IPDC Additions HBR") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date), !specialty %in% exclusions,
         !age_group == "Blank", #Exclude records with "Blank" age
         !gender == "Blank") %>% #Exclude records with "Blank" gender
  rename(sex = gender) %>% #Rename to match population lookup
  mutate(across(c(age_group, sex), ~as.factor(.x)), #Convert age_group and sex to factors
  age_group = fct_relevel(age_group, "5-9", after = 1)) #Put level "5-9" after "0-4"

#2.6 - Save data for Excel and app ----
#1 - add_perf
write.csv(add_perf, file = here::here("data", "processed data", "add_perf.csv"), row.names = FALSE)

#2 - addrem_qtr
#Not needed in the present analysis/output

#3 - perf_qtr_split
write.csv(perf_qtr_split, file = here::here("data", "processed data", "perf_qtr_split.csv"), row.names = FALSE)

#4 - dow_4wk_qtr_pub
write.csv(dow_4wk_qtr_pub, file = here::here("data", "processed data", "dow_4wk_qtr_pub.csv"), row.names = FALSE)

#6 - addhbr
write.csv(addhbr, file = here::here("data", "processed data", "addhbr.csv"), row.names = FALSE)

#6 - add_simd
write.csv(add_simd, file = here::here("data", "processed data", "add_simd.csv"), row.names = FALSE)

#### 3 - Data wrangling ----

#3.1 - Completed and ongoing waits ----

#3.1.1 - Graph of ongoing and completed waits, by month ----
activity_trendplot <- add_perf %>% 
  ggplot(aes(x =floor_date(date, "month"), y = number), group = urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  geom_hline(aes(yintercept=monthly_avg, #Add monthly averages
                 linetype = "2019 monthly average"), 
             colour = "#000000") +
  scale_linetype_manual(name ="", values = c('dashed')) +
  theme_bw() +
  scale_x_date(labels = date_format("%b %y"),
               breaks = seq(from = floor_date(min(addrem$date), "month"), 
                            to = floor_date(max(addrem$date), "month"), by = "1 months")) +
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name="")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name ="") +
  # scale_linetype_manual(name = "2019 average",values = c(1,1)) +
  theme(text = element_text(size = 12))+
  geom_blank(aes(y = y_max)) +
  geom_blank(aes(y = y_max2)) +
  facet_wrap(~indicator, nrow = 3, scales = "free_y",  strip.position = "top", 
             labeller = as_labeller(c(additions_to_list ="Additions to list", Ongoing = "Patients waiting", Completed = "Patients admitted") )) +
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

#Save this image
ggsave("allspecs_activity_trend_monthly.png", plot = activity_trendplot, dpi=300, dev='png', height=20, width=18, units="cm", path = here::here("..","R plots", "Plots for draft report"))

#3.1.2 - Top 6 specialties by additions/admitted/waiting ----

#Identify top 6 specialties by number waiting, calculate what proportion of waiting and seen these represent 

#Calculate the proportion of additions represented by each specialty, per HB 
add_stats <- addrem_qtr %>%
  filter(indicator == "additions_to_list") %>%
  group_by(date, indicator, nhs_board_of_treatment) %>%
  mutate(allspec = sum(number[specialty=="All Specialties"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date, indicator, nhs_board_of_treatment, specialty) %>% 
  summarise(number = sum(number, na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = 100*number/allspec) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique()

#Calculate the proportion of admissions and ongoing waits represented by each specialty per HB, bind onto additions 
specstats <- perf_qtr  %>% 
  group_by(date,patient_type, ongoing_completed, nhs_board_of_treatment) %>%
  mutate(allspec = sum(`number_seen/on_list`[specialty=="All Specialties"],na.rm=T)) %>% #Add all specialties total added to all rows
  group_by(date,patient_type, ongoing_completed, nhs_board_of_treatment, specialty) %>% 
  summarise(`total_seen/on_list` = sum(`number_seen/on_list`, na.rm = T), #Calculate total seen/waiting for each specialty (sum across CP codes)
            proportion = 100*`total_seen/on_list`/allspec) %>% #Calculate the proportion of all seen/waiting for each specialty
  unique() %>%
  rename(indicator = ongoing_completed,
         number = `total_seen/on_list`) %>%
  ungroup() %>%
  select(-`patient_type`) %>%
  bind_rows(add_stats)


#List of top six specialties, by number of ongoing waits
topsix <- specstats %>%
  filter(date == max_date, indicator=="Ongoing", nhs_board_of_treatment == "NHS Scotland", !specialty=="All Specialties") %>% 
  arrange(desc(proportion)) %>% 
  ungroup() %>%
  head(n=6) %>%
  select(specialty) %>%
  as.list()

#Data for top six specialties 
specstats %<>% filter(specialty %in% topsix$specialty, date == max_date)

#Proportion of total seen/waiting represented by these 6 specialties
topsix_prop <- specstats %>%
  filter(nhs_board_of_treatment == "NHS Scotland") %>%
  group_by(indicator) %>%
  summarise(`proportion of total` = sum(proportion))

#Calculate proportion of additions that are P2 per specialty
spec_p2_prop <- addrem_qtr  %>%
  filter(nhs_board_of_treatment == "NHS Scotland",
         indicator == "additions_to_list",
         date == max_date) %>%
  group_by(specialty) %>%
  mutate(total = sum(number, na.rm = T),
         p2_prop = sum(number[urgency == "P2"], na.rm = T)/total) %>%
  select(indicator, specialty, number, p2_prop)# %>%
  rename("p2_prop"="proportion_seen/on_list")

  
#Calculate proportion of additions by HB/spec/CP/date
addrem_qtr_split <- addrem_qtr %>%
  group_by(nhs_board_of_treatment, specialty, indicator,date) %>%
  mutate(total = sum(number, na.rm = T)) %>%
  ungroup() %>%
  mutate(proportion = number/total)

topsix_plot_data <- perf_qtr_split %>%
  ungroup() %>%
  select(nhs_board_of_treatment, specialty, indicator = ongoing_completed, urgency, date, number = `number_seen/on_list`, proportion = `proportion_seen/on_list`) %>%
  mutate(proportion = proportion/100) %>%
  bind_rows(select(addrem_qtr_split, - total)) %>%
  filter(specialty %in% topsix$specialty, 
         nhs_board_of_treatment=="NHS Scotland",
         indicator %in% c("additions_to_list", "Completed", "Ongoing"),
         date == max_date) %>%
  ungroup() %>%
  left_join(select(ungroup(spec_p2_prop), -c(indicator, number)),
            by = c("specialty")) %>%
  unique() %>%
  arrange(indicator,-p2_prop)

#Graph
topsixplot <- topsix_plot_data %>%
  ggplot(aes(x = fct_reorder(specialty, p2_prop, .desc = TRUE), y = proportion), group = urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity") +
  theme_bw() +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~indicator, nrow=3, strip.position = "top",
             labeller = as_labeller(c(additions_to_list = "Additions to list", Completed = "Patients admitted", Ongoing = "Patients waiting") )) +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(1, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.margin=margin(0,0,0,0),
        legend.spacing= unit(0.0, "cm"),
        legend.text = element_text(size = 8))

#Save this image
ggsave("top_six_spec_plot_additions.png", plot = topsixplot, dpi=300, dev='png', height=24, width=20, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#3.1.3 - HBT variation ----
#Graph
#bar chart qe march, All Speciaties, stacked by urgency code, hbt on x axis, facet additions/seen/waiting

#additions from addrem_qtr
hb_var_data <- perf_qtr_split %>%
  ungroup() %>%
  select(-c(patient_type, waited_waiting_over_52_weeks:y_max)) %>%
  rename(number = `number_seen/on_list`,
         indicator = `ongoing_completed`) %>%
  bind_rows(addrem_qtr) %>%
  group_by(nhs_board_of_treatment, indicator, specialty, date) %>%
  mutate(total = sum(number, na.rm = TRUE),
         p2_proportion = sum(number[urgency == "P2"])/total) %>%
  group_by(nhs_board_of_treatment, indicator, specialty, urgency, date) %>%
  mutate(proportion = number/total) 
  

#Calculate proportion that is P2 to allow ordering of Boards
hb_p2_prop <- hb_var_data %>% 
  ungroup() %>%
  filter(indicator == "additions_to_list", date == max_date, urgency=="P2") %>% 
  select(specialty, nhs_board_of_treatment, indicator, p2_proportion)


#Subset data for plotting and bind on 
hb_var_plotdata <- hb_var_data %>% 
  filter(date == max_date,
         indicator %in% c("additions_to_list", "Completed", "Ongoing")) %>% 
  left_join(ungroup(hb_p2_prop)) %>% #select(ungroup(hb_p2_prop), -ongoing_completed), 
            #by =c("nhs_board_of_treatment", "patient_type", "specialty")) %>% 
  arrange(indicator,-`p2_proportion`)


#Create the plot
hb_var_plot <- hb_var_plotdata %>% 
  filter(specialty == "All Specialties") %>%
  ggplot(aes(x = fct_reorder(nhs_board_of_treatment, p2_proportion, .desc =FALSE), y = proportion), urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity", width=0.75) +
  #scale_x_reordered() +
  theme_bw() + 
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_x_discrete(labels= function(x) highlight(x, "NHS Scotland", "black")) +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(.~indicator, 
             labeller = as_labeller(c(`additions_to_list` = "Additions to list", Completed = "Patients admitted", Ongoing = "Patients waiting"))) +
  #facet_grid(cols = vars(ongoing_completed), scales = "free_x",drop = TRUE)+
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.text = element_text(size = 8)) +
  coord_flip() #+
  #theme(axis.text.y=element_markdown())

#Save this image
ggsave("hb_var_plot_2.png", dpi=300, dev='png', height=10, width=20, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#3.1.4 - HBT comparison for a particular specialty ----
#A&A and Lanarkshire for ophthalmology?
hb_spec_plot <- hb_var_plotdata %>% 
  filter(specialty == "Ophthalmology",
         nhs_board_of_treatment %in% c("NHS Dumfries & Galloway", "NHS Forth Valley")) %>%
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
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.25, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.text = element_text(size = 10)) #+
 # coord_flip() +
  theme(axis.text.y=element_markdown())

#Save this image
ggsave("hb_comparison_ophthalmology_dg_fv.png", plot = hb_spec_plot, dpi=300, dev='png', height=12, width=26, units="cm", path = here::here("..","R plots", "Plots for draft report"))

#3.2 - Distribution of waits ----
#3.2.1 - Barplot of number seen/waiting by 4 week intervals and CP split ----

dow_4wk_plot <- dow_4wk_qtr_pub %>%
  mutate(weeks2 = case_when(weeks == "000-004"  ~"<=4",
                            weeks == "Over 104" ~">104",
                            TRUE ~  gsub("(?<![0-9])0+", "", weeks, perl = TRUE))) 

dow_barplot <- dow_4wk_plot %>%
  filter(nhs_board_of_treatment == "NHS Scotland", specialty == "All Specialties", date == max_date) %>%
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
             labeller = as_labeller(c(Completed = "Patients admitted", Ongoing = "Patients waiting"))) +
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

#Save this plot
ggsave("dow Scotland all specs qe mar 2022.png", dpi=300, dev='png', height=15, width=18, units="cm", path = here::here("..","R plots", "Plots for draft report"))

#3.2.2 - Barplot of two contrasting specialties (Gynae and Ophthalmology) ----
spec_dow_bar <-  dow_4wk_plot %>%
  filter(specialty %in% c("Urology", "Orthopaedics"), date == max_date, nhs_board_of_treatment =="NHS Scotland") %>%
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


#spec_dow_bar <-  dow_4wk_plot %>%
#  filter(specialty %in% c("Urology", "Orthopaedics"), date == max_date, nhs_board_of_treatment =="NHS Scotland") %>%
#  group_by(nhs_board_of_treatment,  ongoing_completed, specialty, weeks, date) %>%
#  mutate(y_max = roundUpNice(sum(`number_seen/on_list`, na.rm=T))) %>%
#  group_by(nhs_board_of_treatment, ongoing_completed, date) %>%
#  mutate(y_max = max(y_max),
#         ongoing_completed = if_else(ongoing_completed =="Ongoing", "Patients waiting", "Patients admitted")) %>%
#  unite("BothLabels", ongoing_completed, specialty, sep = " - ", remove = FALSE) %>% #Create labels
#  ggplot(aes(x = weeks, y = `number_seen/on_list`, group = BothLabels)) +
#  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)),
#               fill=fct_rev(factor(urgency, levels = colourset$codes))),
#           stat="identity") +
#  geom_blank(aes(y = plyr::round_any(y_max,1000,f = ceiling))) + #add blank geom to extend y axis up to nearest 1000
#  theme_bw() +
#  scale_x_discrete(labels = unique(dow_4wk_plot$weeks2)) +
#  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
#  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
#  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
#  facet_grid(ongoing_completed~specialty, scales = "free_y", labeller = as_labeller(paste0(dow_4wk_plot$ongoing_completed, dow_4wk_plot$specialty, sep = " #- "))) + #,
#  ylab(NULL) +
#  xlab("Weeks waited or waiting") +
#  theme(text = element_text(size = 12),
#        strip.background = element_blank(),
#        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
#        axis.text.x = element_text(angle = 45, hjust = 1),
#        panel.spacing = unit(0.5, "cm"),
#        panel.border = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        panel.grid.major.x = element_blank(),
#        legend.position="bottom",
#        legend.key.height= unit(0.25, 'cm'),
#        legend.key.width= unit(0.25, 'cm'),
#        legend.text = element_text(size = 8))
#

#Save this plot
ggsave("dow_ortho_urology_mar2022.png", plot = spec_dow_bar, dpi=300, dev='png', height=18, width=20, units="cm", path = here::here("..","R plots", "Plots for draft report"))



#3.2.3 - Barplot of two contrasting Boards for single specialty (D&G and FV) ----
hb_dow_bar <-  dow_4wk_plot %>%
  filter(specialty =="Ophthalmology", date == as.Date("2022-03-31"), nhs_board_of_treatment %in% c("NHS Dumfries & Galloway", "NHS Forth Valley")) %>%
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
  facet_wrap(~BothLabels, nrow = 2,  strip.position = "top") + #,
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

#Save this plot
ggsave("dow_ophthalmology_d&g_fv_mar2022.png", plot = hb_dow_bar, dpi=300, dev='png', height=15, width=20, units="cm", path = here::here("..","R plots", "Plots for draft report"))

#3.2.4 - Barplot showing number waiting by defined wait lengths by HBT ----
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

#3.3.1 - Trend in additions and removals by CP (barplot) ----
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


#3.3.2 - Trend in additions and removals, by CP category (line plot) ----
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

#3.4.1.A - Age-sex standardisation using European Standard Population 2013 ----

#*1 Define populations path in cl-out ----
pop_path <- ("/conf/linkage/output/lookups/Unicode/Populations")

#*2 - Define age groups ----
agebreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,500)
agelabels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39", "40-44", "45-49", "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")

#*3 - Get population projections for 2021 onwards and calculate Scotland total ----
pop <- readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
  mutate(board = paste0("NHS ",str_replace(hb2019name, " and ", " & "))) %>% #Reformat names to match other data 
  bind_rows(readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
              group_by(year, age, sex_name) %>%
              summarise(pop = sum(pop, na.rm=T)) %>%
              mutate(board = "NHS Scotland")) %>%
  filter(year >= "2021") %>%
  mutate(age_group = as.factor(cut(age, agebreaks, agelabels, include.lowest = TRUE, right= FALSE)), 
           sex_name = as.factor(sex_name),
         board = toupper(board)) %>%
  group_by(board, year, age_group, sex_name) %>%
  summarise(pop = sum(pop, na.rm =T))

#*4 - Calculate age-sex specific rates per month ----
add_rate <- addhbr %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, age_group, sex, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  filter(!health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  ungroup() %>%
  complete(date, urgency, age_group, sex, nesting(patient_type,health_board_of_residence, specialty), fill = list(additions_to_list = 0)) %>%
  mutate(year=year(date)) %>%
  left_join(pop, by = c("health_board_of_residence" = "board", "year", "sex"="sex_name", "age_group")) %>%
  mutate(asr = (100000*(additions_to_list/pop))) %>%
  select(-year) 

#*5 - Calculate crude rates per quarter ----
add_rate_qtr <- addhbr %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, age_group, sex, date) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  filter(!health_board_of_residence %in% c("NOT KNOWN", "ENGLAND/WALES/NORTHERN IRELAND", "OUTSIDE U.K.", "NO FIXED ABODE", NA)) %>%
  ungroup() %>%
  complete(date, urgency, age_group, sex, nesting(patient_type,health_board_of_residence, specialty), fill = list(additions_to_list = 0)) %>%
  group_by(date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1), urgency, age_group, sex, patient_type, health_board_of_residence, specialty) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  mutate(year=year(date)) %>%
  left_join(pop, by = c("health_board_of_residence" = "board", "year", "sex"="sex_name", "age_group")) %>%
  mutate(asr = (100000*(additions_to_list/pop))) %>%
  select(-year)

#*6 - Calculate standardised rates using ESP2013 ----

#Import ESP2013 data
stdpop <- read.spss(glue::glue(pop_path, "/Standard/ESP2013_by_sex.sav"), to.data.frame = TRUE) 

levels(stdpop$agegroup) <- str_remove_all(levels(stdpop$agegroup),"Persons aged ") #Remove "Persons aged " from factors

stdpop$agegroup <- fct_recode(stdpop$agegroup, "90+" = "90 or above") #Recode "90 or above" factor to match addhbr data 

#Bind ESP to addhbr and calculate age-specific rates

stdrate <- add_rate %>% 
  left_join(stdpop, by = c("age_group" = "agegroup", "sex")) %>%
  mutate(`asr*ESP2013` = (asr * ESP2013),
         CI = (asr * ESP2013 * ESP2013)*100000/pop) %>%
  ungroup() %>%
  group_by(health_board_of_residence, specialty, urgency, sex, date) %>%
  summarise(crude_rate = 100000 * (sum(additions_to_list, na.rm=T)/sum(pop)),
            var = sum(CI, na.rm = T)/(sum(ESP2013)^2),
            std_err = sqrt(var),
            EASR = sum(`asr*ESP2013`, na.rm = T)/sum(ESP2013, na.rm=T),
            low = EASR - (1.96*std_err),
            high = EASR + (1.96*std_err),
            .groups = "keep")

stdrate_qtr <- add_rate_qtr %>% 
  left_join(stdpop, by = c("age_group" = "agegroup", "sex")) %>%
  mutate(`asr*ESP2013` = (asr * ESP2013),
         CI = (asr * ESP2013 * ESP2013)*100000/pop) %>%
  group_by(health_board_of_residence, specialty, urgency, date) %>%
  summarise(pop = sum(pop),
            crude_rate = 100000 * (sum(additions_to_list, na.rm=T)/sum(pop)),
            var = sum(CI, na.rm = T)/(sum(ESP2013)^2),
            std_err = sqrt(var),
            EASR = sum(`asr*ESP2013`, na.rm = T)/sum(ESP2013, na.rm=T),
            low = EASR - (1.96*std_err),
            high = EASR + (1.96*std_err),
            .groups = "keep")

#Create combined male+female rates 

stdrate_all <- stdrate %>%
  group_by(health_board_of_residence, specialty, urgency, date) %>%
  summarise(person_EASR = mean(EASR, na.rm =T),
            person_var = sum(var)/4,
            std_err = sqrt(person_var),
            low = person_EASR - (1.96*std_err),
            high = person_EASR + (1.96*std_err),
            .groups = "keep")

stdrate_all_qtr <- stdrate_qtr %>%
  group_by(health_board_of_residence, specialty, urgency, date) %>%
  summarise(pop = unique(pop),
            person_EASR = mean(EASR, na.rm =T),
            person_var = sum(var)/4,
            std_err = sqrt(person_var),
            low = person_EASR - (1.96*std_err),
            high = person_EASR + (1.96*std_err),
            .groups = "keep")


#Graph for selected specialty ----

#Function to plot the EASR 
EASRplot <- function(df, specialty_of_interest, qtrdate, urgencylist) {
  df %>%
    filter(specialty == specialty_of_interest,
           date == qtrdate,
           urgency %in% urgencylist) %>%
    ggplot(aes(x = fct_rev(health_board_of_residence), y = person_EASR)) +
    geom_errorbar(aes(ymin=low, ymax=high, color = fct_rev(factor(urgency, levels = colourset$codes))),
                  width = 0.2,
                  stat="identity") + 
    theme_bw() + 
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    facet_wrap(~urgency) + 
    labs(x = NULL, y = "Person European Age Standardised Rate (per 100k population)") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          #panel.grid.minor.x = element_blank(), 
          #panel.grid.major.x = element_blank(),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8)) +
    coord_flip() +
    ggtitle(paste0("Standardised addition rates for ", specialty_of_interest, ", ", max_date))
}
  

rate_plot <- EASRplot(stdrate_all_qtr, "All Specialties", max_date, c("P2", "P3", "P4"))

#3.4.1.B - Age-sex-deprivation direct standardisation using Scotland Population ----

#Since SIMD stratification is only available for Scotland, we cannot calculate a EASR under age-sex-SIMD-standardisation. 
#Therefore we use the Scottish population (mid-year estimate) as the standard and follow the method above.


#3.4.1.C - Indirect age-sex-deprivation standardisation ----

#This method uses Scotland as the standard population and allows us to calculate what the additions would have been if the HB populations followed the same profile as Scotland as a whole. This is the same method CT used for the funnel plot analysis of high volume procedures for the SG in late 2017/early 2018.

#Steps in this process:
#1 - Get HB and Scotland level data by urgency, age group, sex, SIMD for the appropriate time period
#2 - Calculate Scotland crude rates = 1000 * additions per urgency / population (rate/1000 population)
#3 - Get age-sex-SIMD standardised rates for Scotland. ScotlandRate = as above but for each age/sex/SIMD group
#4 - Bind ScotlandRate and crude Scotland rate onto HB level data and calculate expected additions = ScotlandRate * HB population / 1000
#5 - Aggregate up to get HB level. additions = sum(additions), expected = sum(expected), population = sum(population)
#6 - Calculate crude HB rate = 1000 * additions/population and SOR = additions/expected additions
#7 - Calculate CI limits and standardised rate for each HB 
# 95% lower = 1000 * ((crude rate Scotland/1000) - (1.96*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 95% upper = 1000 * ((crude rate Scotland/1000) + (1.96*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 99.8% lower = 1000 * ((crude rate Scotland/1000) - (3.091*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 99.8% upper = 1000 * ((crude rate Scotland/1000) + (3.091*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# Standardised rate = SOR * crude rate Scotland
#8 (optional) - Calculate control limit status for each HB - if standardised rate between 99.8% limits, "Between limits", if > 99.8% upper, "Above control limits", if < 99.8% lower, "Below control limits"

#*1. Import data ----
#2020 population estimates by HB, sex, age group and SIMD
pop_simd <- read.xlsx("data/HB Population by SIMD 2020 estimate.xlsx") %>%
  select(-year) %>%
  bind_rows(read.xlsx("data/HB Population by SIMD 2020 estimate.xlsx") %>% 
              group_by(simd, sex, age_group) %>%
              summarise(pop = sum(pop, na.rm=T)) %>%
              mutate(board = "NHS Scotland")) %>%
  mutate(age_group = fct_relevel(age_group, "5-9", after = 1), 
         sex = as.factor(sex))

#Additions data by HB, sex, age group, SIMD and urgency
add_simd <- read.xlsx("data/Removal Reason excl. Lothian Dental by age gender SIMD.xlsx", sheet = "IPDC Additions HBR") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date), !specialty %in% exclusions,
         !age_group == "Blank", #Exclude records with "Blank" age
         !gender == "Blank",
         !is.na(simd)) %>% #Exclude records with no SIMD
  rename(sex = gender,
         board = health_board_of_residence) %>% #Rename to match population lookup
  mutate(across(c(age_group, sex), ~as.factor(.x)), #Convert age_group and sex to factors
         age_group = fct_relevel(age_group, "5-9", after = 1)) %>% #Put level "5-9" after "0-4"
  complete(date, urgency, age_group, sex, nesting(patient_type, board, specialty, simd), fill = list(additions_to_list = 0)) %>%
  ungroup() %>%
  group_by(date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1), urgency, age_group, sex, simd, board, specialty) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T), .groups = "keep")

add_simd_scot <- add_simd %>%
  group_by(date, urgency, age_group, sex, simd, specialty) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm=T)) %>%
  mutate(board = "NHS Scotland")
  
#*2 and 3 - Scotland crude and standardised rates----

add_simd_scot %<>% 
  left_join(pop_simd, by = c("board", "sex", "age_group", "simd")) %>% #Join on population data
  mutate(standardisedRate = 1000 * additions_to_list/pop) %>% #Calculate rates per group
  group_by(date, urgency, specialty) %>%
  mutate(crudeRate = 1000 * sum(additions_to_list)/sum(pop)) #Calculate crude rates (across all groups within each CP and specialty)

#*4 - Bind rates from Scotland onto add_simd----

add_simd %<>% 
  left_join(pop_simd, by = c("board", "sex", "age_group", "simd")) %>% #Join on population data
  left_join(select(add_simd_scot, -c(board, pop, additions_to_list)), by = c("date", "sex", "age_group", "simd", "specialty", "urgency")) %>%
  mutate(expected_additions = standardisedRate * pop / 1000)

#*5 - Calculate crude HB rate = 1000 * additions/population and SOR = additions/expected additions ----

add_simd %<>% 
  group_by(date, board, specialty, urgency) %>%
  summarise(pop = sum(pop),
            crudeRate = unique(crudeRate),
            additions_to_list = sum(additions_to_list),
            expected_additions = sum(expected_additions),
            crudeHBRate = 1000 * sum(additions_to_list)/sum(pop),
            SOR = sum(additions_to_list)/sum(expected_additions)) %>%
  mutate(ci95_l = 1000 * ((crudeRate/1000) - (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci95_u = 1000 * ((crudeRate/1000) + (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_l = 1000 * ((crudeRate/1000) - (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_u = 1000 * ((crudeRate/1000) + (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         standardisedHBRate = SOR * crudeRate,
         shortHB = case_when(str_detect(board, "ARRAN") ~"A&A", #Create short version names for labelling the points in the graphs
                             str_detect(board, "BORDERS") ~"BORD",
                             str_detect(board, "DUM") ~"D&G",
                             str_detect(board, "FIFE") ~"FIFE",
                             str_detect(board, "FORTH") ~"FV",
                             str_detect(board, "GLASGOW") ~"GG&C",
                             str_detect(board, "GRAMPIAN") ~"GRAM",
                             str_detect(board, "HIGHLAND") ~"HIGH",
                             str_detect(board, "LANARK") ~"LAN",
                             str_detect(board, "LOTHIAN") ~"LOTH",
                             str_detect(board, "ORKNEY") ~"ORKN",
                             str_detect(board, "SHETLAND") ~"SHET",
                             str_detect(board, "TAYSIDE") ~"TAY",
                             str_detect(board, "ISLES") ~"WI")
                             )

crudeRates <- add_simd %>%
  group_by(date, specialty, urgency) %>%
  summarise(crudeRate = unique(crudeRate))

pop_seq <- seq(10000, roundUpNice(max(plotdat$pop)), 70000)

lines <- expand_grid(crudeRates, pop_seq) %>%
  rename(pop = pop_seq) %>%
  mutate(board = "",
         shortHB = "",
         ci95_l = 1000 * ((crudeRate/1000) - (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci95_u = 1000 * ((crudeRate/1000) + (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_l = 1000 * ((crudeRate/1000) - (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_u = 1000 * ((crudeRate/1000) + (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         standardisedHBRate = ci95_l,
         standardisedHBRate2 = ci95_u,
         standardisedHBRate3 = ci998_l,
         standardisedHBRate4 = ci998_u,
         expected_additions = crudeRate*(pop/1000)
  ) %>%
  pivot_longer(cols = c(crudeRate, ci95_l:standardisedHBRate4), names_to = "indicator", values_to = "value")

add_simd_long <- add_simd %>%
  select(date, specialty, urgency, pop, board, shortHB, everything()) %>%
  pivot_longer(cols = c(crudeRate, additions_to_list, crudeHBRate:standardisedHBRate), names_to = "indicator", values_to = "value") %>%
  bind_rows(lines)
  

funnelplot <- function(df, specialty_of_interest, qtrdate, urgencylist) {
  
#Calculate upper and lower CIs over larger population range
  
  plotdat <- df %>%
    filter(specialty == specialty_of_interest,
           date == qtrdate,
           urgency %in% urgencylist)
  
p <- ggplot(data = plotdat[plotdat$indicator == "standardisedHBRate"], aes(x = population/1000, y = value, label = shortHB), group = urgency,colour = indicator) +
    geom_point(stat="identity") + 
  geom_point(data = plotdat[plotdat$indicator == "ci998_l"], aes(y = value)) +#, colour = phs_colours("phs-purple")) +
  #geom_(aes(y = ci998_u), colour = phs_colours("phs-purple")) +
  #geom_line(aes(y = ci95_l), linetype = "dashed", colour = phs_colours("phs-blue")) +
  #geom_line(aes(y = ci95_u), linetype = "dashed", colour = phs_colours("phs-blue")) +
  #geom_line(aes(y = crudeRate)) +
   # geom_line(aes(x = pop_seq, y = crudeRate)) +
    geom_text_repel(hjust=0, vjust=0, box.padding = 0.3) +
    geom_blank(aes(y = 0)) +
    facet_wrap(~urgency, scales = "free", ncol = if_else(length(urgencylist) >3, 2, 3)) +
    theme_bw() +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    labs(x = "Population/1,000", y = "Standardised Rate (per 1,000 population)") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8)) +
    ggtitle(paste0("Age, sex and SIMD standardised addition rates for ", specialty_of_interest, ", ", max_date))
  
  p
}

funnelplot <- function(df, specialty_of_interest, qtrdate, urgencylist) {
df %>%
    filter(specialty == specialty_of_interest,
           date == qtrdate,
           urgency %in% urgencylist) %>%
    ggplot(aes(x = population/1000, y = standardisedHBRate, label = shortHB), group = urgency) +
    geom_point(stat="identity") +
    geom_line(aes(y = ci998_l), colour = phs_colours("phs-purple")) +
    geom_line(aes(y = ci998_u), colour = phs_colours("phs-purple")) +
    geom_line(aes(y = ci95_l), linetype = "dashed", colour = phs_colours("phs-blue")) +
    geom_line(aes(y = ci95_u), linetype = "dashed", colour = phs_colours("phs-blue")) +
    geom_line(aes(y = crudeRate)) +
    geom_text_repel(hjust=0, vjust=0, box.padding = 0.5, max.overlaps = 20) +
    geom_blank(aes(y = 0)) +
    facet_wrap(~urgency, scales = "free", ncol = if_else(length(urgencylist) >3, 2, 3)) +
    theme_bw() +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    labs(x = "Population/1,000", y = "Standardised Rate (per 1,000 population)") +
    theme(text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
    panel.spacing = unit(0.5, "cm"),
    panel.border = element_blank(),
    legend.position="bottom",
    legend.key.height= unit(0.25, 'cm'),
    legend.key.width= unit(0.25, 'cm'),
    legend.text = element_text(size = 8)) +
    ggtitle(paste0("Age, sex and SIMD standardised addition rates for ", specialty_of_interest, ", ", max_date))
}


#*6 - Create graphs for specialties of interest ----
fplot_all <- funnelplot(add_simd, "All Specialties", max_date, c("P2", "P3", "P4"))
  
fplot_cardio <- funnelplot(add_simd, "Cardiology", max_date, c("P2", "P3", "P4"))  

fplot_urol <- funnelplot(add_simd, "Urology", max_date, c("P2", "P3", "P4"))  
  
fplot_gynae <- funnelplot(add_simd, "Gynaecology", max_date, c("P2", "P3", "P4"))  

fplot_gs <- funnelplot(add_simd, "General Surgery", max_date, c("P2", "P3", "P4"))  

fplot_opto <- funnelplot(add_simd, "Ophthalmology", max_date, c("P2", "P3", "P4"))  

fplot_ortho <- funnelplot(add_simd, "Orthopaedics", max_date, c("P2", "P3", "P4")) 

fplot_ent <- funnelplot(add_simd, "Ear, Nose & Throat", max_date, c("P2", "P3", "P4"))  


#3.4.1.D - Indirect age-sex standardisation ----

#This method uses Scotland as the standard population and allows us to calculate what the additions would have been if the HB populations followed the same profile as Scotland as a whole. This is the same method CT used for the funnel plot analysis of high volume procedures for the SG in late 2017/early 2018.

#Steps in this process:
#1 - Get HB and Scotland level data by urgency, age group, sex for the appropriate time period
#2 - Calculate Scotland crude rates = 1000 * additions per urgency / population (rate/1000 population)
#3 - Get age-sex standardised rates for Scotland. ScotlandRate = as above but for each age/sex group
#4 - Bind ScotlandRate and crude Scotland rate onto HB level data and calculate expected additions = ScotlandRate * HB population / 1000
#5 - Aggregate up to get HB level. additions = sum(additions), expected = sum(expected), population = sum(population)
#6 - Calculate crude HB rate = 1000 * additions/population and SOR = additions/expected additions
#7 - Calculate CI limits and standardised rate for each HB 
# 95% lower = 1000 * ((crude rate Scotland/1000) - (1.96*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 95% upper = 1000 * ((crude rate Scotland/1000) + (1.96*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 99.8% lower = 1000 * ((crude rate Scotland/1000) - (3.091*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# 99.8% upper = 1000 * ((crude rate Scotland/1000) + (3.091*sqrt((1/HB population)*((crude rate Scotland/1000)*(1-(crude rate Scotland/1000))))))
# Standardised rate = SOR * crude rate Scotland
#8 (optional) - Calculate control limit status for each HB - if standardised rate between 99.8% limits, "Between limits", if > 99.8% upper, "Above control limits", if < 99.8% lower, "Below control limits"

#*1. Import data ----
#2021, 2022 population projections by HB, sex, age group 
pop <- readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
  mutate(board = paste0("NHS ",str_replace(hb2019name, " and ", " & "))) %>% #Reformat names to match other data 
  bind_rows(readRDS(glue::glue(pop_path, "/Projections/HB2019_pop_proj_2018_2043.rds")) %>% 
              group_by(year, age, sex_name) %>%
              summarise(pop = sum(pop, na.rm=T)) %>%
              mutate(board = "NHS Scotland")) %>%
  filter(year >= "2021") %>%
  mutate(age_group = as.factor(cut(age, agebreaks, agelabels, include.lowest = TRUE, right= FALSE)), 
         sex_name = as.factor(sex_name),
         board = if_else(!board =="NHS Scotland", toupper(board), board)) %>%
  group_by(board, year, age_group, sex_name) %>%
  summarise(pop = sum(pop, na.rm =T))%>%
  rename(sex = sex_name) 

#Additions data by HB, sex, age group, SIMD and urgency
add_hb_agesex <- read.xlsx("data/Removal Reason excl. Lothian Dental by age gender SIMD.xlsx", sheet = "IPDC Additions HBR") %>%
  clean_names(use_make_names = FALSE) %>% #make column names sensible but allow `90th percentile` to start with a number rather than "x"
  mutate(date =openxlsx::convertToDate(date), #Convert dates from Excel format
         specialty = if_else(specialty == "Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)) %>% #Rename T&O as orthopaedics
  filter(between(date, min_date, max_date), !specialty %in% exclusions,
         !age_group == "Blank", #Exclude records with "Blank" age
         !gender == "Blank", #Exclude records with no sex
         !health_board_of_residence %in% c("ENGLAND/WALES/NORTHERN IRELAND", "NOT KNOWN", "NO FIXED ABODE", "OUTSIDE U.K.", NA)) %>% #Exclude non-Scottish residents
  rename(sex = gender,
         board = health_board_of_residence) %>% #Rename to match population lookup
  mutate(across(c(age_group, sex), ~as.factor(.x)), #Convert age_group and sex to factors
         age_group = fct_relevel(age_group, "5-9", after = 1)) %>% #Put level "5-9" after "0-4"
  group_by(patient_type, date, board, specialty, urgency, age_group, sex) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  ungroup() %>%
  complete(date, urgency, age_group, sex, nesting(patient_type, board, specialty), fill = list(additions_to_list = 0)) %>%
  group_by(date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1), urgency, age_group, sex, board, specialty) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T), 
            .groups = "keep") %>%
  mutate(year = year(date))

add_scot_agesex <- add_hb_agesex %>%
  group_by(date, urgency, age_group, sex, specialty) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm=T)) %>%
  mutate(board = "NHS Scotland",
         year = year(date))

#*2 and 3 - Scotland crude and standardised rates----

add_scot_agesex %<>% 
  left_join(pop, by = c("board", "sex", "age_group", "year")) %>% #Join on population data
  mutate(standardisedRate = 1000 * additions_to_list/pop) %>% #Calculate rates per group
  group_by(date, urgency, specialty) %>%
  mutate(crudeRate = 1000 * sum(additions_to_list)/sum(pop)) #Calculate crude rates (across all groups within each CP and specialty)

#*4 - Bind rates from Scotland onto add_simd----

add_hb_agesex %<>% 
     #  ungroup() %>%
  left_join(pop) %>% #Join on population data
  left_join(select(add_scot_agesex, -c(board, pop, additions_to_list)), by = c("date", "sex", "age_group","specialty", "urgency", "year")) %>%
  mutate(expected_additions = standardisedRate * pop / 1000)

#*5 - Calculate crude HB rate = 1000 * additions/population and SOR = additions/expected additions ----

add_hb_agesex %<>% 
  group_by(date, board, specialty, urgency) %>%
  summarise(pop = sum(pop),
            crudeRate = unique(crudeRate),
            additions_to_list = sum(additions_to_list),
            expected_additions = sum(expected_additions),
            crudeHBRate = 1000 * sum(additions_to_list)/sum(pop),
            SOR = sum(additions_to_list)/sum(expected_additions)) %>%
  mutate(ci95_l = 1000 * ((crudeRate/1000) - (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci95_u = 1000 * ((crudeRate/1000) + (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_l = 1000 * ((crudeRate/1000) - (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_u = 1000 * ((crudeRate/1000) + (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         standardisedHBRate = SOR * crudeRate,
         shortHB = case_when(str_detect(board, "ARRAN") ~"A&A", #Create short version names for labelling the points in the graphs
                             str_detect(board, "BORDERS") ~"BORD",
                             str_detect(board, "DUM") ~"D&G",
                             str_detect(board, "FIFE") ~"FIFE",
                             str_detect(board, "FORTH") ~"FV",
                             str_detect(board, "GLASGOW") ~"GG&C",
                             str_detect(board, "GRAMPIAN") ~"GRAM",
                             str_detect(board, "HIGHLAND") ~"HIGH",
                             str_detect(board, "LANARK") ~"LAN",
                             str_detect(board, "LOTHIAN") ~"LOTH",
                             str_detect(board, "ORKNEY") ~"ORKN",
                             str_detect(board, "SHETLAND") ~"SHET",
                             str_detect(board, "TAYSIDE") ~"TAY",
                             str_detect(board, "ISLES") ~"WI"),
         status = case_when(standardisedHBRate <ci998_u & standardisedHBRate > ci998_l ~"Between control limits",
                            standardisedHBRate >= ci998_u ~"Above control limits",
                            standardisedHBRate <= ci998_l ~"Below control limits")
  ) 

crudeRates_agesex <- add_hb_agesex %>%
  group_by(date, specialty, urgency) %>%
  summarise(crudeRate = unique(crudeRate))

pop_seq_agesex <- seq(10000, roundUpNice(max(add_hb_agesex$pop)), 70000)

lines_agesex <- expand_grid(crudeRates_agesex, pop_seq_agesex) %>%
  rename(pop = pop_seq_agesex) %>%
  mutate(board = "",
         shortHB = "",
         ci95_l = 1000 * ((crudeRate/1000) - (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci95_u = 1000 * ((crudeRate/1000) + (1.96*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_l = 1000 * ((crudeRate/1000) - (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         ci998_u = 1000 * ((crudeRate/1000) + (3.091*sqrt((1/pop)*((crudeRate/1000)*(1-(crudeRate/1000)))))),
         standardisedHBRate = ci95_l,
         standardisedHBRate2 = ci95_u,
         standardisedHBRate3 = ci998_l,
         standardisedHBRate4 = ci998_u,
         expected_additions = crudeRate*(pop/1000)
  ) %>%
  pivot_longer(cols = c(crudeRate, ci95_l:standardisedHBRate4), names_to = "indicator", values_to = "value")

funnelplot_agesex <- function(df, specialty_of_interest, qtrdate, urgencylist) {
  df %>%
    filter(specialty == specialty_of_interest,
           date == qtrdate,
           urgency %in% urgencylist) %>%
    ggplot(aes(x = pop/1000, y = standardisedHBRate, label = shortHB), group = urgency) +
    geom_point(stat="identity") +
    geom_line(aes(y = ci998_l), colour = phs_colours("phs-purple")) +
    geom_line(aes(y = ci998_u), colour = phs_colours("phs-purple")) +
    geom_line(aes(y = ci95_l), linetype = "dashed", colour = phs_colours("phs-blue")) +
    geom_line(aes(y = ci95_u), linetype = "dashed", colour = phs_colours("phs-blue")) +
    geom_line(aes(y = crudeRate)) +
    geom_text_repel(hjust=0, vjust=0, point.size =5, box.padding = 0.5, max.overlaps = 5, nudge_x = .15) +#,
#                    nudge_y = .25*min(df$ci998_u, na.rm=T)/max(df$crudeRate, na.rm=T)) +
    geom_blank(aes(y = 0)) +
    facet_wrap(~urgency, scales = "free", ncol = if_else(length(urgencylist) >3, 2, 3)) +
    theme_bw() +
    scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
    scale_x_continuous(labels=function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE)) +
    labs(x = "Population/1,000", y = "Standardised Rate (per 1,000 population)") +
    theme(text = element_text(size = 12),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
          panel.spacing = unit(0.5, "cm"),
          panel.border = element_blank(),
          legend.position="bottom",
          legend.key.height= unit(0.25, 'cm'),
          legend.key.width= unit(0.25, 'cm'),
          legend.text = element_text(size = 8)) +
    ggtitle(paste0("Age, sex standardised addition rates for ", specialty_of_interest, ", ", max_date))
}


#*6 - Create graphs for specialties of interest ----
fplot_all_as <- funnelplot_agesex(add_hb_agesex, "All Specialties", max_date, c("P2", "P3", "P4"))

fplot_cardio_as <- funnelplot_agesex(add_hb_agesex, "Cardiology", max_date, c("P2", "P3", "P4"))  

fplot_urol_as <- funnelplot_agesex(add_hb_agesex, "Urology", max_date, c("P2", "P3", "P4"))  

fplot_gynae_as <- funnelplot_agesex(add_hb_agesex, "Gynaecology", max_date, c("P2", "P3", "P4"))  

fplot_gs_as <- funnelplot_agesex(add_hb_agesex, "General Surgery", max_date, c("P2", "P3", "P4"))  

fplot_opto_as <- funnelplot_agesex(add_hb_agesex, "Ophthalmology", max_date, c("P2", "P3", "P4"))  

fplot_ortho_as <- funnelplot_agesex(add_hb_agesex, "Orthopaedics", max_date, c("P2", "P3", "P4")) 

fplot_ent_as <- funnelplot_agesex(add_hb_agesex, "Ear, Nose & Throat", max_date, c("P2", "P3", "P4"))


#3.4.1.E - Comparison of age-sex and age-sex-SIMD standardisation outcomes ----
#Do the classifications (above/below/between) change significantly for any Boards 

#Bind add_simd and add_hb_agesex, then count what categrories each HB, spec, urgency is in

comparison <- add_simd %>% 
  filter(date == max_date) %>% 
  group_by(board, specialty, urgency) %>% 
  mutate(status = case_when(standardisedHBRate <ci998_u & standardisedHBRate > ci998_l ~"Between control limits",
                            standardisedHBRate >= ci998_u ~"Above control limits",
                            standardisedHBRate <= ci998_l ~"Below control limits"),
         type = "age-sex-simd") %>%
  bind_rows(mutate(filter(add_hb_agesex, date == max_date), type = "age-sex")) %>%
  group_by(board, specialty, urgency, type) %>%
  summarise(above = sum(str_detect(status, "Above")), below = sum(str_detect(status, "Below")), between = sum(str_detect(status, "Between")))%>%
  pivot_wider(id_cols = c(board, specialty, urgency), names_from = type, values_from = c(above, below, between))


#3.4.2 - Additions by CP/HBR ----

#Calculate proportions by CP category for each HBR, specialty, quarter
add_prop <- addhbr %>%
  group_by(patient_type, health_board_of_residence, specialty, urgency, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  group_by(patient_type, health_board_of_residence, specialty, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  mutate(total_additions = sum(additions_to_list, na.rm = T),
         p2_additions = sum(additions_to_list[urgency=="P2"], na.rm = T),
         p1_p3_additions = sum(additions_to_list[urgency %in% c("P1A-1B","P2", "P3")], na.rm = T)) %>%
  ungroup() %>%
  mutate(proportion = additions_to_list/total_additions,
         p2_proportion = p2_additions/total_additions,
         p1_p3_proportion = p1_p3_additions/total_additions) %>%
  mutate(type = "Residence") %>%
  rename(board = health_board_of_residence)

#Graph for all specialties ----
prop_plot <- add_prop %>%
  filter(!board %in% c("ENGLAND/WALES/NORTHERN IRELAND", "NO FIXED ABODE", "NOT KNOWN", "OUTSIDE U.K.", NA),
         type == "Residence",
         specialty == "All Specialties",
         date == max_date) %>%
  ggplot(aes(x = fct_reorder(board,p2_proportion, .desc=FALSE), y = `proportion`),group=urgency) +
  #group=health_board_of_residence) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), 
               fill=fct_rev(factor(urgency, levels = colourset$codes))),
           stat="identity", width=0.75) +
  theme_bw() +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size = 10)) +
  coord_flip()


#Save this image
ggsave("hb_additions_plot.png", plot = prop_plot, dpi=300, dev='png', height=14, width=17, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#Graph for top six specialties ----

topsix_hbr <- add_prop %>%
  filter(!board %in% c("ENGLAND/WALES/NORTHERN IRELAND", "NO FIXED ABODE", "NOT KNOWN", "OUTSIDE U.K.", NA),
         specialty %in% topsix$specialty,
         type == "Residence",
         date == max_date) %>%
  mutate(specialty = fct_reorder(specialty,p2_proportion, .desc=TRUE)) %>%
  #ungroup() %>%
 # group_by(specialty) %>%
  # 2. Arrange by
  #   i.  facet group
  #   ii. bar height
#  arrange(board) %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  ggplot(aes(x = fct_rev(board), y = `proportion`, order = p2_proportion),
         group=urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), 
               fill=fct_rev(factor(urgency, levels = colourset$codes))),
           stat="identity", width=0.75) +
  theme_bw() +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~specialty, ncol = 2) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.ticks.x = element_line(colour = "#000000" ),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.text = element_text(size = 8)) +
  coord_flip()


ggsave("top_six_additions_plot.png", topsix_hbr, dpi=300, dev='png', height=20, width=18, units="cm", path = here::here("..","R plots", "Plots for draft report"))

#Graph for opthalmology ----
opt_prop_plot <- add_prop %>%
  filter(!board %in% c("ENGLAND/WALES/NORTHERN IRELAND", "NO FIXED ABODE", "NOT KNOWN", "OUTSIDE U.K.", NA),
         specialty == "Ophthalmology",
         type == "Residence",
         date == max_date,
         total_additions >=1000) %>%
  ggplot(aes(x = fct_reorder(board,p2_proportion, .desc=FALSE), y = `proportion`),
         group=urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), 
               fill=fct_rev(factor(urgency, levels = colourset$codes))),
           stat="identity", width=0.75) +
  theme_bw() +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size = 10)) +
  coord_flip()

ggsave("ophthalmology_additions_plot.png", dpi=300, dev='png', height=14, width=17, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#3.4.2.1 - Additions by HBT ----
add_prop_hbt <- addhbr %>%
  filter(!nhs_board_of_treatment == "NHS Scotland") %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, urgency, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  summarise(additions_to_list = sum(additions_to_list, na.rm = T)) %>%
  group_by(patient_type, nhs_board_of_treatment, specialty, date=as.Date(as.yearqtr(date, format = "Q%q/%y"), frac = 1)) %>%
  mutate(total_additions = sum(additions_to_list, na.rm = T),
         p2_additions = sum(additions_to_list[urgency=="P2"], na.rm = T),
         p1_p3_additions = sum(additions_to_list[urgency %in% c("P1A-1B","P2", "P3")], na.rm = T)) %>%
  ungroup() %>%
  mutate(proportion = additions_to_list/total_additions,
         p2_proportion = p2_additions/total_additions,
         p1_p3_proportion = p1_p3_additions/total_additions) %>% 
  mutate(type = "Treatment",
         nhs_board_of_treatment = toupper(nhs_board_of_treatment)) %>%
  rename(board = nhs_board_of_treatment)


prop_plot_hbt <- add_prop_hbt %>%
  filter(!board=="NHS Scotland",
         type == "Treatment",
         specialty == "All Specialties",
         date == max_date) %>%
  ggplot(aes(x = fct_reorder(board,p2_proportion, .desc=FALSE), y = `proportion`),
         group=urgency) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), 
               fill=fct_rev(factor(urgency, levels = colourset$codes))),
           stat="identity", width=0.75) +
  theme_bw() +
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size = 10)) +
  coord_flip()

ggsave("hbt_additions_plot.png", dpi=300, dev='png', height=14, width=17, units="cm", path = here::here("..","R plots", "Plots for draft report"))


#3.4.2.2 - Combined HBR and HBT figures ---- 
hbr_hbt_prop <- bind_rows(add_prop, add_prop_hbt) %>%
  filter(!board %in% c("ENGLAND/WALES/NORTHERN IRELAND", "NOT KNOWN", "NO FIXED ABODE", "OUTSIDE U.K.", "SCOTLAND", NA)) %>%
  group_by(patient_type, board, specialty, urgency, date) %>%
  summarise(type = unique(type),
            proportion = unique(proportion),
            p2_prop = sum(p2_proportion[type == "Residence"]))

#Graph of proportions facetted by residence/treatment ----
hbr_hbt_plot <- hbr_hbt_prop %>% 
  filter(date ==max_date,
         specialty == "All Specialties") %>%
  ggplot(aes(x = fct_reorder(board, p2_prop, .desc =FALSE),y = proportion), group=type) +
  geom_bar(aes(color = fct_rev(factor(urgency, levels = colourset$codes)), fill=fct_rev(factor(urgency, levels = colourset$codes))),stat="identity", width=0.75) +
  theme_bw() + 
  scale_colour_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "")+
  scale_fill_manual(values=phs_colours(colourset$colours), breaks = colourset$codes, name = "") +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(.~type)+#, 
  #             labeller = as_labeller(c(Completed = "Patients admitted", Ongoing = "Patients waiting"))) +
  labs(x = NULL, y = NULL) +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.x = element_text(angle = 0,hjust = 0,size = 12),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.spacing = unit(0.5, "cm"),
        panel.border = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.text = element_text(size = 8)) +
  coord_flip()


#3.4.3 - Cross Border Flow ----

#cross-border flow table
cbf_test <- addhbr %>% 
  filter(!nhs_board_of_treatment=="NHS Scotland", #Exclude Boards
         !health_board_of_residence %in% c(NA, "OUTSIDE U.K.", "NOT KNOWN", "NO FIXED ABODE", "ENGLAND/WALES/NORTHERN IRELAND"),
         date <= max_date) %>%
  mutate(source = health_board_of_residence, target = nhs_board_of_treatment) %>%
  group_by(specialty) %>% 
  summarise(total_additions = sum(additions_to_list, na.rm = T),
            cbf_additions = sum(additions_to_list[!source == target & !source ==toupper(target)], na.rm=T)) %>%
  mutate(cbf_proportion = cbf_additions/total_additions) %>%
  filter(!specialty == "All Specialties",
         total_additions >=500) %>%
  arrange(desc(cbf_proportion))


cbf_plot_data <- addhbr %>% 
  filter(!nhs_board_of_treatment=="NHS Scotland", #Exclude certain Boards
         !health_board_of_residence %in% c(NA, "OUTSIDE U.K.", "NOT KNOWN", "NO FIXED ABODE", "ENGLAND/WALES/NORTHERN IRELAND"),
         date <= max_date) %>%
  group_by(source = health_board_of_residence, target = nhs_board_of_treatment, specialty) %>%
  summarise(value = sum(additions_to_list[!source == target & !source ==toupper(target)], na.rm = T))

#Create links data for sankeyNetwork function
links <- data.frame(source = cbf_plot_data$source,
                    target = cbf_plot_data$target,
                    value  = cbf_plot_data$value)

#Create lists of source and target Boards for nodes data frame
id <- unique(c(as.character(cbf_plot_data$source), as.character(cbf_plot_data$target)))
label <- toupper(id)

# create nodes data frame from unique nodes found in links data frame
nodes <- data.frame(id = id, label = label) %>% 
  mutate(node_group = gsub(" ", "_", label),
         x = if_else(id %in% unique(cbf_plot_data$source), 0.3, 0.7), 
         pad = 10) #node_group

sankey_data <- links %>%
  mutate(IDsource = match(links$source, nodes$id) -1,
         IDtarget = match(links$target, nodes$id) - 1,
         link_group = gsub(" ", "_", links$source))

list(
  type = "sankey",
  # arrangement = "snap",
  # domain = c(
  #    x =  c(0,1),
  #    y =  c(0,1)
  #  ),
  node = list(
    label = nodes$label,
    x = nodes$x,
    pad = 20),
  link = list(
    source = as.numeric(sankey_data$IDsource), 
    target = as.numeric(sankey_data$IDtarget), 
    value = sankey_data$value
  )
) -> trace1

plot_ly(
  domain=trace1$domain, link=trace1$link,
  node=trace1$node, type=trace1$type
)
