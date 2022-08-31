########################################################################
# Name of file - CP-data-prep.R
# Data release - Stage of Treatment
# Original Authors - Caroline Thomson
# Orginal Date - May 2022
#
# Written/run on - RStudio Server
# Version of R - 3.6.1
#
# Description - Analysis of CP data for developmental stats
#
# Approximate run time - xx minutes
#########################################################################

#### 1 - Packages and functions ----
#1.1 - Load libraries ----
source("packages/packages.R")

Sys.umask("002") # Used to ensure directory permissions are correct

### 2 - Define Whether Running on Server or Locally ----
#2.1 - Server or desktop?
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)",
                                  "x86_64-pc-linux-gnu (64-bit)")) {
  platform <- "server"
} else {
  platform <- "locally"
}


#2.2 Live or snapshot?
#This generates a message in the console - user chooses 1 (live) or 2 (snapshot) and file paths are updated automatically
universe <- switch(
  menu(c("live", "snapshot")), 
  "live", "snapshot", 
  title = "Choose a universe:")


# Define root directory for stats server based on whether script is running 
# locally or on server
filepath <- dplyr::if_else(platform == "server",
                           "/PHI_conf/WaitingTimes/SoT/Projects/CP MMI",
                           "//stats/WaitingTimes/SoT/Projects/CP MMI")

### 3 - Import data ----
# 3.1 - Load functions 
source(glue::glue(filepath, "/R Code/CP-functions.R"))

#3.2 - File paths ----
#Procedure level data to create proclevedata.RDS (if need-be)
proc_path <- glue::glue(filepath, "/BOXI Extracts/", {universe}, " data/CO_DoW_proc.zip")

#Procedure level RDS location (we'll look here to see if the file exists and is recent) 
proc_data <- glue::glue(filepath, "/BOXI Extracts/", {universe}, " data/procleveldata.RDS")

#Performance (copy latest version for publication from appropriate folder)
perfpath <- glue::glue(filepath, "/BOXI Extracts/", {universe}, " data/Performance excl. Lothian Dental.xlsx")

#Additions and removals (copy latest version for publication from appropriate folder) 
addpath <- glue::glue(filepath, "/BOXI Extracts/", {universe}, " data/Removal Reason excl. Lothian Dental.xlsx")

#Procedure codes
codes_path <- "/PHI_conf/WaitingTimes/Portfolio/IRs-PQs-FOIs/Completed 2021/IR2021-00649 (Re-run of IR2021-00503).zip" #OPCS4 codes and groups

codeslookup <- read.csv(unz(codes_path, "IR2021-00649 (Re-run of IR2021-00503)/proc_lookup.csv"), stringsAsFactors = FALSE)


#Procedure data with all codes included 
#This code chunk first checks if the procleveldata.RDS file has been created in the current quarter (from within "R procedures code.R" script). If it has, the file is simply loaded as usual; if not, the data is created and saved as procleveldata.RDS. The same chunk could be added to "Procedure completeness.R" in the Procedure completeness routine IR script.

if(file.exists(proc_data) & #Check 1 - Does the file exist?
  lubridate::quarter(file.info(proc_data)$ctime) == lubridate::quarter(Sys.Date())) { #Check 2 - Has the file been created in the current quarter?
  allcodes_data <- readRDS(proc_data)
    
} else{
  allcodes_data <- read.csv(unz(proc_path, "IPDC.csv"), stringsAsFactors = F) %>%
    clean_names() %>%
    mutate(number_seen_on_list = as.numeric(number_seen_on_list), #Added as numbers came through as character in csv
           month = if_else(str_detect(month, "/"), as.Date(month, "%d/%m/%Y"), as.Date(as.numeric(month), origin = "1899-12-30")),
           specialty = ifelse(specialty=="Trauma And Orthopaedic Surgery", "Orthopaedics", specialty),
           urgency = case_when(
             urgency %in% c("Not Known", "Not Known", "Routine", "Urgent") ~ "Other",
             urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ paste0("P",str_extract(urgency, "[\\d]")), #Create short form names (P2, P3 etc)
             urgency %in% c("Priority 1A", "Priority 1B") ~"P1A-1B",
             TRUE ~urgency
           )
    ) %>%
    rename(board = nhs_board_of_treatment) %>%
    group_by(patient_type, ongoing_completed, board, specialty, month, urgency, x4d_proc, procedure_name, x3d_proc, wait_length) %>%
    summarise(number_seen_on_list = sum(number_seen_on_list)) %>%
    left_join(codeslookup, by = c("x4d_proc"="main_proc")) %>% #First join on 4D code as this is most specific
    left_join(codeslookup, by=c("x3d_proc"="main_proc"))  %>%  #Then join on 3D code
    mutate(description = ifelse(is.na(`description.x`), `description.y`, `description.x`), #If 3D-matched vales are NA, use 4D ones in case a more specific match was made
           proc_grp = ifelse(is.na(`proc_grp.x`), `proc_grp.y`, `proc_grp.x`),
           proc_subgrp = ifelse(is.na(`proc_subgrp.x`), `proc_subgrp.y`, `proc_subgrp.x`),
           across(.cols = description:proc_subgrp, #Fill all 3 new columns with "Blank" if 3D and 4D codes are NA
                  .fns = ~if_else(is.na(`x3d_proc`) & is.na(`x4d_proc`), "Blank", .),
                  .names = "{.col}")) %>%
    ungroup() %>%
    select(-contains(c(".","x"))) %>%
    select(c(patient_type:specialty, proc_grp, proc_subgrp, description, month:wait_length, number_seen_on_list))
  
  #Write out procedure data
   saveRDS(allcodes_data, glue::glue(filepath, "/BOXI Extracts/", {universe}, " data/procleveldata.RDS"))  
}

# 3.3 - Patients seen/waiting data ----
patdata <- allcodes_data %>%
  select(-`patient_type`) %>%
  mutate(wl = ifelse(nchar(wait_length) == 7, as.numeric(substr(wait_length,5,7)), as.numeric(substr(wait_length,1,3)))) %>%
  group_by(ongoing_completed, board, specialty, month, urgency, wl) %>%
  summarise(patients = sum(number_seen_on_list, na.rm=T),
            `>4wk` = sum(patients[wl>4], na.rm=T), #Number waiting over 4 weeks
            `>12wk` = sum(patients[wl>12], na.rm=T), #Number waiting over 12 weeks
            `>52wk` = sum(patients[wl>52], na.rm=T), #etc
            `>78wk` = sum(patients[wl>78], na.rm=T)
  ) %>%
  group_by(ongoing_completed, board, specialty, month, urgency) %>%
  summarise(patients = sum(patients, na.rm=T),
            `>4wk` = sum(`>4wk`, na.rm=T),
            `>12wk` = sum(`>12wk`, na.rm=T),
            `>52wk` = sum(`>52wk`, na.rm=T),
            `>78wk` = sum(`>78wk`, na.rm=T)) %>%
  group_by(ongoing_completed, board, specialty, month) %>%
  mutate(spec_patients = sum(patients, na.rm=T), #Calculate total patients per specialty
         percentage = round(100*patients/spec_patients,2)) #Calculate % split per urgency category


#3.4 - Completeness data for DQ tab ----
DQstats <- patdata %>%
  mutate(completeness = sum(percentage[urgency != "Other"])) %>% 
  select(ongoing_completed, board, specialty, month, urgency, completeness)

write.csv(DQstats, file = glue::glue(filepath, "/R output/DQstats.csv"), row.names = FALSE)

#3.5 - Calculate date for 3 months before end of CP data ----
qend = ceiling_date(floor_date(max(patdata$month), "months") - months(3), "months") - days(1)


# 3.5 - Specialty-level publication data ----
perfdata <- read.xlsx(perfpath, sheet="IPDC") %>% 
  clean_names() %>%
  rename(board=nhs_board_of_treatment) %>%
  select(c(ongoing_completed:number_seen_on_list)) %>%
  mutate(date = if_else(str_detect(date,"/"),
                        as.Date(date, format = "%d/%m/%Y"),
                        ceiling_date(as.Date(paste("01", date, sep=" "), format = "%d %m %Y"), "months")- days(1)),
         specialty = ifelse(specialty=="Trauma And Orthopaedic Surgery", "Orthopaedics", specialty) #Rename T&O as orthopaedics
  ) %>%
  group_by(ongoing_completed, board, specialty) %>% 
  summarise(lastq = number_seen_on_list[date==qend],
            precovidavg = round(sum(number_seen_on_list[between(date, as.Date("2019-01-01"), as.Date("2019-12-31"))]/12),0))

#### 4 - Patients seen -----
patdata %<>% left_join(perfdata) #Join publication data onto activity data

#4.1 - Save all data for Excel ----
write.csv(patdata, file = glue::glue(filepath, "/R output/waitsHBspecCP.csv"), row.names = FALSE) #This goes into the ongoing_completed data tab


#### 5 - Addition and removals, rates/100k population ----
#Note that the rates calculations are currently based on HBT rather than HBR - script and BOXI queries need modified to use HBR

#5.1 - Import population data ----
pop_path <- dplyr::if_else(platform == "server",
                           "/conf/linkage/output/lookups/Unicode/Populations",
                           "//stats/cl-out/lookups/Unicode/Populations")

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

#Create combined population df
pop <- bind_rows(popest, popproj)

# 5.2 - Additions and removals calculations ----
addrem <- read.xlsx(addpath, sheet="IPDC") %>% 
  clean_names() %>%
  rename(board=nhs_board_of_treatment) %>%
  mutate(date = ceiling_date(as.Date(paste("01", date, sep=" "), format = "%d %m %Y"), "months")- days(1),#Reformat date to be last day of month
         urgency = case_when(
           urgency %in% c("Not Known", "Routine", "Urgent") ~ "Other",
           urgency %in% c("Priority 2A", "Priority 3A", "Priority 4A") ~ paste0("P",str_extract(urgency, "[\\d]")),
           urgency %in% c("Priority 1A", "Priority 1B") ~"P1A-1B",
           TRUE ~urgency
         ),
         specialty = ifelse(specialty=="Trauma And Orthopaedic Surgery", "Orthopaedics", specialty)
  ) %>%
  group_by(patient_type, board, specialty, date, urgency) %>%
  summarise(across(additions_to_list:other_reasons, ~(sum(.x, na.rm=T))))


addrem %<>% 
  mutate(year=year(date)) %>%
  left_join(pop, by = c("board","year")) %>%
  mutate(additions_per_100k = (100000*additions_to_list/pop)) %>%
  select(-year)

#5.3 - Calculate total additions/removals per HB, specialty and date ----
tot_addrem <- addrem %>%
  group_by(patient_type, board, specialty,date) %>%
  summarise(across(additions_to_list:other_reasons, ~(sum(.x, na.rm=T))),
            pop=min(pop)) %>%
  mutate(additions_per_100k = 100000*additions_to_list/pop,
         urgency = "Total") %>%
  group_by(patient_type, board, specialty, date, urgency) %>%
  pivot_longer(additions_to_list:additions_per_100k, names_to = "indicator", values_to = "value") %>% #Pivot data longer to make summarising easier
  filter(indicator!="pop") %>%
  group_by(patient_type, board, specialty, indicator) %>% 
  mutate(lastq = round(sum(value[date==qend], na.rm=T), 
                       ifelse(indicator=="additions_per_100k", 1,0)),
         precovidavg = round(sum(value[between(date, as.Date("2019-01-01"), as.Date("2019-12-31"))]/12, na.rm=T), 
                             ifelse(indicator=="additions_per_100k", 1,0)))

#Apply the same calculations to data at urgency category level
addrem %<>%
  group_by(patient_type, board, specialty, date, urgency) %>%
  pivot_longer(additions_to_list:additions_per_100k, names_to = "indicator", values_to = "value") %>%
  filter(indicator!="pop") %>%
  group_by(patient_type, board, specialty, urgency, indicator) %>%
  mutate(lastq = round(sum(value[date==qend], na.rm=T), #Calculate last quarter values at indicator level
                       ifelse(indicator=="additions_per_100k", 1,0))) %>%
  group_by(patient_type, board, specialty, indicator) %>% 
  mutate(precovidavg = NA #Don't calculate pre-covid averages for new CP codes
  )

#Bind total onto original data
addrem %<>% 
  bind_rows(tot_addrem) %>%
  filter(date >= as.Date("2021-06-30"))  #Only take forward recent dates

write.csv(addrem, file = glue::glue(filepath, "/R output/additions_removals.csv"), row.names = FALSE, na="")


#### 6 - Distribution of wait ----
# 6.1 - Weekly time bands ----
dowdata <- readRDS(proc_data) %>%
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency, wait_length) %>%
  summarise(patients = sum(number_seen_on_list, na.rm=T))

# 6.2 - 4 weekly time bands ----
#Define the boundaries for the time bands
waitbreaks <- c(-1,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,900)

#Define labels for the time bands
waitlabels <- c("000-004","004-008","008-012","012-016","016-020","020-024","024-028","028-032","032-036","036-040",
                "040-044","044-048","048-052","052-056","056-060","060-064","064-068","068-072","072-076","076-080",
                "080-084","084-088","088-092","092-096","096-100","100-104","104+")

#Regroup dow data with new bands
dowdata_4wk <- dowdata %>%
  mutate(wl = if_else(nchar(wait_length)==7, #Need to make wait lengths numeric to cut them into 4 week bands
                      as.numeric(str_remove_all(substr(wait_length,5,7),"^0")),
                      105),
         wb = cut(wl, breaks = waitbreaks, labels = waitlabels) # Create 4-week bands
  ) %>% 
  group_by(patient_type, ongoing_completed, board, specialty, month, urgency, wb) %>%
  summarise(patients = sum(patients, na.rm=T)) %>% #Recalculate patients in the new groups
  rename(wait_length = wb)

# 6.3 - Export data ----
write.csv(dowdata, file = glue::glue(filepath, "/R output/dow.csv"), row.names = FALSE)
write.csv(dowdata_4wk, file = glue::glue(filepath, "/R output/dow4wk.csv"), row.names = FALSE)

#END OF SCRIPT ----