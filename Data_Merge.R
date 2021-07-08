rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library(purrr)
library(lubridate)
library(tidyselect)
library(qpcR)
library(stringr)
library(reachR)
library(zoo)
library(googlesheets)
library(openxlsx)
library(data.table)

#for the server
#setwd("Z:/")
#for the dropbox
# setwd("C:/Users/hazre/Desktop/JMMI_Interactive-master/Datasets")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./Datasets")

#All data saved on the z drive
#MAKE SURE YOU ARE PULLING THE RIGHT SHEET (DO NOT PULL THE RAW DATA SHEET)
#Add a line to mutate a variable called jmmi with "Month_Year" inside, to streamline date (and simplify data consolidation)

April_2018 <- read_excel("2.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April2018_recode_cols.xlsx",sheet = 2) %>% mutate(jmmi="April_2018")
May_2018 <- read_excel("3.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May2018_recode_cols.xlsx",sheet = 2) %>% mutate(jmmi="May_2018")
June_2018 <- read_excel("4.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_June2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="June_2018")
July_2018 <- read_excel("5.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_July2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="July_2018")
August_2018 <-read_excel("6.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="August_2018")
September_2018 <-read_excel("7.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_September2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="September_2018")
October_2018 <-read_excel("8.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_October2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="October_2018")
November_2018 <-read_excel("9.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_November2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="November_2018")
December_2018 <-read_excel("10.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_December2018_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="December_2018")
January_2019 <-read_excel("11.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_January2019_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="January_2019")
February_2019<-read_excel("12.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_February2019_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="February_2019")
March_2019<-read_excel("13.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_March2019_recode_cols .xlsx", sheet=2) %>% mutate(jmmi="March_2019")
April_2019<-read_excel("14.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April2019_recode_cols.xlsx", sheet=2) %>% mutate(jmmi="April_2019")
May_2019<-read_excel("15. REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May2019.xlsx", sheet=2) %>% mutate(jmmi="May_2019")
June_2019<-read_excel("16.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_June2019.xlsx", sheet = 2) %>% mutate(jmmi="June_2019")
July_2019<-read_excel("17.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_July2019.xlsx", sheet = 2) %>% mutate(jmmi="July_2019")
August_2019<-read_excel("18.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August2019.xlsx", sheet = 2) %>% mutate(jmmi="August_2019")
September_2019<-read_excel("19.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_September2019.xlsx", sheet = 2) %>% mutate(jmmi="September_2019")
October_2019<-read_excel("20.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_October2019.xlsx", sheet = 2) %>% mutate(jmmi="October_2019")
November_2019<-read_excel("21.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_November2019.xlsx", sheet = 3) %>% mutate(jmmi="November_2019")
December_2019<-read_excel("22.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_December2019.xlsx", sheet = 3) %>% mutate(jmmi="December_2019")
January_2020<-read_excel("23.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_January2020.xlsx", sheet = 2) %>% mutate(jmmi="January_2020")
February_2020<-read_excel("24.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_Febraury2020.xlsx", sheet = 2) %>% mutate(jmmi="February_2020")
March_2020 <-read_excel("25.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_March2020.xlsx", sheet = 2) %>% mutate(jmmi="March_2020")
April_2020 <-read_excel("26.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April2020.xlsx", sheet = 3) %>% mutate(jmmi="April_2020")
May_2020 <-read_excel("27.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May2020.xlsx", sheet = 3) %>% mutate(jmmi="May_2020")
June_2020 <-read_excel("28.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_June2020.xlsx", sheet = 3) %>% mutate(jmmi="June_2020")
July_2020 <-read_excel("29.REACH_YEM_Dataset_Joint-Market-Monitoring-Initiative-JMMI_July2020.xlsx", sheet = 3) %>% mutate(jmmi="July_2020")
August_2020 <-read_excel("30b.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_August2020.xlsx", sheet = 5) %>% mutate(jmmi="August_2020")
September_2020 <-read_excel("31.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_September2020.xlsx", sheet = 3) %>% mutate(jmmi="September_2020")
October_2020 <-read_excel("32.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_October2020.xlsx", sheet = 3) %>% mutate(jmmi="October_2020")
November_2020 <-read_excel("33.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_November2020.xlsx", sheet = 3) %>% mutate(jmmi="November_2020")
December_2020 <-read_excel("34.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_December2020.xlsx", sheet = 3) %>% mutate(jmmi="December_2020")
January_2021 <-read_excel("35.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_January2021.xlsx", sheet = 3) %>% mutate(jmmi="January_2021")
February_2021 <-read_excel("36.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_February 2021.xlsx", sheet = 3) %>% mutate(jmmi="February_2021")
March_2021 <-read_excel("37.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_March 2021.xlsx", sheet = 3) %>% mutate(jmmi="March_2021")
April_2021 <- read_excel("38.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_April 2021.xlsx", sheet = 3) %>% mutate(jmmi="April_2021")
May_2021 <- read_excel("39.REACH_YEM_Dataset_Joint Market Monitoring Initiative (JMMI)_May 2021.xlsx", sheet = 3) %>% mutate(jmmi="May_2021")

list_df = setNames(lapply(ls(), function(x) get(x)), ls())
list_df_names <- names(list_df)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("other scripts/utils.R")                                                 # harmonisation functions + adding pcodes

list_df <- lapply(list_df, harmonise.df)
df <- do.call("rbind.fill", list_df)                                            # Trying to rbind.fill all of dataset to keep market functionality questions

col_price <- colnames(df)[grepl("^calc_*|cost_cubic_meter|exchange_rate_result\\b|fuel_gov_origin|wash_gov_origin|food_gov_origin", colnames(df))]
col_mkt_functionnality <- colnames(df)[grepl("mrk|market|sell_|cash_feasibility|COVID", colnames(df))]
metacol <- colnames(df)[grepl("jmmi|jmmi_date|^country_|^governorate_*|^district_*", colnames(df))]

data_all_JMMI <- df %>% dplyr::select(-district_name, -governorate_id, -governorate_name) %>% # Rename dataframe to fit rest of the code - Get rid of all other meta columns to only keep streamlined pcodes 
  mutate_at(vars(matches("calc|exchange|cost")), as.numeric)                          # change type as numeric for relevant columns

#substitute out the pcodes to standardize the name (taken from JMMI scripting, with csv (utf-8) sheet)
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

source("./other scripts/add_pcodes.R")
#debug(add.pcodes)
data_all_JMMI<-add.pcodes(data_all_JMMI) %>% dplyr::select(aor, matches("country_"), matches("governorate_"), matches("district_"), everything())

#change Pcodes for origin governorates
source("./other scripts/gov_code_switch.R")
data_all_JMMI$fuel_gov_origin<-gov_code_switch(data_all_JMMI$fuel_gov_origin)
data_all_JMMI$wash_gov_origin<-gov_code_switch(data_all_JMMI$wash_gov_origin)
data_all_JMMI$food_gov_origin<-gov_code_switch(data_all_JMMI$food_gov_origin)
#pull in the full modes script
source("./other scripts/full_modes.R")

################then begin the ananlysis of the files######################

#get rid of all districts that have less than three observation   => should we consider 2? as coverage very limited
data_all_JMMI <- data_all_JMMI %>%
                        dplyr::group_by(jmmi, district_id)%>%
                        filter(n()>2)%>%
                        as_tibble()

#make the JMMI column act as a date column and begin to sort by that, will be important for when you want to do that national by the previous month
# data_all_JMMI$jmmi<-gsub("_"," ", data_all_JMMI$jmmi)
#this is the actual date that will be sorted by with in the server script
# data_all_JMMI$jmmi_date_old <- as.character(as.Date(as.yearmon(as.character(data_all_JMMI$jmmi))))
date_list<-sort(unique(data_all_JMMI$jmmi_date))
metacol <- c("aor", metacol)

# Selects all market functionality indicators
df_mkt_functionnality <- data_all_JMMI %>% dplyr::select(all_of(metacol), all_of(col_mkt_functionnality))

# Selects all prices + market functionnality
data_all <- data_all_JMMI %>% dplyr::select(all_of(metacol), all_of(col_price), all_of(col_mkt_functionnality))

# Only keeps prices [for aggregation below]
data_all_JMMI <- data_all_JMMI %>% dplyr::select(all_of(metacol), all_of(col_price))

# Aggregation loops - will aggregate by median and calculate # of observations for all prices by district and governorate 

# Loop 1 - Compute medians at district/gov level using all available observations, except national data which subsets districts with consistent coverage
# District, Governorate and national level medians are computed using base data. [No median of median methodology]

for(i in seq_along(date_list)){
  if (i ==1){
    df1 <- data_all_JMMI %>% filter(jmmi_date==date_list[i])
    
    district_all <- df1 %>% aggregate_median("district_id")
    district_obs <- df1 %>% dplyr::select("district_id","jmmi","jmmi_date") %>%
      dplyr::count(district_id, jmmi)
    
    governorate_all <- df1 %>% dplyr::select(-any_of(c("aor"))) %>%               # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("governorate_id") 
    governorate_obs <- df1 %>% dplyr::select("governorate_id","jmmi","jmmi_date")%>%
      dplyr::count(governorate_id, jmmi)
    
    national_all <- df1 %>% dplyr::select(-any_of(c("aor"))) %>%                  # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("country_id") 
    national_obs <- df1 %>% dplyr::select("country_id","jmmi","jmmi_date")%>%
      dplyr::count(country_id, jmmi)
    
  }else{
    
    df1 <- data_all_JMMI %>% filter(jmmi_date==date_list[i])
    df0 <- data_all_JMMI %>% filter(jmmi_date==date_list[i-1])
    
    district_all <- df1 %>% 
      aggregate_median("district_id") %>% bind_rows(district_all)
    district_obs <- df1 %>% dplyr::select("district_id","jmmi","jmmi_date") %>%
      dplyr::count(district_id,jmmi) %>% bind_rows(district_obs)
    
    governorate_all <- df1 %>% dplyr::select(-any_of(c("aor"))) %>%             # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("governorate_id") %>% bind_rows(governorate_all)
    governorate_obs <- df1 %>% dplyr::select("governorate_id","jmmi","jmmi_date")%>%
      dplyr::count(governorate_id,jmmi) %>% bind_rows(governorate_obs)
    
    df0_pull <- unique(df0$district_id)
    df_dist <- subset(df1, district_id %in% df0_pull)
    
    national_all <- df_dist %>% dplyr::select(-any_of(c("aor"))) %>%            # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("country_id") %>% bind_rows(national_all)
    national_obs <- df_dist %>% dplyr::select("country_id","jmmi","jmmi_date")%>%
      dplyr::count(country_id, jmmi) %>% bind_rows(national_obs)
    
    print(date_list[i])
  }
}

# Loop 2 - Compute medians at district/gov/national data using districts with consistent coverage
# Here district medians are computed with base data, while national and governorate medians are computed from the district medians

for(i in seq_along(date_list)){
  if (i ==1){
    df1 <- data_all_JMMI %>% filter(jmmi_date==date_list[i])
    
    district_all_pct_change <- df1 %>%
      aggregate_median("district_id")
    district_obs_pct_change <- df1 %>% dplyr::select("district_id","jmmi","jmmi_date") %>%
      dplyr::count(district_id, jmmi)
  
    governorate_all_pct_change <- district_all_pct_change %>% dplyr::select(-any_of(c("aor"))) %>%  # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("governorate_id")
    governorate_obs_pct_change <- df1 %>% dplyr::select("governorate_id","jmmi","jmmi_date") %>%
      dplyr::count(governorate_id, jmmi)
    
    national_all_pct_change <- district_all_pct_change %>%                      # here takes median of medians [if dispersion across district medians + low number of covered districts, estimates will be very coverage dependent]
      aggregate_median("country_id")
    national_obs_pct_change <- df1 %>% dplyr::select("country_id","jmmi","jmmi_date")%>%
      dplyr::count(country_id, jmmi)
    
  }else{
    df1 <- data_all_JMMI %>% filter(jmmi_date==date_list[i])
    df0 <- data_all_JMMI %>% filter(jmmi_date==date_list[i-1])
    
    df0_pull<-unique(df0$district_id)
    df_dist<-subset(df1, district_id %in% df0_pull)                             # subsets only districts that were covered previous month 
    
    district_all_alone_pct_change <- df_dist %>%
      aggregate_median("district_id")
    district_all_pct_change <- bind_rows(district_all_alone_pct_change,district_all_pct_change) # Add the medians of this months to all past months, based on consistent coverage
    district_obs_pct_change <- df1 %>% dplyr::select("district_id","jmmi","jmmi_date") %>%
      dplyr::count(district_id,jmmi) %>% bind_rows(district_obs_pct_change)
    
    governorate_all_alone_pct_change <- district_all_alone_pct_change %>% dplyr::select(-any_of(c("aor"))) %>% # unselect aor which will be aggregated with mode, leading to misleading data.
      aggregate_median("governorate_id")
    governorate_all_pct_change <- bind_rows(governorate_all_alone_pct_change,governorate_all_pct_change)
    governorate_obs_pct_change <- df1 %>% dplyr::select("governorate_id","jmmi","jmmi_date") %>%
      dplyr::count(governorate_id,jmmi) %>% bind_rows(governorate_obs_pct_change)
    
    national_all_pct_change <- district_all_alone_pct_change %>%
      aggregate_median("country_id") %>% bind_rows(national_all_pct_change)
    national_obs_pct_change <- df1 %>% dplyr::select("country_id","jmmi","jmmi_date") %>%
      dplyr::count(country_id, jmmi) %>% bind_rows(national_obs_pct_change)
    
    print(date_list[i])
  }
}

#join the observations and the values

district_final <- dplyr::full_join(district_all,district_obs, by = c("district_id", "jmmi"))
governorate_final <- dplyr::full_join(governorate_all,governorate_obs, by = c("governorate_id", "jmmi")) %>% dplyr::select(-starts_with("district"))
national_final <- dplyr::full_join(national_all,national_obs, by = c("country_id", "jmmi")) %>% dplyr::select(-starts_with("district"), -starts_with("governorate"))

district_final_pct_change <- dplyr::full_join(district_all_pct_change,district_obs_pct_change, by = c("district_id", "jmmi"))
governorate_final_pct_change <- dplyr::full_join(governorate_all_alone_pct_change,governorate_obs_pct_change, by = c("governorate_id", "jmmi")) %>% dplyr::select(-starts_with("district"))
national_final_pct_change <- dplyr::full_join(national_all_pct_change,national_obs_pct_change, by = c("country_id", "jmmi")) %>% dplyr::select(-starts_with("district"), -starts_with("governorate"))

# Rename and reorder all variables to fit with google sheets templates
meta_dis_raw <- c("jmmi_date","aor","governorate_name","governorate_id","district_name","district_id")
meta_dis_final <- c("date","aor","government_name","government_ID","district_name","district_ID")
col_price_raw <- c("calc_price_bleach","calc_price_wheat_flour","calc_price_rice","calc_price_beans_dry","calc_price_beans_can","calc_price_lentil","calc_price_vegetable_oil","calc_price_sugar","calc_price_salt","calc_price_potato","calc_price_cooking_gas","calc_price_onion","calc_price_petrol","calc_price_diesel","calc_price_bottled_water","calc_price_treated_water","calc_price_soap","calc_price_laundry","calc_price_sanitary","cost_cubic_meter","exchange_rate_result","n")
col_price_final <- c("bleach","wheat_flour","rice","beans_dry","beans_can","lentil","vegetable_oil","sugar","salt","potato","cooking_gas","onion","petrol","diesel","bottled_water","treated_water","soap","laundry_powder","sanitary_napkins","cost_cubic_meter","exchange_rates","num_obs")

rename.var <- function(df){
  df <- df %>% 
    setNames(gsub("calc_price_", "", colnames(.))) %>%
    setnames(old = c(meta_dis_raw, "laundry", "sanitary", "exchange_rate_result", "n"),
             new = c(meta_dis_final, "laundry_powder","sanitary_napkins", "exchange_rates", "num_obs"), skip_absent = T) 
}

list_admin <- list(district_final, governorate_final, national_final, district_final_pct_change, governorate_final_pct_change, national_final_pct_change)
names(list_admin) <- c("district_final", "governorate_final", "national_final", "district_final_pct_change", "governorate_final_pct_change", "national_final_pct_change")
for (df_name in names(list_admin)){
  assign(df_name, 
         list_admin[[df_name]] %>% rename.var %>% dplyr::select(any_of(meta_dis_final), all_of(col_price_final)))
  }

data_all <- data_all %>% rename.var %>%                                    # Only rename relevant columns for data all [keep all other columns]
  dplyr::select(jmmi, any_of(meta_dis_final), any_of(col_price_final), everything(), -starts_with("country_"))

final_list <- list("District"=district_final,"Governorate" = governorate_final, "National" = national_final)
final_list_pct_change <- list("District"=district_final_pct_change,"Governorate" = governorate_final_pct_change, "National" = national_final_pct_change)

this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

write.xlsx(final_list, file = "./data/updated_interactive.xlsx")
write.xlsx(final_list_pct_change, file = "./data/updated_interactive_pct_change.xlsx")

# undo after all clear
write.csv(district_final, file = "./data/district_interactive.csv")
write.csv(governorate_final, file = "./data/governorate_interactive.csv")
write.csv(national_final, file = "./data/national_interactive.csv")

write.csv(district_final_pct_change, file = "./data/district_interactive_pct_change.csv")
write.csv(governorate_final_pct_change, file = "./data/governorate_interactive_pct_change.csv")
write.csv(national_final_pct_change, file = "./data/national_interactive_pct_change.csv")

write.csv(data_all, file = "./data/data_all.csv")
write.csv(df_mkt_functionnality, "data/data_market_functionnality.csv")
