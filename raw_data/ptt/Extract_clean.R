# Import Libraries --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)

# Download Data from BC Data Catalouge ------------------------------------
DR_monthly_2018 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/3a33d90e-be3d-4ceb-8f7f-dc6bcd6866f4/resource/8bdf79ce-fdfd-44a0-ab6c-faf642ded24b/download/development_region_monthly_2018.csv")
DR_monthly_2017 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/c2cc7b0a-b464-4fa2-8ea7-cd1ff07fb799/resource/00d2d1df-d0dd-4a34-86aa-5e3789d7b2da/download/developmentregionmonthly2017.csv")
DR_monthly_2016 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/8e38e8ee-a30b-4d7d-a2b4-9463a2b509d5/download/development_region_monthly_2016.csv")


# Ensure column names are consistent for Development Region files---------------

# Data cleaning for DR_2018
DR_monthly_2018_modified <- DR_monthly_2018 %>% 
  rename("year"="Year","month"="Month","no_res_trans"="n_res_trans","n_comm_strata_nores"="n_comm_strata_nores_rental") %>% 
  mutate(n_res_strata=NA,RegionalDistrict=NA,Municipality=NA, n_foreign_other=NA,sum_FMV_foreign=NA,mn_FMV_foreign=NA,md_FMV_foreign=NA,sum_invest_foreign_res=NA)

# Data cleaning for DR_2017
DR_monthly_2017_modified <- DR_monthly_2017 %>% 
  rename("year"="Year","month"="Month", "no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign") %>% 
  mutate(RegionalDistrict=NA,Municipality=NA,n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_res=NA,n_foreign_nonres=NA,n_foreign_other=NA ) %>% 
  select(-resid_comm_count,-no_resid_farm,-no_resid_non_strata)

# Data cleaning for DR_2016

DR_monthly_2016$trans_period <- zoo::as.yearmon(DR_monthly_2016$trans_period, format="%b-%y") 

DR_monthly_2016_modified <- DR_monthly_2016 %>% 
  rename("no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign") %>% 
  mutate(year=year(trans_period),
         month=month(trans_period), RegionalDistrict=NA,Municipality=NA,n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_res=NA,n_foreign_nonres=NA,n_foreign_other=NA) %>% 
  select(-trans_period,-no_resid_farm,-no_resid_non_strata,-resid_comm_count,-no_bare_trust)
         

# Binding 2016, 2017, 2018 data together ----------------------------------


DR_all_years <- rbind(DR_monthly_2016_modified,DR_monthly_2017_modified,DR_monthly_2018_modified) %>% 
  select(year, month, DevelopmentRegion, everything())

write_csv(DR_all_years,"output/DR_2016_2018.csv")

