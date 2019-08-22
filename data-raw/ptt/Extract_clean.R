# Import Libraries --------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)

# Download DR Data from BC Data Catalouge ------------------------------------
DR_monthly_2018 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/3a33d90e-be3d-4ceb-8f7f-dc6bcd6866f4/resource/8bdf79ce-fdfd-44a0-ab6c-faf642ded24b/download/development_region_monthly_2018.csv")
DR_monthly_2017 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/c2cc7b0a-b464-4fa2-8ea7-cd1ff07fb799/resource/00d2d1df-d0dd-4a34-86aa-5e3789d7b2da/download/developmentregionmonthly2017.csv")
DR_monthly_2016 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/8e38e8ee-a30b-4d7d-a2b4-9463a2b509d5/download/development_region_monthly_2016.csv")


# Ensure column names are consistent for Development Region files---------------

# Data cleaning for DR_2018
DR_monthly_2018_modified <- DR_monthly_2018 %>% 
  rename("year"="Year","month"="Month","no_res_trans"="n_res_trans","n_comm_strata_nores"="n_comm_strata_nores_rental") %>% 
  mutate(RegionalDistrict=NA,Municipality=NA,n_res_strata=n_res_strata_row + n_res_strata_other, n_foreign_other=NA,sum_FMV_foreign=NA,sum_invest_foreign_res=NA)

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
  select(-resid_comm_count,-no_resid_farm,-no_resid_non_strata, - mn_FMV_foreign, - md_FMV_foreign)


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
  select(-trans_period,-no_resid_farm,-no_resid_non_strata,-resid_comm_count,-no_bare_trust,- mn_FMV_foreign, - md_FMV_foreign)
         

# Binding 2016, 2017, 2018 data together ----------------------------------


DR_all_years <- rbind(DR_monthly_2016_modified,DR_monthly_2017_modified,DR_monthly_2018_modified) %>% 
  select(year, month, DevelopmentRegion, RegionalDistrict, Municipality,everything())





# Download Regional Data from BC Data Catalouge  --------------------------

Regional_monthly_2018 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/3a33d90e-be3d-4ceb-8f7f-dc6bcd6866f4/resource/7d83b79e-9ab1-476e-b5d9-e437c15b6ff7/download/regional_district_monthly_2018.csv")
Regional_monthly_2017 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/c2cc7b0a-b464-4fa2-8ea7-cd1ff07fb799/resource/60cf5065-710e-43da-b3ae-5bf38b78405e/download/regionaldistrictmonthly2017.csv")
Regional_monthly_2016 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/1ae2299f-8ce5-4af1-9dab-3d4e8586a77a/download/regional_district_monthly_2016.csv")



# Ensure column names are consistent for Regional files -------------------

# Data cleaning for Regional_2018
Regional_monthly_2018_modified <- Regional_monthly_2018 %>% 
  rename("year"="Year","month"="Month","no_res_trans"="n_res_trans","n_comm_strata_nores"="n_comm_strata_nores_rental") %>% 
  mutate(Municipality=NA,n_res_strata=n_res_strata_row + n_res_strata_other, n_foreign_other=NA) %>% 
  select(-perc_FMV_foreign_res,-perc_invest_foreign_res,-perc_n_foreign_res)


# Data cleaning for Regional_2017
Regional_monthly_2017_modified <- Regional_monthly_2017 %>% 
  rename("year"="Year","month"="Month", "no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign","n_foreign_res"= "no_foreign_res", "n_foreign_nonres"="no_foreign_nonres") %>% 
  mutate(Municipality=NA,n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_other=NA) %>% 
  select(-resid_comm_count,-no_resid_farm,-no_resid_non_strata, -mn_FMV_foreign, -md_FMV_foreign, -perc_no_foreign_res, -perc_FMV_foreign_res,-perc_invest_foreign_res)

# Data cleaning for Regional_2016

### Data manipulation for trans_period as the format is different for Jan through May
Regional_monthly_2016_Jan_throu_May <- Regional_monthly_2016 %>% 
  filter(trans_period %in% c("Jan-16","Feb-16","Mar-16","Apr-16","May-16" ))


Regional_monthly_2016_Jun_throu_Dec <- Regional_monthly_2016 %>% 
  filter(trans_period %in% c( "16-Jun","16-Jul","16-Aug","16-Sep","16-Oct","16-Nov","16-Dec"))
  
  
  
Regional_monthly_2016_Jan_throu_May$trans_period <- zoo::as.yearmon(Regional_monthly_2016_Jan_throu_May$trans_period, format="%b-%y")
Regional_monthly_2016_Jun_throu_Dec$trans_period <- zoo::as.yearmon(Regional_monthly_2016_Jun_throu_Dec$trans_period, format="%y-%b")

Regional_monthly_2016 <- rbind(Regional_monthly_2016_Jan_throu_May,Regional_monthly_2016_Jun_throu_Dec)

Regional_monthly_2016_modified <- Regional_monthly_2016 %>% 
  rename("no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign") %>% 
  mutate(Municipality=NA, year=year(trans_period),month=month(trans_period), n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_nonres=NA,n_foreign_other=NA,n_foreign_res=NA) %>% 
  select(-trans_period,-no_resid_farm,-no_resid_non_strata,-resid_comm_count,-no_bare_trust, -mn_FMV_foreign, -md_FMV_foreign)



Regional_all_years <- rbind(Regional_monthly_2016_modified,Regional_monthly_2017_modified,Regional_monthly_2018_modified) %>% 
  select(year, month, DevelopmentRegion, RegionalDistrict, Municipality,everything())
# Download Municipal Data from BC Data Catalouge  --------------------------

Municipal_monthly_2018 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/3a33d90e-be3d-4ceb-8f7f-dc6bcd6866f4/resource/d3b17041-af0b-4d68-83c1-41bc6e119b78/download/municipal_monthly_2018.csv")
Municipal_monthly_2017 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/c2cc7b0a-b464-4fa2-8ea7-cd1ff07fb799/resource/1fc4ffad-b50a-4b41-bcbf-51d3d0f6beb8/download/municipalitymonthly2017.csv")
Municipal_monthly_2016 <- read_csv("https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/6abca1b9-2eab-4d0d-8350-d27d69af3258/download/municipality_monthly_2016.csv")


# Data cleaning for Municipal_2018
Municipal_monthly_2018_modified <- Municipal_monthly_2018 %>% 
  rename("year"="Year","month"="Month","no_res_trans"="n_res_trans","n_comm_strata_nores"="n_comm_strata_nores_rental", "Municipality"="Jurisdiction") %>% 
  mutate(n_res_strata=n_res_strata_row + n_res_strata_other, n_foreign_other=NA,sum_FMV_foreign=NA,sum_invest_foreign_res=NA) 


# Data cleaning for Municipal_2017


Municipal_monthly_2017_modified <- Municipal_monthly_2017 %>% 
  rename("year"="Year","month"="Month", "no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign") %>% 
  mutate(n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_nonres=NA,n_foreign_other=NA,n_foreign_res=NA,n_foreign_nonres=NA) %>% 
  select(-resid_comm_count,-no_resid_farm,-no_resid_non_strata, -mn_FMV_foreign, -md_FMV_foreign)




# Data cleaning for Municipal_2016

### READ NEW DATA FOR 2016
# path in which all the new files exist
data_path <- "I:/02_Finance & Admin/17-272 Property Transfer Tax Data Analysis/Data/R/2016_data_full_munis/3_Final outputs/DBC"

# Grab all the files that end with .csv and include JUR
files <- grep("JUR", dir(data_path, pattern = "*.csv"), value=TRUE)

# read all the above files and combine them
Municipal_monthly_2016<- files %>% 
  map_df(~ read.csv(file.path(data_path, .)))


Municipal_monthly_2016_modified <- Municipal_monthly_2016 %>% 
  rename("year"="Year","month"="Month","Municipality"="Jurisdiction","no_res_trans"="no_resid_trans","n_res_acreage_trans"="no_resid_acreage_trans",
         "n_res_fam"="no_resid_fam","n_res_1fam_dwelling"="no_res_1fam",
         "n_res_strata"="no_resid_strata","n_res_other"="no_resid_other","n_comm_tran"="no_comm_tot",
         "n_comm_comm"="no_comm_comm","n_comm_strata_nores"="no_comm_strata_nores","n_comm_other"="no_comm_other",
         "n_recr_tran"="no_recr_tot","n_farm_tran"="no_farm_tot","n_unkn_tran"="no_unkn_tot",
         "n_foreign_tran"="no_foreign") %>% 
  mutate( n_res_1fam_suite=NA,n_res_1fam_vacant_res=NA,n_res_1fam_other=NA,n_res_strata_row=NA, n_res_strata_other=NA,
         n_fth=NA,n_nbh=NA,sum_FMV_res=NA,sum_FMV_foreign_res=NA,md_FMV_foreign_res=NA,
         mn_FMV_foreign_res=NA, sum_invest_foreign_res=NA,n_foreign_nonres=NA,n_foreign_other=NA,n_foreign_res=NA,n_foreign_nonres=NA) %>% 
select(-resid_comm_count,-no_resid_farm,-no_resid_non_strata, -mn_FMV_foreign, -md_FMV_foreign, -no_bare_trust)
         


Municipal_all_years <- rbind(Municipal_monthly_2016_modified,Municipal_monthly_2017_modified,Municipal_monthly_2018_modified) %>% 
  select(year, month, DevelopmentRegion, RegionalDistrict, Municipality,everything())

### Make DevelopmentRegoin fields all capital
DR_all_years$DevelopmentRegion <- toupper(DR_all_years$DevelopmentRegion)
Regional_all_years$DevelopmentRegion <- toupper(Regional_all_years$DevelopmentRegion)
Municipal_all_years$DevelopmentRegion <- toupper(Municipal_all_years$DevelopmentRegion)

### consistent regional names
Regional_all_years$RegionalDistrict <- gsub("COLUMBIA SHUSWAP","COLUMBIA-SHUSWAP",Regional_all_years$RegionalDistrict)
Municipal_all_years$RegionalDistrict <- gsub("COLUMBIA SHUSWAP","COLUMBIA-SHUSWAP",Municipal_all_years$RegionalDistrict)


Regional_all_years$RegionalDistrict <- gsub("KOOTENAY BOUNDARY","KOOTENAY-BOUNDARY",Regional_all_years$RegionalDistrict)
Municipal_all_years$RegionalDistrict <- gsub("KOOTENAY BOUNDARY","KOOTENAY-BOUNDARY",Municipal_all_years$RegionalDistrict)


Regional_all_years$RegionalDistrict <- gsub("NORTHERN ROCKIES-PEACE RIVER","NORTHERN ROCKIES RM-PEACE RIVER",Regional_all_years$RegionalDistrict)
Municipal_all_years$RegionalDistrict <- gsub("NORTHERN ROCKIES-PEACE RIVER","NORTHERN ROCKIES RM-PEACE RIVER",Municipal_all_years$RegionalDistrict)


### consistent municipal names
Municipal_all_years$Municipality <- gsub("CITY OF FORT ST. JOHN","CITY OF FORT ST JOHN",Municipal_all_years$Municipality)
Municipal_all_years$Municipality <- gsub("FORT ST JOHN RURAL","FORT ST. JOHN RURAL",Municipal_all_years$Municipality)
### Writing the csv files to the output folder

write_csv(DR_all_years,"output_all_years/DR_all_years.csv")
write_csv(Regional_all_years,"output_all_years/Regional_all_years.csv")
write_csv(Municipal_all_years,"output_all_years/Municipal_all_years.csv")



development_DR <- data.frame(development_DR=unique(DR_all_years$DevelopmentRegion))
Regional_DR <- data.frame(unique(Regional_all_years$DevelopmentRegion))
Municipal_DR <- data.frame(unique(Municipal_all_years$DevelopmentRegion))


development_RD <- data.frame(unique(DR_all_years$RegionalDistrict))
Regional_RD <- data.frame(unique(Regional_all_years$RegionalDistrict))
Municipal_RD <- data.frame(unique(Municipal_all_years$RegionalDistrict))


development_Municipality <- data.frame(unique(DR_all_years$Municipality))
Regional_Municipality <- data.frame(unique(Regional_all_years$Municipality))
Municipal_Municipality <- data.frame(unique(Municipal_all_years$Municipality))
  


write_csv(development_DR,"output/development_DR.csv")   
write_csv(Regional_DR,"output/Regional_DR.csv") 
write_csv(Municipal_DR,"output/Municipal_DR.csv") 



write_csv(development_RD,"output/development_RD.csv") 
write_csv(Regional_RD,"output/Regional_RD.csv") 
write_csv(Municipal_RD,"output/Municipal_RD.csv") 




write_csv(development_Municipality,"output/development_Municipality.csv") 
write_csv(Regional_Municipality,"output/Regional_Municipality.csv") 
write_csv(Municipal_Municipality,"output/Municipal_Municipality.csv") 





